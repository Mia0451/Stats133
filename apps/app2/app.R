# ===============================================
# Fill in the following fields
# ===============================================
# Title: Harry Potter Text Analysis
# Description: App that performs two kind of analysis
# 1) a change of sentiment analysis.
# 2) a bigram analysis
# Details: 
# 1) for change of sentiment analysis, user are only allowed to select
#   one book with different lines for one section.
# 2) for bigram analysis, we allow user to select book (or all books) and input 
#   the word they are interested in.
# Author: Wenyi Shi
# Date: Nov 10, 2024


# ===============================================
# Required packages
# (you can use other packages if you want to)
# ===============================================
library(shiny)
library(tidyverse)  # for data manipulation and graphics
library(tidytext)   # for text mining
library(DT)         # to render Data Tables nicely
library(plotly)     # if you are interested in using ggplotly()



# ===============================================
# Import data
# ===============================================
# we use the CSV file
tbl <- read_csv(file = "harry_potter_books.csv", col_types = "ccc")

# vector with names of books
book_names = unique(tbl$book)

# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  # Application title
  titlePanel("Text Analysis of Harry Potter"),
  
  # -------------------------------------------------------
  # Input widgets
  # -------------------------------------------------------
  fluidRow(
    # widgets of column 1
    column(4,
           p(em("Analysis 1 & 2")),
           selectInput(inputId = "selected_book", 
                       label = "Select a book", 
                       choices = c(book_names, "All books"),
                       selected = book_names[1])
    ), # closes column 1
    
    # widgets of column 2
    column(3,
           p(em("Analysis 1")),
           radioButtons(inputId = "stopwords", 
                        label = "Stopwords", 
                        choices = c("use all tokens" = "opt1",
                                    "remove stopwords" = "opt2"), 
                        selected = "opt1")
    ), # closes column 2
    
    # widgets of column 3
    column(3,
           p(em("Analysis 1")),
           sliderInput(inputId = "lines_of_section", 
                       label = "# of lines for one section",
                       min = 20,
                       max = 100,
                       value = 50)
    ), # closes column 3

    # widgets of column 4
    column(2,
           p(em("Analysis 2")),
           textInput(inputId = "key_word",
                     label = strong("Sentiment word for bigram analysis"),
                     value = "dark")
    ), # closes column 4
    
  ), # closes fluidRow
  
  hr(), # horizontal rule
  
  # -------------------------------------------------------
  # Tabset Panel of outputs
  # Customize the following output elements with your own outputs
  # -------------------------------------------------------
  tabsetPanel(type = "tabs",
              tabPanel("Analysis1",
                       h3("Change of Sentiments Analysis"),
                       plotOutput("plot1"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis2", 
                       h3("Bigram Analysis for Sentiment Word"),
                       plotOutput("plot2"),
                       hr(),
                       dataTableOutput('table2'))
  ) # closes tabsetPanel
  
) # closes ui



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # ===============================================
  # Reactive conductor table for Tab 1
  # Change of sentiments
  # ===============================================
  # reactive conductor
  change_of_sentiment <- reactive({
    # Only single book selected allowed, if "All books" selected
    # default to the first book
    if (input$selected_book != "All books") {
      selected_book = tbl |>
        filter(book == input$selected_book)
    } else {
      selected_book = tbl |>
        filter(book == book_names[1])
    }
    
    # mutate to get row number
    persuasion_tokens <- selected_book |>
      mutate(linenumber = row_number()) |>
      unnest_tokens(output = word, input = text)
    
    # Should stopwords be removed?
    if (input$stopwords == "opt2") {
      persuasion_tokens = persuasion_tokens |>
        anti_join(stop_words, by = "word")
    } else {
      persuasion_tokens = persuasion_tokens
    }
    
    persuasion_sentiments <- persuasion_tokens |>
      inner_join(sentiments, by = "word")
    
    persuasion_sections <- persuasion_sentiments |>
      count(index = linenumber %/% input$lines_of_section, sentiment)
    
    persuasion_sentim_flow <- persuasion_sections |>
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
      mutate(sentiment = positive - negative)
    
    persuasion_sentim_flow
  })
  
  
  # ===============================================
  # Reactive conductor table for Tab 2
  # Bigram analysis for typed word
  # ===============================================
  # reactive conductor
  selected_bi_tokens <- reactive({
    # Single book to be selected? Or use "All books"?
    selected_book = tbl
    if (input$selected_book != "All books") {
      selected_book = tbl |>
        filter(book == input$selected_book)
    }
    
    persuasion_bigrams <- selected_book |>
      unnest_tokens(
        output = bigram, 
        input = text, 
        token = "ngrams", 
        n = 2)
    
    # step 2) filter those containing typed word
    persuasion_happy_bigrams = persuasion_bigrams |>
      filter(str_detect(bigram, input$key_word)) |>
      count(bigram, sort = TRUE, name = "count")
    
    persuasion_happy_bigrams
  })
  
  
  # ===============================================
  # Outputs for the first TAB
  # Change of sentiment analysis
  # ===============================================
  # plot1: bar chart of common words
  output$plot1 <- renderPlot({
    if (input$selected_book != "All books") {
      selected_book = input$selected_book
    } else {
      selected_book = book_names[1]
    }
    
    change_of_sentiment() |>
      ggplot(aes(x = index, y = sentiment, fill = factor(sign(sentiment)))) +
      geom_col(show.legend = FALSE) +
      theme_bw() +
      labs(title = paste("Sentiment through ", selected_book, sep=""))
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    change_of_sentiment()
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  # code for plot2
  output$plot2 <- renderPlot({
    selected_bi_tokens() |>
      slice_head(n = 15) |>
      ggplot(aes(x = count, y = reorder(bigram, count))) +
      geom_col() +
      labs(title = paste("Top-15 bigrams containing word '", input$key_word, "'", sep=""),
           y = "bigram")

  })
  
  # code for statistics
  output$table2 <- renderDataTable({
    selected_bi_tokens() |>
      slice_head(n = 15)
  })
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

