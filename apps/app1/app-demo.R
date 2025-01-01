# Title: Harry Potter Text Analysis
#
# Description: Sample app that performs 
# 1) a basic frequency word analysis, and
# 2) a basic sentiment analysis.
#
# Author: Gaston Sanchez
# Date: Fall 2024


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
# Import data, and tokenization
# Notice that none of these operations depend on input widgets!
# ===============================================
# for demo purposes of the "template", we use the CSV file
tbl <- read_csv(file = "harry_potter_books.csv", col_types = "ccc")

# vector with names of books
book_names = unique(tbl$book)

# tokenization
tbl_tokens = tbl |> 
  unnest_tokens(word, text)



# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  # Application title
  titlePanel("Text Analysis Demo App"),
  
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
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
           p(em("Analysis 1 & 2")),
           sliderInput(inputId = "top_n", 
                       label = "Top n words",
                       min = 1,
                       max = 50,
                       value = 10)
    ), # closes column 2
    
    # widgets of column 3
    column(3,
           p(em("Analysis 1 & 2")),
           radioButtons(inputId = "stopwords", 
                        label = "Stopwords", 
                        choices = c("use all tokens" = "opt1",
                                    "remove stopwords" = "opt2"), 
                        selected = "opt1")
    ), # closes column 3
    
    # widgets of column 4
    column(2,
           p(em("Analysis 2")),
           checkboxInput(inputId = "facets",
                         label = strong("Facet by sentiment"),
                         value = FALSE)
    ) # closes column 4
    
  ), # closes fluidRow
  
  hr(), # horizontal rule
  
  # -------------------------------------------------------
  # Tabset Panel of outputs
  # Customize the following output elements with your own outputs
  # -------------------------------------------------------
  tabsetPanel(type = "tabs",
              # First tab
              tabPanel(title = "Analysis1",
                       h3("Word Frequency Analysis"),
                       plotOutput("plot1"),
                       hr(),
                       dataTableOutput('table1')),
              # Second tab
              # (notice the use of plotly output!)
              tabPanel(title = "Analysis2", 
                       h3("Basic Sentiment Analysis"),
                       plotlyOutput("plot2"),
                       hr(),
                       dataTableOutput('table2'))
  ) # closes tabsetPanel
  
) # closes ui



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # ===============================================
  # Reactive conductor table for Tabs 1 & 2
  # ===============================================
  # reactive conductor
  selected_tokens <- reactive({
    # Should stopwords be removed?
    if (input$stopwords == "opt2") {
      selected_tokens = tbl_tokens |>
        anti_join(stop_words, by = "word")
    } else {
      selected_tokens = tbl_tokens
    }
    
    # Single book to be selected? Or use "All books"?
    if (input$selected_book != "All books") {
      selected_tokens = selected_tokens |>
        filter(book == input$selected_book)
    }
    
    selected_tokens
  })
  
  
  # ===============================================
  # Outputs for TAB-1: Word Frequency Analysis
  # ===============================================
  
  # auxiliary reactive conductor
  tbl_top_n <- reactive({
    # Top-n words
    selected_tokens() |>
      count(word, name = "count", sort = TRUE) |>
      slice_head(n = input$top_n)
  })
  
  
  # plot1: bar chart of common words
  output$plot1 <- renderPlot({
    tbl_top_n() |>
      mutate(word = reorder(word, count)) |>
      ggplot(aes(y = word, x = count)) +
      geom_col() +
      theme_minimal() + 
      labs(title = paste(input$top_n, "Most common words in", input$selected_book))
  })
  
  
  # table1: word frequencies
  output$table1 <- renderDataTable({
    tbl_top_n()
  })
  
  
  # ===============================================
  # Outputs for TAB-2: Basic Sentiment Analysis
  # ===============================================
  
  # auxiliary reactive table of word counts, and their sentiments
  tbl_sentiments <- reactive({
    # word counts with sentiments
    count_tokens <- selected_tokens() |>
      count(word, name = "count", sort = TRUE) |>
      inner_join(sentiments, by = "word") |> 
      slice_head(n = input$top_n)
  })

  # plot2: bar chart of common words with sentiments
  output$plot2 <- renderPlotly({
    gg2 = tbl_sentiments() |>
      ggplot() +
      geom_col(aes(x = count, y = reorder(word, count), fill = sentiment)) +
      labs(title = paste(input$top_n, "common words with an associated sentiment"),
           subtitle = input$selected_book,
           x = "count",
           y = "word")
    
    # Should facets by sentiments be displayed?
    if (input$facets) {
      gg2 + facet_wrap(~ sentiment, scales = "free_y") +
        theme(legend.position = "none")
    } else {
      gg2
    }
  })
  
  # table2: word freqs and their sentiments
  output$table2 <- renderDataTable({
    tbl_sentiments()
  })
  
} # close server



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

