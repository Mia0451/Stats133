# ===============================================
# Fill in the following fields
# ===============================================
# Title:
# Description:
# Details: 
# Author: 
# Date:


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
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with harry potter data)
dat <- dplyr::starwars



# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  # Application title
  titlePanel("Title of your app"),
  
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
  # -------------------------------------------------------
  fluidRow(
    # replace with your widgets
    column(3,
           p(em("Inputs (column 1)")),
           numericInput(inputId = "widget1", 
                        label = "Widget 1", 
                        value = 133)
    ), # closes column 1
    
    # replace with your widgets
    column(3,
           p(em("Inputs (column 2)")),
           numericInput(inputId = "widget2", 
                        label = "Widget 2", 
                        value = 133)
    ), # closes column 2
    
    # replace with your widgets
    column(3,
           p(em("Inputs (column 3)")),
           numericInput(inputId = "widget3", 
                        label = "Widget 3", 
                        value = 133)
    ), # closes column 3
    
    # replace with your widgets
    column(3,
           p(em("Inputs (column 4)")),
           numericInput(inputId = "widget4", 
                        label = "Widget 4", 
                        value = 133)
    ) # closes column 4
    
  ), # closes fluidRow
  
  hr(), # horizontal rule
  
  # -------------------------------------------------------
  # Tabset Panel of outputs
  # Customize the following output elements with your own outputs
  # -------------------------------------------------------
  tabsetPanel(type = "tabs",
              tabPanel("Analysis1",
                       h3("What kind of analysis1?"),
                       plotOutput("plot1"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis2", 
                       h3("What kind of analysis2"),
                       plotOutput("plot2"),
                       hr(),
                       dataTableOutput('table2'))
  ) # closes tabsetPanel
  
) # closes ui



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. dummy data frame to be used in plot1)
  dat_freq <- reactive({
    dat |> group_by(sex) |> count()
  })
  
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # code for plot1
  output$plot1 <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = dat_freq(), aes(x = sex, y = n)) +
      geom_col()
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    # replace the code below with your code!!!
    dat_freq()
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  # code for plot2
  output$plot2 <- renderPlot({
    # replace the code below with your code!!!
    p1 = ggplot(data = dat, aes(x = height, y = mass)) +
      geom_point()
    p1
  })
  
  # code for statistics
  output$table2 <- renderDataTable({
    # replace the code below with your code!!!
    dat |> 
      group_by(gender) |> 
      summarize(
        median_height = median(height, na.rm = TRUE),
        median_mass = median(mass, na.rm = TRUE)
      )
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

