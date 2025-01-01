# title: Top-n mtcars
# description: Creates barcharts of a given numeric variable from mtcars data
# author: Gaston Sanchez
# date: Spring 2024


# ===============================================
# Packages
# ===============================================
library(shiny)
library(tidyverse)


# ===============================================
# a couple of changes to "mtcars" data: 
# - adding column "model"
# - removing rownames
# ===============================================
mtcars = mutate(mtcars, model = rownames(mtcars))
rownames(mtcars) = 1:nrow(mtcars)


# =======================================================
# Define UI for graphing barchart, and displaying table
# =======================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Top-n cars"),
  
  # -------------------------------------------------------
  # Sidebar with a select-box input, and a slider input
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      varSelectInput(inputId = "variable", 
                     label = "Select a variable", 
                     data = select(mtcars, -model), 
                     selected = "mpg"),
      sliderInput(inputId = "n",
                  "Number of cars:",
                  min = 1,
                  max = nrow(mtcars), 
                  step = 1,
                  value = 5)
    ),
    
    # -------------------------------------------------------------
    # Main Panel with outputs: barchart, and table of top-n cars
    # -------------------------------------------------------------
    mainPanel(
      plotOutput(outputId = "graphic"),
      hr(),
      dataTableOutput(outputId = "table")
    )
  ) # closes sidebarLayout
) # closes fluidPage


# ==============================================================
# Server logic to draw barchart, and compute top-n table
# ==============================================================
server <- function(input, output) {

  # table of top-n cars (this a reactive conductor!!!)
  topn = reactive({
    mtcars |>
      arrange(desc(!!input$variable)) |>
      slice(1:input$n) |>
      select(model, !!input$variable)
  })
  
  # output graphic
  # (notice the use of the reactive conductor, as well as the bang-bang!!)
  output$graphic <- renderPlot({
    ggplot(data = topn(), aes(x = !!input$variable, 
                            y = reorder(model, !!input$variable))) +
      geom_col() +
      labs(title = paste("Top", input$n, "cars"),
           y = "Model")
  })
  
  # output table
  # (notice the use of the reactive conductor)
  output$table <- renderDataTable({
    topn()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
