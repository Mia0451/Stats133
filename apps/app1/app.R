# Title: S&P 500 Analysis
# Description: 
# Author: Wenyi Shi
# Date: Nov 1, 2024


# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse)  # for data manipulation and graphics
library(lubridate)  # for working with dates
library(tidyquant)  # financial & quant functions
library(plotly)     # for web interactive graphics


# ======================================================
# Auxiliary objects/functions 
# (that don't depend on input widgets)
# ======================================================
# You may want to add one or more auxiliary objects for your analysis
# (by "auxiliary" we mean anything that doesn't depend on input widgets)





# =======================================================
# Define UI for application
# =======================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("S&P 500 Returns"),
  
  # -------------------------------------------------------
  # Sidebar with input widgets
  # (adapt code with widgets of your choice!!!)
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # inputs
      sliderInput(
        inputId = "years_range",
        label = "Range of years",
        min = 1928, 
        max = 2023,
        value = c(1990, 2020)),
      radioButtons(inputId = "scale", 
                   label = "Scale of y axis", 
                   choices = c("linear", "log10"),
                   selected = "linear"),
      sliderInput(
        inputId = "multi_years_return",
        label = "# of years in return",
        min = 1, 
        max = 10,
        value = 2),
      selectInput(inputId = "statistics",
                  label = "Statistics to impose",
                  choices = c("mean", "median", "stddev"),
                  selected = "mean"),
    ),  # closes sidebarPanel of inputs
    
    # -------------------------------------------------------
    # Main Panel with outputs: plots and table
    # -------------------------------------------------------
    mainPanel(
      plotlyOutput(outputId = "plot1"),
      hr(),
      plotlyOutput(outputId = "plot2"),
      hr(),
      h3("S&P 500 Statistic analysis"),
      tableOutput(outputId = "table"),
    ) # closes mainPanel of outputs
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)


# ======================================================
# Define server logic
# ======================================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Auxiliary table (note that this is a reactive conductor)
  # (adapt code to your own analysis)
  # ------------------------------------------------------------
  tbl = reactive({
    # the following code is for demo purposes only; adapt it!!!
    sp500_all_data <- tq_get("^GSPC", from = "1928-01-03", to = "2023-12-29")
    sp500_dat = sp500_all_data |>
      select(date, close) |>
      mutate(year = year(date)) |>
      filter(year >= input$years_range[1] & year <= input$years_range[2])
    sp500_dat
  })
  
  tbl_multi_returns = reactive({
    year_start <- input$years_range[1]
    year_end <- input$years_range[2]
    year_stop <- year_end - (input$multi_years_return - 1)
    sp500_multi_years_returns <- tibble(years = character(), return = numeric())
    for (i in year_start:year_stop) {
      years_seq <- i:(i + ((input$multi_years_return - 1)))
      years_str <- paste(years_seq[1], years_seq[input$multi_years_return], sep = "-")  
      return_value <- tbl() %>%
        filter(year %in% years_seq) %>%
        summarise(return = (last(close) - first(close)) / first(close)) %>%
        pull(return)
      sp500_multi_years_returns <- sp500_multi_years_returns %>%
        add_row(years = years_str, return = return_value)
    }
    sp500_multi_years_returns
  })
  
  
  # ------------------------------------------------------------
  # Plot (timeline of daily closing values)
  # (adapt code to make a timeline according to your analysis!!!)
  # ------------------------------------------------------------
  output$plot1 <- renderPlotly({
    # the following code is for demo purposes only; adapt it!!!
    # ggplot(data = tbl(), aes(x = date, y = close)) +
    #  geom_line()
    if (input$scale == "log10") {
      end = ", logarithmic scale)"  # Apply log10 scale to the y-axis
    } else {
      end = ")"
    }
    
    temp <- ggplot(tbl(), aes(x = date, y = close)) +
      geom_line(color = "blue") +
      labs(title = paste("S&P 500 (", as.character(input$years_range[1]), "-", as.character(input$years_range[2]), end, sep=""),
           x = NULL,
           y = "closing value") +
      theme_minimal()
    
    if (input$scale == "log10") {
      temp <- temp + scale_y_log10()  # Apply log10 scale to the y-axis
    } else {
      temp
    }
  })


  # ------------------------------------------------------------
  # Plot (bar-chart of multi-year returns)
  # (adapt code to make a timeline according to your analysis!!!)
  # ------------------------------------------------------------
  
  output$plot2 <- renderPlotly({
    # the following code is for demo purposes only; adapt it!!!
    multi_years = tbl_multi_returns()
    temp <- ggplot(tbl_multi_returns(), aes(x = years, y = return)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(title = paste("S&P 500 ", as.character(input$multi_years_return), "-years returns", sep=""),
           x = NULL,
           y = "Return (%)") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    stats = summarize(
      multi_years,
      mean = mean(multi_years$return, na.rm = TRUE),
      sd = sd(multi_years$return, na.rm = TRUE),
      median = median(multi_years$return, na.rm = TRUE)
    )
    
    if (input$statistics == "mean") {
      temp <- temp + geom_hline(data= stats, aes(yintercept = mean))
    } else if (input$statistics == "median") {
      temp <- temp + geom_hline(data= stats, aes(yintercept = median))
    } else if (input$statistics == "stddev") {
      temp <- temp + geom_hline(data= stats, aes(yintercept = sd))
    }
    temp
  })
  
    
  # ------------------------------------------------------------
  # Table
  # (adapt code to display appropriate table!!!)
  # ------------------------------------------------------------
  output$table <- renderTable({
    # the following code is for demo purposes only; adapt it!!!
    multi_years = tbl_multi_returns()
    stats = summarize(
      multi_years,
      mean = mean(multi_years$return, na.rm = TRUE),
      sd = sd(multi_years$return, na.rm = TRUE),
      median = median(multi_years$return, na.rm = TRUE)
    )
    quants <- quantile(multi_years$return, probs = c(0.1, 0.25, 0.75, 0.9))
    iqr =  IQR(multi_years$return, na.rm = FALSE)
    a = matrix(stats)
    for (i in 1:4) {
      a <- rbind(a, quants[i])
    }
    a <- rbind(a, iqr[1])
    a <- cbind(c("mean", "sddev", "median", "perc10", "perc25", "perc75", "perc90", "iqr"), a)
    colnames(a) = c("statistic", "value")
    a
  })
  
 
} # closes server


# Run the application 
shinyApp(ui = ui, server = server)
