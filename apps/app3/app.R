# Title: California Crash Data Report
# Description: An app for analyzing California Crash in 2014 ~ 2023
# Author: Wenyi Shi
# Date: Dec 1, 2024


# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse)    # data wrangling and graphics
library(lubridate)    # for working with dates
library(leaflet)      # web interactive maps
library(plotly)       # web interactive graphics


# =======================================================
# Import data
#
# For demo purposes, in this "template" we use storms data
# (but you will have to replace this with the crash data)
#
# Uncomment the lines below in order to import the crash data!!!
# We're assuming the CSV file is in the working directory of your app.
# =======================================================
crashes = read_csv(
   file = "crashes_california_2014_2023.csv", 
   col_types = list(
     col_double(),    #  1) CASE_ID
     col_double(),    #  2) ACCIDENT_YEAR
     col_date(),      #  3) COLLISION_DATE 
     col_double(),    #  4) COLLISION_TIME 
     col_double(),    #  5) HOUR 
     col_integer(),   #  6) DAY_OF_WEEK 
     col_character(), #  7) WEATHER_1 
     col_character(), #  8) WEATHER_2 
     col_character(), #  9) STATE_HWY_IND
     col_character(), # 10) COLLISION_SEVERITY 
     col_integer(),   # 11) NUMBER_KILLED 
     col_integer(),   # 12) NUMBER_INJURED 
     col_integer(),   # 13) PARTY_COUNT 
     col_character(), # 14) PCF_VIOL_CATEGORY 
     col_character(), # 15) TYPE_OF_COLLISION 
     col_character(), # 16) ROAD_SURFACE 
     col_character(), # 17) ROAD_COND_1 
     col_character(), # 18) ROAD_COND_2 
     col_character(), # 19) LIGHTING 
     col_character(), # 20) PEDESTRIAN_ACCIDENT 
     col_character(), # 21) BICYCLE_ACCIDENT 
     col_character(), # 22) MOTORCYCLE_ACCIDENT 
     col_character(), # 23) TRUCK_ACCIDENT 
     col_character(), # 24) NOT_PRIVATE_PROPERTY 
     col_character(), # 25) ALCOHOL_INVOLVED 
     col_character(), # 26) COUNTY 
     col_character(), # 27) CITY 
     col_character(), # 28) PO_NAME
     col_double(),    # 29) ZIP_CODE
     col_double(),    # 30) POINT_X 
     col_double()     # 31) POINT_Y 
))


crashes = filter(crashes, ACCIDENT_YEAR > 2020)
accident_years = unique(crashes$ACCIDENT_YEAR) |> sort()
accident_types = c("PEDESTRIAN_ACCIDENT", "BICYCLE_ACCIDENT", "MOTORCYCLE_ACCIDENT", "TRUCK_ACCIDENT")
violation_category = unique(crashes$PCF_VIOL_CATEGORY)
day_of_week = unique(crashes$DAY_OF_WEEK) |> sort()

# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("California Crash Data Analysis"),
  
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # ---------------------------------------------
      # input widgets of first tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==1",
        h4("Exploratory Analysis"),
        # replace with your widgets
        selectInput(inputId = "select_year", 
                    label = "Select a year", 
                    choices = c(accident_years, "All years"),
                    selected = accident_years[1]),
        textInput(inputId = "county",
                  label = strong("Crash report for a specific county"),
                  value = "SAN FRANCISCO")
      ), # closes 1st panel
      
      # ---------------------------------------------
      # input widgets of second tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==2",
        h4("Map"),
        # replace with your widgets
        selectInput(inputId = "select_year_second", 
                    label = "Select a year", 
                    choices = c(accident_years, "All years"),
                    selected = accident_years[1]),
        textInput(inputId = "city",
                  label = strong("Crash report for specific city"),
                  value = "Los Angeles"),
        selectInput(inputId = "violation_category", 
                    label = "Select a violation category", 
                    choices = c(violation_category, "All categories"),
                    selected = violation_category[1]),
        selectInput(inputId = "day_of_week", 
                    label = "Select day of the week", 
                    choices = c(day_of_week, "All days"),
                    selected = day_of_week[1]),
      ), # closes 2nd panel
      
    ), # closes sidebarPanel
    
    
    # -------------------------------------------------------
    # Main Panel with 2 tabsets: 
    # tab1: exploratory analysis
    # tab2: map of crashes
    # -------------------------------------------------------
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # first tab (graphic)
        tabPanel(title = "Explore",
                 value = 1,
                 plotlyOutput(outputId = "plot1"),
                 hr(),
                 plotlyOutput(outputId = "plot2"),
                 hr(),
                 plotlyOutput(outputId = "plot3")),
        # second tab (map)
        tabPanel(title = "Map",
                 value = 2,
                 leafletOutput("map", height = 600)),
        # selected tab
        id = "tabselected"
        
      ) # closes tabsetPanel
    ) # closes mainPanel
    
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)



# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  
  # ------------------------------------------------
  # Output for first TAB (i.e. summary plots)
  # (adapt code to your analysis)
  # ------------------------------------------------
  # reactive conductor
  selected_crashes <- reactive({
    selected_crashes = crashes
    if (input$select_year != "All years") {
      selected_crashes = crashes |>
        filter(ACCIDENT_YEAR == input$select_year)
    }
    
    selected_crashes
  })
  
  selected_year <- reactive({
    selected_year = "2014 - 2023"
    if (input$select_year != "All years") {
      selected_year = input$select_year
    }
    
    selected_year
  })
  
  selected_county <- reactive({
    selected_county = "all counties"
    if (input$county != "") {
      selected_county = input$county
    }
    
    selected_county
  })
  
  output$plot1 <- renderPlotly({
    crashes_summary = selected_crashes() |>
      group_by(COUNTY) |>
      summarise(num_of_crashes = n())
    
    crashes_summary |>
      ggplot() + 
      geom_col(aes(x = reorder(COUNTY, -num_of_crashes), y = num_of_crashes, width=.5), fill = "skyblue") +
      labs(title = paste("Number of crashes per county in ", selected_year(), sep=""),
           x = "County",
           y = "# of crashes") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$plot2 <- renderPlotly({
    crashes_for_county = selected_crashes()
    
    if (input$county != "") {
      crashes_for_county = crashes_for_county |>
        filter(COUNTY == toupper(input$county))
    }

    crashes_for_county = crashes_for_county |>
      group_by(COLLISION_SEVERITY, DAY_OF_WEEK) |>
      summarise(num_of_crashes = n()) |>
      group_by(COLLISION_SEVERITY)
    
    crashes_for_county |>
      ggplot() + 
      geom_col(aes(x = DAY_OF_WEEK, y = num_of_crashes), fill = "#FC4661") +
      labs(title = paste("Number of crashes in ", selected_county(), " in ", selected_year(), " per severity", sep = ""),
           subtitle = "Different severity in each day in the week",
           x = "Day of the week",
           y = "# of crashes") +
      theme_minimal() +
      facet_wrap(~ COLLISION_SEVERITY)
  })
  
  
  output$plot3 <- renderPlotly({
    crashes_per_month = selected_crashes() |>
      mutate(month = month(COLLISION_DATE)) |>
      group_by(ACCIDENT_YEAR, month) |>
      summarise(num_crashes = n())
    
    crashes_per_month |>
      ggplot(aes(x = month, y = num_crashes)) + 
      geom_point(aes(color = ACCIDENT_YEAR), size = 1) +
      geom_line(aes(color = ACCIDENT_YEAR)) +
      labs(title = paste("Number of crashes in each month in California in ", selected_year(), sep = ""),
           x = "Month",
           y = "# of crashes") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  
  
  # -----------------------------------------------
  # Output for second TAB (i.e. map)
  # (adapt code to make your map of crashes)
  # -----------------------------------------------
  # reactive conductor
  selected_crashes_second_tab <- reactive({
    selected_crashes = crashes
    
    if (input$select_year_second != "All years") {
      selected_crashes = selected_crashes |>
        filter(ACCIDENT_YEAR == input$select_year_second)
    }
    
    if (input$violation_category != "All categories") {
      selected_crashes = selected_crashes |>
        filter(PCF_VIOL_CATEGORY == input$violation_category)
    }
    
    if (input$city != "") {
      selected_crashes = selected_crashes |>
        filter(PO_NAME == input$city)
    }
    
    if (input$day_of_week != "All days") {
      selected_crashes = selected_crashes |>
        filter(DAY_OF_WEEK == input$day_of_week)
    }
    
    selected_crashes
  })
  
  output$map <- renderLeaflet({
    # the following code is for demo purposes only; adapt it!!!
    selected_crashes = selected_crashes_second_tab()
    
    color_palette = colorFactor(
      palette = rainbow(n = length(unique(selected_crashes$COLLISION_SEVERITY))),
      domain = selected_crashes$COLLISION_SEVERITY
    )
      
    selected_crashes |>
      leaflet() |>
      addTiles() |>
      addCircleMarkers(
        lng = ~POINT_X, 
        lat = ~POINT_Y, 
        radius = 2, 
        color = ~color_palette(COLLISION_SEVERITY), 
        label = ~COLLISION_SEVERITY)
  })
  
  
} # closes server



# ===============================================
# Run the application
# ===============================================
shinyApp(ui = ui, server = server)
