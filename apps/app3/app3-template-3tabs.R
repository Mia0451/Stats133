# Title:
# Description:
# Author:
# Date:


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
# Uncomment the lines below in order to import the data!!!
# We're assuming the CSV file is in the working directory of your app.
# =======================================================
# crashes = read_csv(
#   file = "crashes_california_2014_2023.csv", 
#   col_types = list(
#     col_double(),    #  1) CASE_ID
#     col_double(),    #  2) ACCIDENT_YEAR
#     col_date(),      #  3) COLLISION_DATE 
#     col_double(),    #  4) COLLISION_TIME 
#     col_double(),    #  5) HOUR 
#     col_integer(),   #  6) DAY_OF_WEEK 
#     col_character(), #  7) WEATHER_1 
#     col_character(), #  8) WEATHER_2 
#     col_character(), #  9) STATE_HWY_IND
#     col_character(), # 10) COLLISION_SEVERITY 
#     col_integer(),   # 11) NUMBER_KILLED 
#     col_integer(),   # 12) NUMBER_INJURED 
#     col_integer(),   # 13) PARTY_COUNT 
#     col_character(), # 14) PCF_VIOL_CATEGORY 
#     col_character(), # 15) TYPE_OF_COLLISION 
#     col_character(), # 16) ROAD_SURFACE 
#     col_character(), # 17) ROAD_COND_1 
#     col_character(), # 18) ROAD_COND_2 
#     col_character(), # 19) LIGHTING 
#     col_character(), # 20) PEDESTRIAN_ACCIDENT 
#     col_character(), # 21) BICYCLE_ACCIDENT 
#     col_character(), # 22) MOTORCYCLE_ACCIDENT 
#     col_character(), # 23) TRUCK_ACCIDENT 
#     col_character(), # 24) NOT_PRIVATE_PROPERTY 
#     col_character(), # 25) ALCOHOL_INVOLVED 
#     col_character(), # 26) COUNTY 
#     col_character(), # 27) CITY 
#     col_character(), # 28) PO_NAME
#     col_double(),    # 29) ZIP_CODE
#     col_double(),    # 30) POINT_X 
#     col_double()     # 31) POINT_Y 
#   ))


# this is just for demo purposes (delete these commands)
storms = storms |>
  mutate(id = paste0(name, "-", year))



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
        numericInput(inputId = "widget1",
                    label = "Widget 1",
                    value = 133),
        numericInput(inputId = "widget2",
                     label = "Widget 2",
                     value = 133),
        numericInput(inputId = "widget3",
                     label = "Widget 3",
                     value = 133),
        p('You can add more widgets if you want')
      ), # closes 1st panel
      
      # ---------------------------------------------
      # input widgets of second tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==2",
        h4("Map"),
        # replace with your widgets
        numericInput(inputId = "widget4",
                     label = "Widget 4",
                     value = 133),
        numericInput(inputId = "widget5",
                     label = "Widget 5",
                     value = 133),
        numericInput(inputId = "widget6",
                     label = "Widget 6",
                     value = 133),
        numericInput(inputId = "widget7",
                     label = "Widget 7",
                     value = 133),
        p('You can add more widgets if you want')
      ), # closes 2nd panel
      
      # ---------------------------------------------
      # input widgets of third tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==3",
        h4("More Analysis"),
        # replace with your widgets
        numericInput(inputId = "widget8",
                     label = "Widget 8",
                     value = 133),
        numericInput(inputId = "widget9",
                     label = "Widget 9",
                     value = 133),
      ) # closes 3rd panel
      
    ), # closes sidebarPanel
    
    
    # -------------------------------------------------------
    # Main Panel with 3 tabsets: 
    # tab1: exploratory analysis
    # tab2: map of crashes
    # tab3: table
    # -------------------------------------------------------
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # first tab (graphic)
        tabPanel(title = "Explore",
                 value = 1,
                 plotlyOutput(outputId = "plot1"),
                 hr(),
                 plotOutput(outputId = "plot2"),
                 hr(),
                 plotOutput(outputId = "plot3")),
        # second tab (map)
        tabPanel(title = "Map",
                 value = 2,
                 leafletOutput("map", height = 600)),
        # third tab (other)
        tabPanel(title = "Table",
                 value = 3,
                 dataTableOutput(outputId = "table")),
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
  # Output for first TAB (i.e. summary plot)
  # (adapt code to your analysis)
  # ------------------------------------------------
  output$plot1 <- renderPlotly({
    # the following code is for demo purposes only; adapt it!!!
    storms_summary = storms |>
      group_by(year) |>
      summarise(
        num_storms = length(unique(id)),
        med_wind = median(wind)
      )
    
    storms_summary |>
      ggplot() + 
      geom_col(aes(x = year, y = num_storms), fill = "skyblue") +
      labs(title = "Number of Tropical Cyclones per Year",
           y = "Number of Storms") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))
  })

  
  output$plot2 <- renderPlot({
    # the following code is for demo purposes only; adapt it!!!
    avg_storms_per_month = storms |>
      group_by(year, month) |>
      summarize(num_storms = length(unique(id)),
                .groups = "drop") |> 
      group_by(month) |> 
      summarize(avg = mean(num_storms))
    
    avg_storms_per_month |>
      ggplot() + 
      geom_col(aes(x = month, y = avg), fill = "#FC4661") +
      labs(title = "Average Number of Tropical Cyclones per Month",
           y = "Average") +
      scale_x_continuous(breaks = 1:12) +
      theme_minimal()
  })
  
  
  output$plot3 <- renderPlot({
    # fill this output with one of your plots
    
  })
  
  
  # -----------------------------------------------
  # Output for second TAB (i.e. map)
  # (adapt code to make your map of crashes)
  # -----------------------------------------------
  output$map <- renderLeaflet({
    # the following code is for demo purposes only; adapt it!!!
    storms2010 = storms |>
      filter(year == 2010)
    
    color_palette = colorFactor(
      palette = rainbow(n = length(unique(storms2010$name))),
      domain = storms2010$name
    )
      
    storms2010 |>
      leaflet() |>
      addTiles() |>
      addCircles(lng = ~long,
                 lat = ~lat,
                 label = ~paste0(name, "; ", month, "-", day),
                 color = ~color_palette(name))
  })
  
  
  # -----------------------------------------------
  # Output for third TAB (i.e. table of fires)
  # (adapt code to show a table of fires in selected year)
  # -----------------------------------------------
  output$table <- renderDataTable({
    # the following code is for demo purposes only; adapt it!!!
    storms |>
      filter(year == 2010) |>
      group_by(name) |>
      summarise(max_wind = max(wind),
                min_pres = min(pressure))
  })
  
  
  
} # closes server



# ===============================================
# Run the application
# ===============================================
shinyApp(ui = ui, server = server)
