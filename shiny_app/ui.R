# ui.R
library(shiny)
library(plotly)

# Load data (make sure 'cleaned_crimedata.csv' is in the same directory)
crime_data <- read_csv("cleaned_crimedata.csv")

ui <- fluidPage(
  
  # Application title
  titlePanel("Los Angeles Crime Data Explorer"),
  
  # Tabset panel to switch between plots
  tabsetPanel(
    type = "tabs",
    
    # Tab for Spatial Crime Map
    tabPanel(
      "Spatial Crime Map",
      
      # Sidebar with controls for spatial map
      sidebarLayout(
        sidebarPanel(
          # Year filter for spatial map
          checkboxGroupInput(
            inputId = "year_map",
            label = "Select Years for Crime Map:",
            choices = unique(crime_data$year),
            selected = unique(crime_data$year)  # Select all years by default
          ),
          
          # Crime type filter for spatial map
          selectInput(
            inputId = "crime_type_map",
            label = "Select Crime Type for Map:",
            choices = unique(crime_data$crime_description),
            multiple = TRUE,  # Allow multiple selections
            selected = NULL
          ),
          
          # Month filter for spatial map
          checkboxGroupInput(
            inputId = "month_map",
            label = "Select Months for Map:",
            choices = unique(crime_data$month),
            selected = unique(crime_data$month)  # Select all months by default
          ),
          
          # Time of day filter for spatial map
          sliderInput(
            inputId = "time_of_day_map",
            label = "Select Time of Day for Map:",
            min = 0,
            max = 23,
            value = c(0, 23),
            step = 1
          )
        ),
        
        # Main panel for spatial map
        mainPanel(
          plotlyOutput("crime_map")
        )
      )
    ),
    
    # Tab for Crime Trends Analysis
    tabPanel(
      "Crime Trends Analysis",
      
      # Sidebar with controls for spatial map
      sidebarLayout(
        sidebarPanel(
          # Year filter for spatial map
          checkboxGroupInput(
            inputId = "year_trend",
            label = "Select Years for Trend Analysis:",
            choices = unique(crime_data$year),
            selected = unique(crime_data$year)  # Select all years by default
          ),
          
          # Crime type filter
          selectInput(
            inputId = "crime_type_trend",
            label = "Select Crime Type:",
            choices = c("All" = "", unique(crime_data$crime_description)),  # Include "All" option
            selected = ""  # Select "All" by default
          ),
          
          # Plot type selection
          radioButtons(
            inputId = "plot_type",
            label = "Select Plot Type",
            choices = c("Line" = "line", "Bar" = "bar"),
            selected = "line"
          ),
          
          # Time aggregation selection
          radioButtons(
            inputId = "time_agg",
            label = "Select Time Aggregation",
            choices = c("Monthly" = "month", "Seasonal" = "season", 
                        "Weekday" = "weekday", "Hourly" = "hour_occurred"),
            selected = "month"
          )
        ),
        
        # Main panel for crime trend plots
        mainPanel(
          plotlyOutput("crime_trend_plot")
        )
      )
    ),
    
    # Tab for Weapon and Victim Age Box Plot
    tabPanel(
      "Weapon and Victim Age Box Plot",
      
      # Sidebar with controls for the box plot
      sidebarLayout(
        sidebarPanel(
          # Area filter
          selectInput(
            inputId = "area_boxplot",
            label = "Select Area:",
            choices = c("All" = "", unique(crime_data$area_name)),  # Include "All" option
            selected = ""  # Select "All" by default
          ),
          
          # Crime type filter (optional)
          selectInput(
            inputId = "crime_type_boxplot",
            label = "Select Crime Type (optional):",
            choices = c("All" = "", unique(crime_data$crime_description)),
            selected = "" 
          )
        ),
        
        # Main panel for the box plot
        mainPanel(
          plotlyOutput("boxplot")
        )
      )
    ),
    
    # Tab for Case Status Distribution
    tabPanel(
      "Case Status Distribution",
      
      # Sidebar with controls for the case status plot
      sidebarLayout(
        sidebarPanel(
          # Year filter
          checkboxGroupInput(
            inputId = "year_status",
            label = "Select Years:",
            choices = unique(crime_data$year),
            selected = unique(crime_data$year)
          ),
          
          # Area filter
          selectInput(
            inputId = "area_status",
            label = "Select Area:",
            choices = c("All" = "", unique(crime_data$area_name)),  # Include "All" option
            selected = ""  # Select "All" by default
          ),
          
          # Month filter
          checkboxGroupInput(
            inputId = "month_status",
            label = "Select Months:",
            choices = unique(crime_data$month),
            selected = unique(crime_data$month)
          ),
          
          # Crime type filter (multiple selection)
          selectInput(
            inputId = "crime_type_status",
            label = "Select Crime Types:",
            choices = unique(crime_data$crime_description),
            multiple = TRUE,
            selected = NULL
          )
        ),
        
        # Main panel for the case status plot
        mainPanel(
          plotlyOutput("case_status_plot")
        )
        
      )
    )
    
  )
)
