# server.R
library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(readr)

# Load data (make sure 'cleaned_crimedata.csv' is in the same directory)
crime_data <- read_csv("cleaned_crimedata.csv")

server <- function(input, output) {
  
  # Reactive expression for spatial map
  crime_map_data <- reactive({
    crime_data %>%
      filter(
        year %in% input$year_map,  # Use %in% for multiple year selection
        if (!is.null(input$crime_type_map)) crime_description %in% input$crime_type_map else TRUE,  # Use %in% for multiple crime type selections
        if (!is.null(input$month_map)) month %in% input$month_map else TRUE,  # Use %in% for multiple month selections
        hour_occurred %in% input$time_of_day_map 
      )
  })
  
  # Generate spatial map
  output$crime_map <- renderPlotly({
    map_data <- crime_map_data()
    
    if (nrow(map_data) == 0) {
      return(
        plot_ly() %>% 
          layout(
            title = "Please select some data to display on the map.", 
            xaxis = list(showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showticklabels = FALSE, zeroline = FALSE)
          )
      )
    } else {
      map_data <- map_data %>%
        group_by(area_name) %>%
        summarise(
          latitude = mean(latitude, na.rm = TRUE),
          longitude = mean(longitude, na.rm = TRUE),
          counts = n(),
          common_crime = names(sort(table(crime_description), decreasing = TRUE)[1]),
          .groups = 'drop' 
        )
      
      fig <- plot_ly(
        data = map_data,
        lat = ~latitude,
        lon = ~longitude,
        size = ~counts,
        color = ~counts,
        colors = colorRamp(c("blue", "red")),
        text = ~paste("Area:", area_name, "<br>Counts:", counts, "<br>Common Crime:", common_crime, "<br>Latitude:", latitude, "<br>Longitude:", longitude),
        hoverinfo = "text",
        type = 'scattermapbox',
        mode = 'markers',
        marker = list(sizeref = 0.1, sizemode = 'area'),
        height = 750,
        width = 1200
      ) %>%
        layout(
          title = "Spatial Clustering of Crime Counts by District",
          mapbox = list(
            style = "open-street-map",
            zoom = 9,
            center = list(lat = 33.922, lon = -117.9437)
          )
        )
      
      fig
    }
  })
  
  # Reactive expression for crime trend plots
  crime_trend_data <- reactive({
    crime_data %>%
      filter(
        year %in% input$year_trend,  # Use %in% for multiple year selection
        if (input$crime_type_trend != "") crime_description == input$crime_type_trend else TRUE  # Filter by crime type if selected
      )
  })
  
  # Function to generate the trend plot
  output$crime_trend_plot <- renderPlotly({
    
    # Filter data based on time aggregation
    crime_counts <- crime_trend_data() %>%
      group_by(!!sym(input$time_agg)) %>%
      summarize(Crime_Count = n(), .groups = 'drop')
    
    if (nrow(crime_counts) == 0) {
      return(
        plot_ly() %>% 
          layout(
            title = "Please select some data to display the trend.", 
            xaxis = list(showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showticklabels = FALSE, zeroline = FALSE)
          )
      )
    } else {
      # Create the plot based on selected type
      if (input$plot_type == "line") {
        fig <- plot_ly(
          data = crime_counts,
          x = ~get(input$time_agg),
          y = ~Crime_Count,
          type = 'scatter',
          mode = 'lines+markers'
        ) %>%
          layout(
            title = ifelse(input$crime_type_trend != "", paste0("Trend of ", input$crime_type_trend, " by ", input$time_agg, " in ", paste(input$year_trend, collapse = ", "), " years"),
                           paste0("Crime Trend by ", input$time_agg, " in ", paste(input$year_trend, collapse = ", "), " years")),
            xaxis = list(title = input$time_agg),
            yaxis = list(title = "Number of Crimes")
          )
      } else if (input$plot_type == "bar") {
        fig <- plot_ly(
          data = crime_counts,
          x = ~get(input$time_agg),
          y = ~Crime_Count,
          type = 'bar'
        ) %>%
          layout(
            title = ifelse(input$crime_type_trend != "", paste0("Trend of ", input$crime_type_trend, " by ", input$time_agg, " in ", paste(input$year_trend, collapse = ", "), " years"),
                           paste0("Crime Trend by ", input$time_agg, " in ", paste(input$year_trend, collapse = ", "), " years")),
            xaxis = list(title = input$time_agg),
            yaxis = list(title = "Number of Crimes")
          )
      }
      
      fig
    }
  })
  # Reactive expression for box plot data
  boxplot_data <- reactive({
    crime_data %>%
      filter(
        area_name == input$area_boxplot,
        if (input$crime_type_boxplot != "") crime_description == input$crime_type_boxplot else TRUE
      )
  })
  
  # Generate the interactive box plot
  output$boxplot <- renderPlotly({
    data <- boxplot_data()
    
    # Filter for top weapons (excluding "Unknown")
    top_weapons <- data %>%
      filter(!is.na(weapon_description) & !(weapon_description %in% "Unknown")) %>%
      count(weapon_description) %>%
      top_n(5, n) %>% 
      pull(weapon_description)
    
    # Filter data for top weapons and selected area
    filtered_data <- data %>%
      filter(weapon_description %in% top_weapons)
    
    if (nrow(filtered_data) == 0) {
      return(
        plot_ly() %>% 
          layout(
            title = "Please select some data to display the box plot.", 
            xaxis = list(showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showticklabels = FALSE, zeroline = FALSE)
          )
      )
    } else {
      # Create the box plot
      fig <- plot_ly(
        data = filtered_data,
        x = ~weapon_description,
        y = ~victim_age,
        type = 'box',
        boxpoints = 'all',  # Show all points
        jitter = 0.3,  # Add some jitter to points
        pointpos = 0,  # Adjust point position
        marker = list(color = 'red', size = 4),
        name = ~weapon_description
      ) %>%
        layout(
          title = paste0("Distribution of Victim Age by Weapon Used (", input$area_boxplot, " Area)"),
          xaxis = list(title = "Weapon Used"),
          yaxis = list(title = "Victim Age")
        )
      
      fig
    }
  })
  # Reactive expression for case status plot data
  case_status_data <- reactive({
    crime_data %>%
      filter(
        year %in% input$year_status,
        if (input$area_status != "") area_name == input$area_status else TRUE,
        month %in% input$month_status,
        if (!is.null(input$crime_type_status)) crime_description %in% input$crime_type_status else TRUE
      ) %>%
      # Filter for top 6 most frequent crime types
      filter(crime_description %in% unique(crime_data %>% count(crime_description) %>% top_n(6, n) %>% pull(crime_description))) %>%
      group_by(crime_description, status_description) %>%
      summarize(count = n(), .groups = 'drop') %>%
      arrange(crime_description, desc(count)) %>%
      mutate(status_description = factor(status_description, levels = unique(status_description)))
  })
  
  
  # Generate the interactive case status plot
  output$case_status_plot <- renderPlotly({
    data <- case_status_data()
    
    if (nrow(data) == 0) {
      return(
        plot_ly() %>% 
          layout(
            title = "Please select some data to display the case status plot.", 
            xaxis = list(showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showticklabels = FALSE, zeroline = FALSE)
          )
      )
    } else {
      # Create the faceted bar plot
      fig <- plot_ly(
        data = data,
        x = ~status_description,
        y = ~count,
        color = ~crime_description,  # Color by crime description
        type = 'bar',
        name = ~crime_description
      ) %>%
        layout(
          title = paste0("Distribution of Case Statuses Across Top 6 Crime Types in ",
                         ifelse(input$area_status != "", paste0(input$area_status, " Area"), "All Areas"),
                         " during ", paste(input$month_status, collapse = ", "),
                         ifelse(length(input$year_status) > 1, 
                                paste("in", paste(input$year_status, collapse = ", ")), 
                                paste("in", input$year_status))
          ),
          xaxis = list(title = "Status Description"),
          yaxis = list(title = "Count"),
          legend = list(title = list(text = "Crime Description"))  # Add legend title
        ) %>%
        config(displayModeBar = FALSE)  # Hide the mode bar
      
      fig
    }
  })
  
  # Initial Display: Top Crime
  output$initial_plot <- renderPlotly({
    top_crime <- crime_data %>%
      count(crime_description) %>%
      top_n(1, n) %>% 
      pull(crime_description)
    
    top_crime_data <- crime_data %>%
      filter(crime_description == top_crime) %>%
      group_by(crime_description) %>%
      summarise(Count = n())
    
    plot_ly(
      data = top_crime_data,
      x = ~crime_description,
      y = ~Count,
      type = "bar",
      color = ~crime_description
    ) %>%
      layout(
        title = "Most Frequent Crime in Los Angeles",
        xaxis = list(title = "Crime Description"),
        yaxis = list(title = "Count")
      ) %>%
      config(displayModeBar = FALSE) 
  })
  
  # Call renderPlotly for the initial plot when the app starts
  output$initial_plot <- renderPlotly({
    
  })
  
}