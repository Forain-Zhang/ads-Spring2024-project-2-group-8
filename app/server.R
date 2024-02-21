#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
###############################Install Related Packages #######################
if (!require("shiny")) {
    install.packages("shiny")
    library(shiny)
}
if (!require("leaflet")) {
    install.packages("leaflet")
    library(leaflet)
}
if (!require("leaflet.extras")) {
    install.packages("leaflet.extras")
    library(leaflet.extras)
}
if (!require("dplyr")) {
    install.packages("dplyr")
    library(dplyr)
}
if (!require("magrittr")) {
    install.packages("magrittr")
    library(magrittr)
}
if (!require("mapview")) {
    install.packages("mapview")
    library(mapview)
}
if (!require("leafsync")) {
    install.packages("leafsync")
    library(leafsync)
}
if (!require("sf")) {
  install.packages("sf")
  library(sf)
}
if (!require("tidyr")) {
  install.packages("tidyr")
  library(tidyr)
}

# Read and preprocess the data
states_shapes <- st_read("States_shapefile.shp")
disaster_data <- read.csv('DisasterDeclarationsSummaries.csv')
disaster_data<- disaster_data%>%
  mutate(across(everything(), ~ifelse(is.na(.), "Unknown", .)))
disaster_data$declarationDate <- as.Date(disaster_data$declarationDate, format = "%Y-%m-%d")
disaster_data$fyDeclared <- as.numeric(as.character(disaster_data$fyDeclared))
disaster_data<- disaster_data%>%
  filter(fyDeclared >= 1953)

# Define server logic
shinyServer(function(input, output, session) {
  # Update the choices for state selector
  observe({
    state_choices <- unique(states_shapes$State_Code)
    updateSelectInput(session, "state", choices = c("All", state_choices), selected="All")
  })
  
  # Update the choices for incident type selector
  observe({
    incident_type_choices <- unique(disaster_data$declarationType)
    updateSelectInput(session, "incidentType", choices = c("All", incident_type_choices), selected="All")
  })
   
  # Set the range for the date selector
  observe({
    min_date <- min(disaster_data$declarationDate, na.rm = TRUE)
    max_date <- max(disaster_data$declarationDate, na.rm = TRUE)
    updateDateRangeInput(session, "yearRange", start = min_date, end = max_date)
  })
  
  observe({
    # Update stateInput choices
    updateSelectInput(session, "stateInput", choices = unique(disaster_data$state))
    # Update incidentTypeInput choices
    updateSelectInput(session, "incidentTypeInput", choices = c("All", unique(disaster_data$incidentType)))
    updateSliderInput(session, "yearRange2",  min = min(disaster_data$fyDeclared, na.rm = TRUE), max = max(disaster_data$fyDeclared, na.rm = TRUE), value=min(disaster_data$fyDeclared, na.rm = TRUE))
  })
  
  output$map <- renderLeaflet({
    # Filter data
    data <- disaster_data
    temp_states <- states_shapes[, c("FID", "State_Code")]
    data <- merge(data, temp_states, by.x = "fipsStateCode", by.y = "FID", all.x = TRUE)
    
    if (input$state != "All") {
      data <- data[data$State_Code == input$state, ]
    } 
    if (input$incidentType != "All") {
      data <- data[data$declarationType %in% input$incidentType, ]
    }
    data <- data[data$declarationDate >= input$yearRange[1] & data$declarationDate <= input$yearRange[2], ]
    
    # Aggregate data by fipsStateCode and declarationType
    data_agg <- data %>%
      group_by(fipsStateCode, declarationType) %>%
      summarise(count = n(), .groups = "drop")
    
    # Spread the counts by declarationType
    data_agg_spread <- data_agg %>%
      pivot_wider(names_from = declarationType, values_from = count, values_fill = list(count = 0))
    
    # Additional aggregation for total counts
    total_agg <- data %>%
      group_by(fipsStateCode) %>%
      summarise(total_count = n(), .groups = "drop")
    
    # Join aggregated data with states_shapes
    states_data_total <- merge(states_shapes, data_agg_spread, by.x = "FID", by.y = "fipsStateCode", all.x = TRUE)
    states_data_total <- merge(states_data_total, total_agg, by.x = "FID", by.y = "fipsStateCode", all.x = TRUE)
    states_data_total$total_count[is.na(states_data_total$total_count)] <- 0
    for (col in c("DR", "EM", "FM")) {
      if (!col %in% names(states_data_total)) {
        states_data_total[[col]] <- NA
      }
    }
    states_data_total$DR[is.na(states_data_total$DR)] <- 0
    states_data_total$EM[is.na(states_data_total$EM)] <- 0
    states_data_total$FM[is.na(states_data_total$FM)] <- 0
    if (all(unique(states_data_total$total_count) == 0)) {
      # If all values are zero, use a single gray color for the palette
      palette <- function(x) "#808080"
    } else {
      # Otherwise, use colorQuantile as before
      palette <- colorQuantile("YlOrRd", unique(states_data_total$total_count), n = 5)
    }
    # Create the map
    map <- leaflet(states_data_total) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~palette(total_count),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~{
            if (input$incidentType == "All") {
              paste0(State_Name, "(", State_Code, ")", "<br/>Total Incidents: ", total_count,
                     "<br/>FM: ", FM, 
                     "<br/>DR: ", DR, 
                     "<br/>EM: ", EM)
            } else {
              paste0(State_Name, "(", State_Code, ")", "<br/>", input$incidentType, ": ", get(input$incidentType))
            }
        } %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "10px", direction = "auto")
      )
    if (length(unique(palette(states_data_total$total_count))) > 1) {
    map <- map %>% addLegend(
      position = "bottomright",
      pal = palette,
      values = ~total_count,
      title = "Incident Counts",
      opacity = 1
    )}
    
    map
  })
  
  filteredData <- reactive({
    filtered <- disaster_data%>%
      filter(state == input$stateInput)
    
    if (input$incidentTypeInput != "All") {
      filtered <- filtered %>%
        filter(incidentType == input$incidentTypeInput)
    }
    
    # Filter based on the selected year range
    yearRange <- input$yearRange
    filtered <- filtered %>%
      filter(fyDeclared >= input$yearRange2[1] & fyDeclared <= input$yearRange2[2])
    return(filtered)
  })
  
  output$incidentTypePlot <- renderPlotly({
    gg <- ggplot(filteredData(), aes(x = incidentType, fill = incidentType)) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Number of Disasters by Incident Type", x = "Incident Type", y = "Number of Disasters") +
      scale_fill_brewer(palette = "Set3")
    ggplotly(gg)
  })
  
  output$timeSeriesPlot <- renderPlotly({
    gg <- ggplot(filteredData(), aes(x = fyDeclared, group = incidentType, color = incidentType)) +
      geom_line(stat = "count") +
      labs(title = "Disaster Declarations Over Time", x = "Fiscal Year", y = "Number of Disasters") +
      scale_color_brewer(palette = "Dark2")
    ggplotly(gg)
  })
  
  output$summaryStats <- renderPrint({
    filtered <- filteredData()
    summaryStats <- filtered %>%
      group_by(incidentType) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
    print(summaryStats)
  })
})