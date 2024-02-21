
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("shinyWidgets")) {
  install.packages("shinyWidgets")
  library(shinyWidgets)
}
if (!require("shinythemes")) {
  
  install.packages("shinythemes")
  library(shinythemes)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}
if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Define UI for application that draws a histogram
shinyUI(
    navbarPage(strong("Disaster Declarations Study",style="color: gray;"), 
               theme=shinytheme("paper"), # select your themes https://rstudio.github.io/shinythemes/
               #------------------------------- Tab Panel - Introduction ---------------------------------
               tabPanel("Introduction",
                        icon = icon("info-circle"),
                        div(style = "background-image: url('https://external-preview.redd.it/d6tC7-hOyuC__4Wy_5KcODi0fzEYwrnyvZSbD_wSXCs.jpg?width=1080&crop=smart&auto=webp&s=7b2356ef0a6a741aa41fc30fbe5531aedb1ea53d'); background-size: cover; height: 800px;",
                            verticalLayout(
                              fluidRow(
                                column(12,
                                       align = "center",
                                       wellPanel(
                                         style = "max-width: 600px; margin: 50px; background-color: rgba(255, 255, 255, 0.8);",  # Semi-transparent background for the text
                                         p("This interactive dashboard explores and analyzes FEMA disaster declarations across the United States. The map visually represents declarations by location, while analytical tools enable users to dissect the data by state, incident type, year, and other filters. By revealing trends and patterns in federally declared disasters, this platform aims to inform preparation and response strategies.")
                                       )
                                )
                              )
                            )
                        )
               ),
               
               #------------------------------- Tab Panel - Geographical Map ---------------------------------
               tabPanel("Geographical Map",
                        icon = icon("map-marker-alt"),
                        
                        fluidRow(
                          column(width = 3,
                                 wellPanel(
                                   dateRangeInput("yearRange", "Year Range", start = "1953-01-01", end = Sys.Date()),
                                   selectInput("state", "State", choices = c("All", "State1", "State2"), selected = "All"),
                                   selectInput("incidentType", "Incident Type", choices = c("All", "Fire", "Flood"), selected = "All"),
                                   
                                 )
                          ),
                          column(width = 9,
                                 leafletOutput("map", height = "800px")
                          )
                        )
               ),
               # ------------------------------- Data Analysis Tab Panels ---------------------------------
               tabPanel("Data Analysis",
                        icon = icon("chart-simple"),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("stateInput", "Select State:", choices = NULL),
                            sliderInput("yearRange2", "Select Year Range:",
                                        min = 1953, max = 2024, value = c(1953, 2024), # Adjust these values based on your data
                                        step = 1),
                            selectInput("incidentTypeInput", "Select Incident Type:", choices = NULL),
                          ),
                          mainPanel(
                            tabsetPanel(id = "tabs_active",
                                        tabPanel("Incident Type Distribution", plotlyOutput("incidentTypePlot"), value = "incidentTypeDistribution"),
                                        tabPanel("Trend Over Time", plotlyOutput("timeSeriesPlot"), value = "trendOverTime"),
                                        tabPanel("Summary Statistics", verbatimTextOutput("summaryStats"), value = "summaryStatistics")
                            )
                          )
                        )
               ),
               #------------------------------- Tab Panel - Reference ---------------------------------
               tabPanel("Reference",
                        icon = icon("book"),
                        verticalLayout(
                          fluidRow(
                            column(12,
                                   align = "center",
                                   wellPanel(
                                     style = "max-width: 600px; margin: auto;",
                                     div(style = "display: inline; margin-right: 5px;", "Disaster Dataset:"),
                                     a(href = "https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2",
                                       "FEMA Disaster Declarations Summaries Dataset", target = "_blank", style = "display: inline;"),
                                     div(),
                                     div(style = "display: inline; margin-right: 5px;", "States Shapefile:"),
                                     a(href = "https://hub.arcgis.com/datasets/1b02c87f62d24508970dc1a6df80c98e/explore?location=35.436585%2C-97.188480%2C5.00",
                                       "States Shapefile", target = "_blank", style = "display: inline;"),
                                   )
                            )
                          )
                        )
               ),
               


    ) #navbarPage closing  
) #Shiny UI closing    
