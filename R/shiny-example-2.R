# install.packages('devtools')
# install.packages('dplyr')
# install.packages('leaflet')
# install.packages('maps')
# install.packages('maptools')
# install.packages('mapview')
# install.packages('rgeos')
# install.packages('shiny')
# install.packages('shinydashboard')
# devtools::install_github("tidyverse/ggplot2")

library(ggplot2)
library(leaflet)
library(shiny)
library(shinydashboard)

# Read in data and add identifier for matching county shapes
censusData <- read.csv('./GitHub/presentation-lsdg-r/R/acs2015_county_data.csv', header = T, stringsAsFactors = F)
censusData$ID <- sprintf('%s,%s', tolower(censusData$State), tolower(censusData$County))

# Load county shapes and join to data set
countyShapes <- sf::st_as_sf(maps::map('county', plot = F, fill = T))
censusData <- dplyr::inner_join(x = censusData, y = countyShapes, by = 'ID')
censusData <- sf::st_as_sf(censusData)

availableStates <- unique(censusData$State)
# availableStates <- c("All States", availableStates)
measurements <- names(censusData)
# Remove identifiers, geometries, and state/county names
measurements <- measurements[-c(1,2,3,38,39)]

generatePlot <- function(state, attribute) {
  if(state == "All States") {
    stateData <- censusData
  } else {
    stateData <- censusData[(censusData$State == state),]
  }
  colorPalette <- colorNumeric(palette = "inferno", domain = as.data.frame(stateData)[,attribute], reverse = T)
  colorFormula <- as.formula(sprintf("~colorPalette(%s)", attribute))
  labelFormula <- as.formula(sprintf("~sprintf('%%s County: %%s', County, %s)", attribute))
  l <- leaflet(stateData) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      color = "#444444",
      fillColor = colorFormula,
      fillOpacity = 0.9,
      opacity = 0.9,
      weight = 1.2,
      label = labelFormula
    ) %>%
    addLegend(pal = colorPalette, values = as.data.frame(stateData)[,attribute], title = sprintf("%s <br> %s", state, attribute), position = "bottomright")
  return(l)
}

app <- shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "US Census Data"),
    dashboardSidebar(
      selectInput("state", "Choose a state:", choices = availableStates),
      selectInput("attribute1", "Choose data for plot 1:", choices = measurements),
      selectInput("attribute2", "Choose data for plot 2:", choices = measurements)
    ),
    dashboardBody(
      leafletOutput("plot1"),
      leafletOutput("plot2")
    )
  ),
  
  server = function(input, output) {
    output$plot1 <- renderLeaflet({
      generatePlot(input$state, input$attribute1)
    })
    output$plot2 <- renderLeaflet({
      generatePlot(input$state, input$attribute2)
    })
  }
)

runApp(app)