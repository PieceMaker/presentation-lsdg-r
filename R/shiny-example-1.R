# install.packages('devtools')
# install.packages('dplyr')
# install.packages('maps')
# install.packages('maptools')
# install.packages('mapview')
# install.packages('rgeos')
# install.packages('shiny')
# devtools::install_github("tidyverse/ggplot2")

library(ggplot2)
library(shiny)

# Read in data and add identifier for matching county shapes
censusData <- read.csv('./Documents/GitHub/presentation-lsdg-r/R/acs2015_county_data.csv', header = T, stringsAsFactors = F)
censusData$ID <- sprintf('%s,%s', tolower(censusData$State), tolower(censusData$County))

# Load county shapes and join to data set
countyShapes <- sf::st_as_sf(maps::map('county', plot = F, fill = T))
censusData <- dplyr::inner_join(x = censusData, y = countyShapes, by = 'ID')
censusData <- sf::st_as_sf(censusData)

availableStates <- unique(censusData$State)
# availableStates <- c("All States", availableStates) # Too much for my laptop
measurements <- names(censusData)
# Remove identifiers, geometries, and state/county names
measurements <- measurements[-c(1,2,3,38,39)]

app <- shinyApp(
  ui = fluidPage(
    titlePanel("US Census Data"),
    sidebarLayout(
      sidebarPanel(
        selectInput("state", "Choose a state:", choices = availableStates),
        selectInput("attribute", "Choose data to display:", choices = measurements)#,
        #textInput("low", label = "Low Color", value = "orange"),
        #textInput("high", label = "High Color", value = "blue")
      ),
      mainPanel(
        plotOutput("statePlot")
      )
    )
  ),
  
  server = function(input, output) {
    output$statePlot <- renderPlot({
      if(input$state == "All States") {
        stateData <- censusData
      } else {
        stateData <- censusData[(censusData$State == input$state),]
      }
      ggplot(stateData) +
        geom_sf(aes_string(fill = input$attribute)) #+
        #scale_fill_gradient(low = input$low, high = input$high)
    })
  }
)

runApp(app)