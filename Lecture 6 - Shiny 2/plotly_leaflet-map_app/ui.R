library(shiny)
library(plotly)
library(leaflet)
library(dplyr)

data <- read.csv("C:/github/BIOL_6692_ISU/Lecture 4/RCrk_data.csv")

ui <- fluidPage(
    fluidRow(
        column(2,
               selectInput("choice", "Choose variable:", choices = names(data[,5:11]), selected = names(data)[5])),
        column(8,
               plotlyOutput("graph")
        )
    ),
    plotlyOutput("graph2"), # load a plotly plot
    br(), # add vertical space between UI elements
    h2("Plot Map"), # add heading, size class 2
    hr(), # add horizontal line spacer to UI
    leafletOutput("leafmap", height=1000), # load a leaflet map
)