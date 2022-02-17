
library(shiny)

data <- read.csv("C:/github/BIOL_6692_ISU/Lecture 4/RCrk_data.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    #Create a plotly output
    output$graph <- renderPlotly({
        plot_ly(data, x = ~get(input$choice), y = ~ELEV_DEM, type = 'scatter', mode = 'markers')
    })
  
    # Make a plotly from ggplot  
    output$graph2 <- renderPlotly({
        
        #Create ggplot, stored as a variable
        a_ggplot <- ggplot(data = data, aes(x = !!as.symbol(input$choice), y = ELEV_DEM)) +
            geom_point() +
            theme_minimal()
        
        # Create and execute a plotly plot using the ggplot variable created above
        ggplotly(a_ggplot)
    })
    
    # Create a leaflet map
    output$leafmap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$OpenTopoMap) %>% #add a basemap 
            addMarkers(lng = data$LONGITUDE, # Add markers from lat, long data columns
                       lat = data$LATITUDE)
    })
    
})



