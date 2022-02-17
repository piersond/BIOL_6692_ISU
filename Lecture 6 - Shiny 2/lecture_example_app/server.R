
data()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        plot_data <- faithful %>% filter(waiting < input$data_max)
        
        # generate bins based on input$bins from ui.R
        x    <- plot_data[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    
    # OBSERVE ONLY 1 INPUT
    observeEvent(input$filterDT, {

        table_data <- faithful %>% filter(waiting < input$data_max)
        
        output$table1 <- renderDataTable({
            table_data
        })
        
    })
    
    # OBSERVE ALL INPUTS
    # observe({
    #     input$filterDT
    #     
    #     table_data <- faithful %>% filter(waiting < input$data_max)
    #     
    #     output$table1 <- renderDataTable({
    #         table_data
    #     })
    #     
    # })

})
