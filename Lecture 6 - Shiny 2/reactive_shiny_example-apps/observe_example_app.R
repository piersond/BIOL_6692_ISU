library(shiny)

ui <- fluidPage(
  headerPanel("Example reactive"),
  
  mainPanel(
    
    # action buttons
    actionButton("button1","Button 1"),
    actionButton("button2","Button 2")
  )
)

server <- function(input, output) {
  
  # observe button 1 press.
  observe({
    # What to observe
    c(input$button1, input$button2)
    
    # What to do when an observed input changes
    showModal(modalDialog(
      title = "Button pressed",
      "You pressed one of the buttons!"
    ))
  })
}

shinyApp(ui = ui, server = server)