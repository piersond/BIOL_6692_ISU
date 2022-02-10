# library(shiny)
# runExample("01_hello")
# 
# # Data availabe in base R
# data()

library(shiny)
# 
# # Define UI for app that draws a histogram ----
# ui <- 
fluidPage( #outer shell of the UI, setup in rows and columns
  
  # App title ----
  #titlePanel("Hello Shiny!"),
  h1("Heading 1"),
  h2("Heading 2"),
  h3("Heading 3"),
  h4("Heading 4"),
  h5("Heading 5"),
  p("plain text"),
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins", #input$bins
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      plotOutput(outputId = "distPlot2")      
    )
  )
)


