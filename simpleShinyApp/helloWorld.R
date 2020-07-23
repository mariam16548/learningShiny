library(shiny)

shinyApp( 
  ui <- fluidPage( 
    fluidRow( 
      column( 
        3, textInput("name", 
                     label="Please Enter Your Name"), 
        actionButton("button", "Submit")), 
      
      column( 
        9, textOutput("value")))
  ), 
  
  server <- function(input, output) {
    
    data <- eventReactive(input$button, {
      paste("Hello", input$name)
    })
    
    output$value <- renderText({
      data()
    })
  })
