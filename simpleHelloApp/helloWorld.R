library(shiny)
shinyApp( 
  ui <- fluidPage(
    tags$head(
      tags$style("*{font-family: BentonSans Book;}")
    ),
    titlePanel(
      h1("Hello World!",
         style = "font-family: Times New Roman, font-weight: 500; color: #00b359;")),

    textInput("name", 
                     label=h5("Please Enter Your Name"), placeholder="Your Name"),
        tags$style(HTML("
            .btn {
              color: #2ecc71;
              border: 2px #2ecc71 solid;
            }
            .btn:hover {
                color: #fff;
                background-color: #2ecc71;
            }
            .btn-default.active, .btn-default:active, .open > .dropdown-toggle.btn-default {
                color: #fff;
                background-color: #2ecc71;
                border-color: #2ecc71;
            }
        ")),
        actionButton("button", "Submit"),
      column( 
        9, h3(textOutput("value"), style = "color: #00b359;"))),

  server <- function(input, output) {
    
    data <- eventReactive(input$button, {
      paste0("Hello, ", input$name, "!")
    })
    
    output$value <- renderText({
      data()
    })
  })
shinyApp(ui, server)
