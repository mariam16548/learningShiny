library(shiny)
ui <- fluidPage(
  fluidRow(
    tags$head(
      
      tags$style(HTML("
      
      h1 {
           display: flex;
           justify-content: center;
           align-items: center;
           height: 100px;
           width:100%
           margin-left: 150%;
       margin-right:0%;
       
          }
                    "))) ,
    headerPanel( 
      h1("Hello World!", align = "center",
         style = "color: #00b359;"))),
  
  fluidRow(
    tags$style(HTML("
    .form-group {
      display: flex;
      justify-content: center;
      align-items: center;
      margin: auto;
    }")),
    textInput("name",  
              label=h5("Enter your name:"), value="",width="50%",placeholder="Your Name") 
  ),
  
  fluidRow(
    tags$style(HTML("
            .btn {
              color: #2ecc71;
              border: 2px #2ecc71 solid;
              margin:auto;
              display:block;
            }
            .btn:hover {
                color: #fff;
                background-color: #2ecc71;
                margin:auto;
                display:block;
            }
            .btn-default.active, .btn-default:active, .open > .dropdown-toggle.btn-default {
                color: #fff;
                background-color: #2ecc71;
                border-color: #2ecc71;
                margin:auto;
                display:block; }         
        ")),
    actionButton("button", "Submit")),
  
  fluidRow(
    tags$style(HTML("
    h2 {
      display: flex;
      justify-content: center;
      align-items: center;
      margin:auto
    }")),
    h2(textOutput("value"),
       style = "color: #66cc66;")))

server <- function(input, output) {
  
  data <- eventReactive(input$button, {
    paste0("Hello,"," ", input$name, "!")
  })
  
  output$value <- renderText({
    data()
  })
}
shinyApp(ui=ui, server=server)
