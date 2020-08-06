library(shiny)
library(shinyWidgets)

shinyApp(
  ui = fluidPage(
    
    titlePanel("Will your actions today propagate the virus to the community?"),
    
    div(
      id = "form",
      sliderInput("popDensity", "What is the population density in your county (in people/square mile)?", value=0, min=0, max=1200),

      
      tags$style(HTML("
            .btn {
              color: #2ecc71;
              border: 2px #2ecc71 solid;
              margin:1%;
              display:block;
            }
        ")),
      actionButton("button", "Evaluate", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      uiOutput("calculation")

    )),
 
  server = function(input, output) {

      observeEvent(input$button, {
      likelihoodOfHarm<-(1/500)*input$popDensity
        
      output$calculation<-renderUI({
      if (likelihoodOfHarm>1) {
        sidebarPanel(style="background-color: red; width: 300px; height: 300px;", h3("Extreme risk, stay home!"))
        
        } else if (likelihoodOfHarm>.65){
          sidebarPanel(style="background-color: orange; width: 300px; height: 300px;", 
                       h3("Very high risk, stay home!"))
        }
        else if (likelihoodOfHarm>.35){
          sidebarPanel(style="background-color: yellow; width: 300px; height: 300px;", 
                       h3("High risk, be careful!"))
        }
        else if (likelihoodOfHarm>.10){
          sidebarPanel(style="background-color: blue; width: 300px; height: 300px;", 
                       h3("Moderate risk, be careful!"))
          
        } else {
         sidebarPanel(style="background-color: #39ac39; width: 300px; height: 300px;", 
                      h3("Low risk, but still be careful!"))
       }

    })
      }) 
           
  }
)
