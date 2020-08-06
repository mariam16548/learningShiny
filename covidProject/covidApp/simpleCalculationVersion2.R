library(shiny)
library(shinyWidgets)

shinyApp(
  ui = fluidPage(
    
    titlePanel("Will your actions today propagate the virus to the community?"),
    
    div(
      id = "form",
      sliderInput("populationDensity", "What is the population density in your county (in people/square mile)?", value=0, min=0, max=1200),
      
      uiOutput("coloredBox")
    )),
  
  server = function(input, output) {
    
    displayColoredBox<- function(color, riskMessage){
      sidebarPanel(style=sprintf("background-color: %s; width: 300px; height: 300px;",
                   h3("%s"), color, riskMessage))
    }    
    
    output$coloredBox<-renderUI({
      req(input$populationDensity)
      populationDensity <- input$populationDensity;
      likelihoodOfHarm <- populationDensity/500
      
      if (likelihoodOfHarm>1) {
        displayColoredBox("red", "Extreme risk, stay home!")
        
      } else if (likelihoodOfHarm>.65){
        displayColoredBox("orange", "Very high risk, stay home!")
        
      }
      else if (likelihoodOfHarm>.35){
        displayColoredBox("yellow", "High risk, be careful!")
        
      }
      else if (likelihoodOfHarm>.10){
        displayColoredBox("blue", "Moderate risk, be careful!")
        
      } else {
        displayColoredBox("green", "Low risk, but still be careful!")
      }
      
    })
  }
)
