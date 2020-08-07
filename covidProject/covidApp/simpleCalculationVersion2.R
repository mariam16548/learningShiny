displayColoredBox<- function(color, riskMessage){
  sidebarPanel(style=sprintf("background-color: %s; width: 300px; height: 300px;", color),
               h3(sprintf("%s", riskMessage)) )  }
shinyApp(
  ui = fluidPage(
    
    titlePanel("Will your actions today propagate the virus to the community?"),
    
    div(
      id = "form",
      sliderInput("populationDensity", "What is the population density in your county (in people/square mile)?", value=0, min=0, max=1200),
      
      uiOutput("coloredBox")
    )),
  
  server = function(input, output) {
    
    output$coloredBox<-renderUI({
      req(input$populationDensity)
      populationDensity <- input$populationDensity;
      likelihoodOfHarm <- populationDensity/500
      
      
      if (likelihoodOfHarm>1) {
        color="red"
        riskMessage="Extreme risk, stay home!"

      } else if (likelihoodOfHarm>.65){
        color="orange"
        riskMessage="Very high risk, stay home!"
      }
      else if (likelihoodOfHarm>.35){
        color="yellow"
        riskMessage="High risk, be careful!"
      }
      else if (likelihoodOfHarm>.10){
        color="blue"
        riskMessage="Moderate risk, be careful!"
      } else {
        color="green"
        riskMessage="Low risk, but still be careful!"
      }
      
      displayColoredBox(color, riskMessage)
      
    })
  }
)
