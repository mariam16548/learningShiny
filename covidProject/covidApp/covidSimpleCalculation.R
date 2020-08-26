library(shiny)
library(RUnit)

displayColoredBox<- function(color, riskMessage){
  sidebarPanel(style=sprintf("background-color: %s; width: 300px; height: 300px;", color),
               h3(sprintf("%s", riskMessage)) )  }
app <-shinyApp(
  ui = fluidPage(
    
    titlePanel("Will your actions today propagate the virus to the community?"),
    
    div(
      id = "form",
      sliderInput("populationDensity", "What is the population density in your county (in people/square mile)?", value=0, min=0, max=5000),
      sliderInput("caseCount", "What is the approximate number of cases per 100,000 people in your county?", value=0, min=0, max=5000),
      uiOutput("coloredBox")
    )),
  
  server = function(input, output, session) {
    
    output$coloredBox<-renderUI({
      req(input$populationDensity)
      req(input$caseCount)
      
      populationDensity <- input$populationDensity;
      caseCount <- input$caseCount;
      
      likelihoodOfHarm <- (populationDensity*caseCount)/5000000
      
      
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
      
      coloredBox=displayColoredBox(color, riskMessage)
      return(coloredBox)      
      
    })
  }
)

testServer(app, {
  session$setInputs(populationDensity = 1500)
  session$setInputs(caseCount = 2500)
  checkEqualsNumeric(output$coloredBox$likelihoodOfHarm, 0.75)
  checkEquals(output$coloredBox$riskMessage, "Very high risk, stay home!")
  checkEquals(output$coloredBox$color,"orange")
})

#reference used: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
