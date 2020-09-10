library(shiny)
library(RUnit)

source("countyLevelInfectionData.R")

displayColoredBox<- function(color, riskMessage){
  sidebarPanel(style=sprintf("background-color: %s; width: 550px; height: 120px;", color),
               h3(sprintf("%s", riskMessage), align = "center") )  
}
app <- shinyApp(
  ui = fluidPage(
    titlePanel("Will your actions today propagate the virus to the community?"),
    
    sidebarLayout(position = "left",
                  sidebarPanel("",
                               numericInput("zipcode", "Please enter your zipcode.", value = 66101, min= NA, max=NA)
                  ),
                  
                  mainPanel("",
                            fluidRow(
                              uiOutput("coloredBox"), 
                              plotOutput("histogramOfCases", width="100%", height="500px"))
                  )
    )),
  
  server <- function(input, output, session) {
    getInfectionData <- reactive({
      req(input$"zipcode")
      
      zipcode <- input$zipcode
      
      infectionData <- data.frame(countyLevelInfectionData(zipcode))
      infectionData
    })
    
    getRiskAndColor<-reactive({ 
      req(input$"zipcode")
      zipcode <- input$zipcode
      
      infectionData <- data.frame(countyLevelInfectionData(zipcode))
      countyPopulatonDensity<-infectionData[1, 3] #extract the county population density
      
      mostRecentCaseCount<- tail(infectionData$caseCount,1)
      likelihoodOfHarm <- (countyPopulatonDensity*mostRecentCaseCount)/10000000
      
      if (likelihoodOfHarm>1) {
        color<-"red"
        riskMessage<-"Extreme risk, stay home!"
        
      } else if (likelihoodOfHarm>.65){
        color<-"orange"
        riskMessage<-"Very high risk, stay home!"
      }
      else if (likelihoodOfHarm>.35){
        color<-"yellow"
        riskMessage<-"High risk, be careful!"
      }
      else if (likelihoodOfHarm>.10){
        color<-"#4d94ff" #this shade of blue isn't too dark
        riskMessage<-"Moderate risk, be careful!"
      } else {
        color<-"#bfff80" #this shade of green isn't too dark
        riskMessage<-"Low risk, but still be careful!"
      }
      list(color=color, riskMessage=riskMessage, likelihoodOfHarm=likelihoodOfHarm) 
      # making these variables the result from the getRiskAndColor() function as global variables, not local ones
      
    })
    
    output$coloredBox<-renderUI({
      riskAndColor<-getRiskAndColor() #the list/result of the function goes into a variable called riskAndColor
      displayColoredBox(riskAndColor$color, riskAndColor$riskMessage) #extract certain elements from the list to plug into the displayColoredBox() function
      #the output will be the output of the displayColorBox() function
      
    })
    
    output$histogramOfCases <- renderPlot({
      infectionData<-getInfectionData() 
      plot(1:nrow(infectionData), infectionData$caseCount, main="COVID-19 Case Count in Your Area", 
           xlab="Days since 01/22/2020", ylab="Infection Count", cex.main=1.5, cex.lab=1.3, cex.axis=1.2, col="red", "h")
    })
    
  }
)

testServer(app, {
  session$setInputs(zipcode = 66101) #set the zipcode as 66101
  riskAndColor<-getRiskAndColor() #the list/result of the function goes into a variable called riskAndColor
  checkTrue(riskAndColor$likelihoodOfHarm> 0.65)  #extract certain element from the list to check its value
  cat("Correct likelihood of harm value!\n") #if previous line is true, paste the given message 
  checkEquals(riskAndColor$riskMessage, "Very high risk, stay home!") #extract certain element from the list to check its value
  cat("Correct risk message!\n") #if previous line is true, paste the given message 
  checkEquals(riskAndColor$color, "orange") #extract certain element from the list to check its value
  cat("Correct color!\n") #if previous line is true, paste the given message 
})
