library(shiny)
library(RUnit)

source("countyLevelInfectionData.R")

displayColoredBox<- function(color, riskMessage){
  sidebarPanel(style=sprintf("background-color: %s; width: 550px; height: 120px;", color),
               h3(sprintf("%s", riskMessage), align = "center") )  
}
app <- shinyApp(
  ui <- fluidPage(
    titlePanel("Will your actions today likely propagate COVID-19 to your community?"),
    
    sidebarLayout(position = "left",
                  sidebarPanel("",
                               textInput("zipcode", label="Enter your zipcode.", value = 66101), 
                               numericInput("age", label="Enter your age.", value = ""),
                               numericInput("groupSize", label="How many people will be with you?", value = ""),
                               selectInput("masking", "Will you wear a mask?",
                                           c("","Yes", "No")),
                               selectInput("alcoholConsumption", "Will you be under the influence of alcohol outside of your home?",
                                           c("","Yes", "No"))),
                  
                  mainPanel("",
                            fluidRow(
                              uiOutput("coloredBox"), 
                              plotOutput("histogramOfCases", width="100%", height="500px"))
                  ))),
  
  server <- function(input, output, session) {
    
    observeEvent(input$zipcode,{     #limits zipcode input to 5 numbers only
      if(nchar(input$zipcode)!=5)
      {
        updateTextInput(session,'zipcode',value=66101)
        showModal(modalDialog(
          title = "Error!",
          "Only 5-character entries are permitted.",
          easyClose = TRUE
        ))
      }
      if(is.na(as.numeric(input$zipcode)))
      {
        showModal(modalDialog(
          title = "Error!",
          "Only numeric values are allowed. Please try again.",
          easyClose = TRUE
        ))
      }
    }
    )
    
    
    getInfectionData <- reactive({
      req(input$zipcode)
      
      zipcode <- input$zipcode
      
      infectionData <- data.frame(countyLevelInfectionData(zipcode))
      infectionData
    })
    
    getRiskAndColor<-reactive({ 
      req(input$zipcode)
      req(input$age)
      req(input$masking)
      req(input$groupSize)
      req(input$alcoholConsumption)
      
      zipcode <- input$zipcode
      age <- input$age
      masking <- input$masking
      groupSize <- input$groupSize
      alcoholConsumption <- input$alcoholConsumption
      
      
      infectionData <- data.frame(countyLevelInfectionData(zipcode))
      countyPopulatonDensity<-infectionData[1, 3] #extract the county population density
      
      mostRecentCaseCount<- tail(infectionData$caseCount,1)
      likelihoodOfHarm <- (countyPopulatonDensity*mostRecentCaseCount)/100000000
      
      if (masking=="Yes") {                    #decreased risk because more precautions are being taken
        likelihoodOfHarm<- likelihoodOfHarm*.35
      }  else {
        likelihoodOfHarm <- likelihoodOfHarm
      }
      
      if (age<30){
        likelihoodOfHarm <- likelihoodOfHarm*1.31     #increased risk because more likely to exhibit risky behavior
      }  else {
        likelihoodOfHarm <- likelihoodOfHarm
      }
      
      if (groupSize>10){
        likelihoodOfHarm <- likelihoodOfHarm*1.3 #increased risk because people around in close proximity
      } else if (groupSize>25){
        likelihoodOfHarm <- likelihoodOfHarm*1.6 #increased risk because more people around in close proximity
      } else if (groupSize>50){
        likelihoodOfHarm <- likelihoodOfHarm*1.9 #increased risk because more people around in close proximity
      }  else {
        likelihoodOfHarm <- likelihoodOfHarm
      }
      
      if (alcoholConsumption=="Yes"){
        likelihoodOfHarm <- likelihoodOfHarm*1.66 #increased risk because lack of awareness
      }  else {
        likelihoodOfHarm <- likelihoodOfHarm
      }
      
      
      
      #designing the coloredBox
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
  session$setInputs(age = 20) #set the age as 20
  session$setInputs(masking = "Yes") #set input as wearing a mask
  session$setInputs(groupSize = 1) #set the group size as 1 
  session$setInputs(alcoholConsumption = "No")  #set input as no alcohol consumption
  
  riskAndColor<-getRiskAndColor() #the list/result of the function goes into a variable called riskAndColor
  checkTrue(riskAndColor$likelihoodOfHarm< 0.65)  #extract certain element from the list to check its value
  cat("Correct likelihood of harm value!\n") #if previous line is true, paste the given message 
  checkEquals(riskAndColor$riskMessage, "Low risk, but still be careful!") #extract certain element from the list to check its value
  cat("Correct risk message!\n") #if previous line is true, paste the given message 
  checkEquals(riskAndColor$color, "#bfff80") #extract certain element from the list to check its value
  cat("Correct color!\n") #if previous line is true, paste the given message 
})

#source for mask lowering risk calculation: 
#https://www.ucdavis.edu/coronavirus/news/your-mask-cuts-own-risk-65-percent/

#source for alcohol causing lowered awareness and increasing risk calculation: --- .08 BAC results in avg of 66% impairment
#https://one.nhtsa.gov/people/injury/research/pub/HS809028/index_.htm

#source for younger people more likely to exhibit risky behavior: 
#https://komonews.com/news/coronavirus/troubling-trend-shows-more-young-people-testing-positive-for-covid-19
#https://www.usatoday.com/story/news/politics/2020/06/26/coronavirus-young-americans-less-likely-social-distance/3257513001/
#45% of people under 30 are socializing with people they do not live with and not social distancing
#around 40% of COVID cases are from people under 30
#therefore, around 16% of young people (likely asymptomatic) are engaging in risky behavior
#82% risk reduction when social distancing (https://newsroom.unsw.edu.au/news/health/covid-19-how-much-do-social-distancing-and-masks-reduce-risk)
#1-(.82*(1-.16))=.31-> 31% risk increase
