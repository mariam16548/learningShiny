library(httr) 
zipcodeToCounty<- function(zip) {
  urlCounties <- "https://raw.githubusercontent.com/scpike/us-state-county-zip/master/geo-data.csv"
  zipcodeToCountyData <- content(GET(urlCounties), type = 'text/csv')
  tbl.county <- subset(zipcodeToCountyData, zipcode == zip) 
  countyName <- tbl.county$county
  stateName <- tbl.county$state
  return(list(countyName, stateName))
}
