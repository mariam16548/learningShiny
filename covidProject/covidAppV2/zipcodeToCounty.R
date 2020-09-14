library(httr) 
urlCounties <- "https://raw.githubusercontent.com/scpike/us-state-county-zip/master/geo-data.csv"
zipcodeToCountyData <- content(GET(urlCounties), type = 'text/csv')

zipcodeToCounty<- function(zip) {
  tbl.county <- subset(zipcodeToCountyData, zipcode == zip) 
  countyName <- tbl.county$county
  stateName <- tbl.county$state
  return(list(countyName, stateName))
}
