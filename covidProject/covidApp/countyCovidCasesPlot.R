library(httr) 
library(RUnit)
#------------------------------------------------------------------------------------------------------------------------
urlCounties <- "https://raw.githubusercontent.com/scpike/us-state-county-zip/master/geo-data.csv"
zipcodeToCountyData <- content(GET(urlCounties), type = 'text/csv')
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
  test_countyCovidCasesPlot()
} # runTests

#------------------------------------------------------------------------------------------------------------------------
countyCovidCasesPlot<- function(zip) {
  tbl.county <- subset(zipcodeToCountyData, zipcode == zip) 
  countyName <- tbl.county$county
  stateName <- tbl.county$state
  stateAbbreviation <- tbl.county$state_abbr
  urlCovidCountyStats <- paste0("https://knowi.com/api/data/ipE4xJhLBkn8H8jisFisAdHKvep",
                "FR5I4bGzRySZ2aaXlJgie?entityName=County%207%20day%20growth",
                "%20rates&exportFormat=csv&c9SqlFilter=select%20*%20where",
                "%20County%20like%20", 
                gsub(" " , "%20", countyName), 
                "%20County%20AND%20State%20like%20", 
                gsub(" " , "%20", stateName))
  tbl.countyStats <- content(GET(urlCovidCountyStats), type = 'text/csv')
  tbl.countyCases <- subset(tbl.countyStats, Type!="Deaths") #only has cases not death stats
  tbl.countyCases <- tbl.countyCases[seq(dim(tbl.countyCases)[1],1),] #flip row order 
  barplot(tbl.countyCases$"7_day_count", 
          main=sprintf("Weekly COVID-19 Case Count in %s County, %s", countyName, stateAbbreviation), 
          xlab="Weeks since 1/22/20", ylab="Weekly Infection Count")
}

#------------------------------------------------------------------------------------------------------------------------
test_countyCovidCasesPlot <- function()
{
  message(sprintf("--- test_countyCovidCasesPlot"))
  
  tbl.county <- subset(zipcodeToCountyData, zipcode == 98125) 
  countyName<- tbl.county$county
  stateName<- tbl.county$state
  stateAbbreviation <- tbl.county$state_abbr
  urlCovidCountyStats <- paste0("https://knowi.com/api/data/ipE4xJhLBkn8H8jisFisAdHKvep",
                "FR5I4bGzRySZ2aaXlJgie?entityName=County%207%20day%20growth",
                "%20rates&exportFormat=csv&c9SqlFilter=select%20*%20where",
                "%20County%20like%20", 
                gsub(" " , "%20", countyName), 
                "%20County%20AND%20State%20like%20", 
                gsub(" " , "%20", stateName))
  tbl.countyStats <- content(GET(urlCovidCountyStats), type = 'text/csv')
  tbl.countyCases <- subset(tbl.countyStats, Type!="Deaths")
  tbl.countyCases <- tbl.countyCases[seq(dim(tbl.countyCases)[1],1),] #flip row order 
  kingCountyWeeklyCases=countyCovidCasesPlot(98125)
  
  return(list(
    exists('zipcodeToCountyData'), 
    checkEquals(class(kingCountyWeeklyCases), "matrix"),
    checkEquals(countyName, "King"), #check the county identification is correct
    checkEquals(stateName, "Washington"),  #check the state identification is correct
    checkEquals(stateAbbreviation, "WA"), #check the state abbreviation is correct  
    checkEquals(nrow((subset(tbl.countyCases, Type=="Deaths"))), 0), #make sure death stats are filtered out
    checkEquals(tbl.countyCases$"7_day"[1], "01/22/2020" ) #make sure the data is in chronological order
  ))
} # test_countyCovidCasesPlot

#------------------------------------------------------------------------------------------------------------------------
