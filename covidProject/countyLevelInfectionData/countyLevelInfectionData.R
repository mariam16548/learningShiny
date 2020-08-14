library(httr) 
countyLevelInfectionData<- function(zip) {
  countyStateNames <- zipcodeToCounty(zip)
  countyName <- countyStateNames[1]
  stateName <- countyStateNames[2]
  urlCountyInfectionStats <- paste0("https://knowi.com/api/data/ipE4xJhLBkn8H8jisFisAdHKvep",
                                "FR5I4bGzRySZ2aaXlJgie?entityName=County%207%20day%20growth",
                                "%20rates&exportFormat=csv&c9SqlFilter=select%20*%20where",
                                "%20County%20like%20", 
                                gsub(" " , "%20", countyName), 
                                "%20County%20AND%20State%20like%20", 
                                gsub(" " , "%20", stateName))
  tbl.countyStats <- content(GET(urlCountyInfectionStats), type = 'text/csv')
  tbl.countyCases <- subset(tbl.countyStats, Type!="Deaths") #only has cases not death stats
  tbl.countyCases <- tbl.countyCases[seq(dim(tbl.countyCases)[1],1),] #flip row order 
  tbl.countyCases <- subset(tbl.countyCases, select = c('7_day','7_day_count')) #only has date and case stats
  names(tbl.countyCases)[1] <- "weekDate"
  names(tbl.countyCases)[2] <- "caseCount"
  print(data.frame(tbl.countyCases), row.names = FALSE)
}
