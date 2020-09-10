library(readxl)
countyPopulationDensity<- function(countyName, stateName) {
  countyName <- paste(countyName,"County")
  countyInfo <- read_excel("censusCountyData.xlsx")
  tbl.county <- subset(countyInfo, County == countyName) 
  tbl.county <- subset(tbl.county, State == stateName) 
  tbl.county[3]<- tbl.county[3]*2.58998811 #convert from people/sq.km to people/sq.miles
  tbl.county$`County Population Density` #returns the county population density 
}
