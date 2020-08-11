library(httr) 
library(tidyverse)
url <- "https://knowi.com/api/data/ipE4xJhLBkn8H8jisFisAdHKvepFR5I4bGzRySZ2aaXlJgie?entityName=County%207%20day%20growth%20rates&exportFormat=csv&c9SqlFilter=select%20*%20where%20County%20like%20King%20County%20AND%20State%20like%20Washington"
data=content(GET(url), type = 'text/csv')

data1<-subset(data, Type!="Deaths") #only has cases stats not death stats
data2 %>% map_df(rev) #flip row order so that stats start from earliest date and ends with latest date

barplot(data3$"7_day_count", main="Weekly COVID-19 Case Count in King County",xlab="Weeks since 1/29/20", ylab="Weekly Infection Count")
