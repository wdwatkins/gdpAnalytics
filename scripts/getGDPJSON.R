#grab gdp json from dashboard 
library(jsonlite)
library(dplyr)
library(data.table)
source('scripts/helperFunctions.R')

pageEmpty <- FALSE
i = 1
allDF <- data.frame()
while(!pageEmpty) {
  baseURL <- 'https://cida.usgs.gov/gdp/process/list?page'
  useURL <- paste(baseURL, i, sep = "=")
  pageJSON <- retryJSON(useURL, simplifyVector = TRUE)
  #now returning some kind of invalid df, needs to be flattened or
  #crashes R with bind_rows
  if(length(pageJSON) > 0){
    allDF <- bind_rows(allDF, flatten(pageJSON))
  } else {
    pageEmpty <- TRUE
  }
    
  print(paste("Finished page", i))
  i <- i + 1
}

uniqueDF <- unique(allDF) #if requests are submitted during d/l, will cause duplicates
fwrite(uniqueDF, quote = TRUE, file = "data/uniqueDF_7_11.csv")

#server was restarted june 12th for new logging
unique_newStats <- filter(uniqueDF, creationTime > "2017-06-12")
fwrite(unique_newStats, file = "data/uniqueDF_newStats_7_11.csv")

