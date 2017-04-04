#grab gdp json from dashboard 
library(jsonlite)
library(dplyr)
source('helperFunctions.R')

pageEmpty <- FALSE
i = 1
allDF <- data.frame()
while(!pageEmpty) {
  baseURL <- 'https://cida.usgs.gov/gdp/process/list?page'
  useURL <- paste(baseURL, i, sep = "=")
  pageJSON <- retryJSON(useURL, simplifyVector = TRUE) 
  if(length(pageJSON) > 0){
    allDF <- bind_rows(allDF, pageJSON)
  } else {
    pageEmpty <- TRUE
  }
  print(paste("Finished page", i))
  i <- i + 1
}

uniqueDF <- unique(allDF) #if requests are submitted during d/l, will cause duplicates
write.csv(uniqueDF, row.names = FALSE, quote = TRUE, file = "uniqueDF_3_27.csv")
