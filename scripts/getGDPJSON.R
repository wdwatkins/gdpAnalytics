#grab gdp json from dashboard 
library(jsonlite)
library(dplyr)
library(data.table)
library(assertthat)
source('scripts/helperFunctions.R')

existing <- fread('canonicalData/uniqueDF_json.csv', colClasses = "character")

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
  
  #Is there any overlap with the existing data?  just compare IDs
  if(any(pageJSON$requestId %in% existing$requestId)) {
    pageEmpty <- TRUE
  }
}
allDF <- mutate(allDF, requestId = sub(".*=", "", requestLink)) %>% mutate_all(as.character)
#combine
#if requests are submitted during d/l, will cause duplicates, also depending on 
#page break for existing records
#don't trust unique here - best to just go off of requestId
bound <- bind_rows(existing, allDF)
dups <- which(duplicated(bound$requestId))
uniqueDF <- slice(bound, -dups)

assert_that(uniqueN(uniqueDF$requestId) == nrow(uniqueDF))
fwrite(uniqueDF, quote = TRUE, file = "canonicalData/uniqueDF_json.csv")

#server was restarted june 12th for new logging
unique_newStats <- filter(uniqueDF, creationTime > "2017-06-12")
fwrite(unique_newStats, file = "data/uniqueDF_newStats.csv")

