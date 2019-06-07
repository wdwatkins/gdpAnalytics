library(dplyr)
library(data.table)
source('R/helperFunctions.R')

#load two 7_3_newStatsOnly csvs
jobsDF <- read.csv('canonicalData/uniqueDF_json.csv', stringsAsFactors = FALSE)
xmlDF <- fread('canonicalData/xml.csv')

allDF <- left_join(jobsDF, xmlDF, by = "requestLink")
allDF_filtered_nopy <- removePyGDPtest(allDF) 
allDF_filtered <- filter(allDF_filtered_nopy, creationTime > as.Date('2017-06-12') & 
                           dataFetchInfo.fetchSize > outputInfo.returnSize &
                           dataFetchInfo.fetchSize > 1 &
                           outputInfo.returnSize > 1)
                           #data_uri %in% popular_uris$data_uri)
allDF_filtered_success <- filter(allDF_filtered, 
                                 status == "SUCCEEDED" & clientInfo.userHash != "") %>% 
  mutate(dataFetchInfo.fetchSize = as.numeric(dataFetchInfo.fetchSize),
         outputInfo.returnSize = as.numeric(outputInfo.returnSize),
         percentOverWire = outputInfo.returnSize/dataFetchInfo.fetchSize * 100,
         bwSaved = dataFetchInfo.fetchSize - outputInfo.returnSize)

#stats
uniqueHash <- length(unique(allDF_filtered_success$clientInfo.userHash))
cat(paste(uniqueHash, "unique users hashes with successful jobs\n"))

percentOverWire_med <- median(allDF_filtered_success$percentOverWire, na.rm = TRUE)
percentOverWire_mean <- mean(allDF_filtered_success$percentOverWire, na.rm = TRUE)
cat(paste("Mean", round(percentOverWire_mean, 1), "percent over the wire\n"))
cat(paste("Median", round(percentOverWire_med,1), "percent over the wire\n"))

bwSaved_mean <- round(mean(allDF_filtered_success$bwSaved, na.rm = TRUE), 1)/10^9
bwSaved_median <- round(median(allDF_filtered_success$bwSaved, na.rm = TRUE), 1)/10^9
bwSaved_total <- round(sum(allDF_filtered_success$bwSaved)/10^12)
cat(paste(bwSaved_total, "total terabytes saved over the wire\n"))
cat(paste(bwSaved_median, "median GB saved over the wire per job\n"))
cat(paste(bwSaved_mean, "mean GB saved over the wire per job\n"))

