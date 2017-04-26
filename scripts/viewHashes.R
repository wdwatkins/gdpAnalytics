library(dplyr)
library(data.table)
library(lubridate)
jobsDF <- fread('data/uniqueDF_4_21.csv', stringsAsFactors = FALSE,
                colClasses = "character")
xmlDF <- fread('data/GDP_XML_4_21.csv')
joinedDF_noAgent <- left_join(jobsDF, xmlDF, by = "requestLink") 
successJobs <- filter(joinedDF_noAgent, status == "SUCCEEDED") %>% 
  mutate(creationDate = date(creationTime))

allGrp <- group_by(successJobs, data_uri, md5) %>% summarize(n=n()) %>% 
          arrange(desc(n))

for(hash in allGrp$md5) {
  filtDF <- filter(successJobs, md5 == hash)
  print(paste(nrow(filtDF), "rows"))
  View(filtDF)
  invisible(readline("Press a key for next"))
}