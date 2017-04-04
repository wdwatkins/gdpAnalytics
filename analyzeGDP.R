library(dplyr)
library(ggplot2)
library(lubridate)
source('helperFunctions.R')

#load DFs, join
jobsDF <- read.csv('uniqueDF_3_27.csv', stringsAsFactors = FALSE,
                   colClasses = "character")
xmlDF <- read.csv('GDP_XML_3_27.csv', stringsAsFactors = FALSE, 
                  colClasses = c(rep("character", 6), "numeric"))

joinedDF_noAgent <- left_join(jobsDF, xmlDF, by = "requestLink") 

userAgents <- read.csv('useragents.csv', stringsAsFactors = FALSE, 
                       colClasses = "character", 
                       col.names = c("ignore_id", "requestId", "agent"))
agentJobs <- left_join(joinedDF_noAgent, userAgents, by = "requestId")

agentJobs <- filter(agentJobs, !is.na(agent))

#failed vs successful jobs
#most popular datasets
successJobs <- filter(joinedDF_noAgent, status == "SUCCEEDED") %>% 
  mutate(creationDate = date(creationTime))
failedJobs <- filter(joinedDF_noAgent, status == "FAILED")

successJobsNoTests <- removePyGDPtest(successJobs)

#want stats for successful jobs
successGrp <- group_by(successJobsNoTests, data_uri)
summary <- summarise(successGrp, n = n()) %>% 
  arrange(desc(n)) %>% mutate(dataSet = sub(".*dodsC/", "", data_uri))

gt100 <- filter(summary, n > 100)

p<-ggplot(data=gt100, aes(x=dataSet, y=n)) + geom_bar(stat="identity") +
  coord_flip() + ylab("Sucessful Jobs")

#########

#add simple user agent name
successAgentJobs <- agentJobs %>% filter(status == "SUCCEEDED") %>% 
  mutate(creationDate = date(creationTime)) %>% arrange(creationDate)
successAgentJobs <- shortAgentName(successAgentJobs)
successAgentJobs$agentCount <- 1
successAgentJobs_grpcum <- successAgentJobs %>% group_by(shortAgentName) %>% mutate(agentCountCum = cumsum(agentCount)) 

#remove pyGDP tests
successAgentJobs_grpcum <- removePyGDPtest(successAgentJobs_grpcum)
plot(successAgentJobs_grpcum$creationDate , 
     successAgentJobs_grpcum$agentCountCum, col = successAgentJobs_grpcum$color, 
     xlab = "Date (2016-2017)", ylab = "Job Count", main = "GDP User Agents", 
     pch = 16)
legend("topleft", legend = c("geoknife", "python", "UI", "other"), 
       col = c("darkgoldenrod1", "aquamarine", "mediumorchid", "gray48"), pch = 16)

#time series single data set




