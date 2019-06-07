library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
source('R/helperFunctions.R')

#load DFs, join
jobsDF <- fread('../data/uniqueDF_4_21.csv', stringsAsFactors = FALSE,
                   colClasses = "character")
xmlDF <- fread('../data/GDP_XML_4_21.csv', stringsAsFactors = FALSE, 
                  colClasses = c(rep("character", 6), "numeric"))

joinedDF_noAgent <- left_join(jobsDF, xmlDF, by = "requestLink") 

userAgents <- read.csv('../data/useragents.csv', stringsAsFactors = FALSE, 
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

#calculate number of time steps per call
#don't have info for 2 datasets
resDF <- read.csv('gt100.txt', sep = "\t", stringsAsFactors = FALSE)
#need to add hours for 00:00:00 dates
resDF <- filter(resDF, !is.na(res_num)) %>% rowwise() %>% mutate(dur = duration(res_num, units = res_unit))
successJobs_gt100 <- filter(successJobsNoTests, data_uri %in% resDF$data_uri) %>% rowwise() %>% 
                      mutate(end = ifelse(nchar(end) == 10, paste(end, "00:00:00"), end),
                             start = ifelse(nchar(start) == 10, paste(start, "00:00:00"), start))
successJobs_gt100 <- left_join(successJobs_gt100, resDF, by = "data_uri") %>% 
                      filter(nchar(end) > 0) %>% 
                      mutate(start = as.POSIXct(start), end = as.POSIXct(end))
#time bounds are inclusive!
successJobs_nsteps <- successJobs_gt100 %>% mutate(nsteps = round((end - start)/dur) + 1) %>% 
                        filter(nsteps > 0)

#there are a few jobs with weird end dates "0203-12-31" that cause negative nsteps

stepsSummary <- group_by(successJobs_nsteps, dataSet) %>% summarize(n= n(), totalSteps = sum(nsteps), 
                                                                    meanSteps = totalSteps/n)

p<-ggplot(data=stepsSummary, aes(x=reorder(dataSet, n), y=n)) + geom_bar(stat="identity") +
  coord_flip() + ylab("Sucessful Jobs") + xlab("Data set") + theme(text = element_text(size = 16, face = "bold"))
p
p2<-ggplot(data=stepsSummary, aes(x=reorder(dataSet, n), y=totalSteps)) + geom_bar(stat="identity") +
  coord_flip() + ylab("Total time steps requested") + xlab(NULL) + theme(text = element_text(size = 16, face = "bold"),
                                                                                axis.text.y = element_blank())
library(gridExtra)
grid.arrange(arrangeGrob(p, p2, ncol = 2, widths= c(6,4)))

#locally - hosted data sets

successJobs_local <- rowwise(successJobsNoTests) %>% mutate(eros = ifelse(grepl(pattern = "cida", data_uri),
                                                                          yes = TRUE, no = FALSE)) 
  
# successJobs_local <- filter(successJobsNoTests, grepl(pattern = "cida", data_uri)) %>% 
#                     mutate(dataSet = sub(".*dodsC/", "", data_uri))
local_summary <- group_by(successJobs_local, eros) %>% summarise(n=n())
