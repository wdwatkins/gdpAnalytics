retryJSON <- function(..., retries=3){
  
  safeWQP = function(...){
    result = tryCatch({
      read_json(...)
    }, error = function(e) {
      if(e$message == 'Operation was aborted by an application callback'){
        stop(e)
      }
      return(NULL)
    })
    return(result)
  }
  retry = 1
  while (retry < retries){
    result = safeWQP(...)
    if (!is.null(result)){
      retry = retries
    } else {
      message('query failed, retrying')
      retry = retry+1
    }
  }
  return(result)
}

library(dplyr)
library(lubridate)
plotSetsInList <- function(df, sets) {
  sets <- na.omit(sets)
  plotMat <- matrix(data = 1:12, nrow = 3, ncol = 2, byrow = TRUE)
  
  for(i in 1:length(sets)) {
    if(i %in% c(seq(1,25, by = 6))){
      layout(plotMat)
    }
    filteredSet <- filter(df, dataSet == sets[i]) %>% arrange(creationDate) %>% 
      mutate(count = 1, sumCount = cumsum(count))
    xmin = as.Date("2015-08-05")
    xmax = as.Date("2017-04-01")
    if(filteredSet$hosting[1] == "EROS") {
      col = "dodgerblue"
    } else { col = "darkgray"}
    plot(x = filteredSet$creationDate, y = filteredSet$sumCount, 
         main = filteredSet$dataSet[1], 
         xlim = c(xmin, xmax), col = col,
         xlab = "Date (August 2015 - Present)", ylab = "Job Count")
  }
}


plotAgentsCumulative <- function(df) {
  
}

#parse agent to usable names, add colors
shortAgentName <- function(df) {
  df$shortAgentName <- NA
  #df <- mutate(df, shortAgentName = ifelse(grepl(pattern = "geoknife", x = agent), "geoknife", "other"))
  df <- mutate(df, shortAgentName = ifelse(grepl(pattern = "geoknife", x = agent), "geoknife",
                                           ifelse(grepl(pattern = "python", x = agent, ignore.case = TRUE), "python",
                                                  ifelse(grepl(pattern = "Mozilla", x = agent, ignore.case = TRUE), "UI",
                                                         "other"))))
  df <- mutate(df, color = ifelse(grepl(pattern = "geoknife", x = agent), "darkgoldenrod1",
                                  ifelse(grepl(pattern = "python", x = agent, ignore.case = TRUE), "aquamarine",
                                         ifelse(grepl(pattern = "Mozilla", x = agent, ignore.case = TRUE), "mediumorchid",
                                                "gray48"))))
  return(df)
}

library(dplyr)
#remove pyGDP tests, based on an example
#assuming all tests are adjacent
removePyGDPtest <- function(df) {
  pyTest <- read.csv('pygdp_test_sample.csv', colClasses = "character")
  toDrop <- data.frame()
  
  if("shortAgentName" %in% names(df)){
    pyJobs <- filter(df, shortAgentName == "python")
    for(i in seq(1,nrow(pyJobs))) {
      chunkDF <- slice(pyJobs, i:(i+4)) %>% select(identifier, start, end, data_uri,
                                                   variable_names, shortAgentName)
      if(identical(pyTest, as.data.frame(chunkDF))) {
        toDrop <- bind_rows(toDrop, slice(pyJobs, i:(i+4)))
      }
    } 
  }else {#for earlier data with no user agent
    pyTest <- select(pyTest, -shortAgentName)
    for(i in seq(1,nrow(df))) {
      chunkDF <- slice(df, i:(i+4)) %>% select(identifier, start, end, data_uri,
                                               variable_names)
      if(identical(pyTest, as.data.frame(chunkDF))) {
        print(i)
        toDrop <- bind_rows(toDrop, slice(df, i:(i+4)))
      }
    }
  }
  #get rid of test rows in main df
  retDF <- anti_join(df, toDrop)
  return(retDF)
}

removeGMOtest <- function(df) {
  df_remove <- df %>% filter(grepl(x = data_uri, pattern = "gmo/GMO_w_meta.ncml") &
                           start == "1950-01-01" & end == "1950-01-02" &
                           variable_names == "Prcp" & 
                            identifier == "FeatureCoverageOPeNDAPIntersectionAlgorithm")
  df_filt <- anti_join(df, df_remove)
  return(df_filt)
}


parseGDPElapsed <- function(vec) {
  #order of these is important!
  vec <- gsub("s", "seconds", vec)
  vec <- gsub("m", "minutes", vec)
  return(vec <- duration(vec))
}
  
getUniqueJobsSummary <- function(df) {
  summ <- group_by(df, data_uri) %>% summarize(unique = length(unique(md5)), total = n())
  return(summ)
}
