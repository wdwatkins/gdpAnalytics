#read in jobs df (from JSON)
jobsDF <- read.csv('uniqueDF_3_27.csv', stringsAsFactors = FALSE,
                   colClasses = "character")
xmlDF <- data.frame(matrix(nrow = nrow(jobsDF), ncol = 7)) #preallocate
names(xmlDF) <- c("requestLink", "alg_ver", "start", "end", "data_uri", 
                  "variable_names", "nvars")
library(geoknife)
for(i in 1:nrow(jobsDF)) {
  link <- jobsDF$requestLink[i]
  
  #some failed jobs don't have XML
  if(jobsDF$status[i] == "FAILED") {
    job <- tryCatch({
      geojob(link)
    }, error = function(err) {
      err
    })
  } else {
    job <- geojob(link)
  }
  if(inherits(job, "error")){
    message("Failed job and geojob() failed - skipping this one")
    next
  }
    
  wd <- tryCatch({webdata(job)
  }, error = function(err) {
    message("error in creating webdata, returning NAs for all fields")
    return(webdata())
  })
  wdTimes <- as.character(times(wd)) #otherwise matrix makes it numeric
  wdVars <- variables(wd)
  
  linkDF <- data.frame(link, job@algorithm.version, wdTimes[1], wdTimes[2],
                       url(wd), paste(wdVars, collapse = ":"), length(wdVars),
                       stringsAsFactors = FALSE)
  
  xmlDF[i,] <- linkDF
  if(i %% 100 == 0) {
    print(paste("Finished ", i))
  }
}

write.csv(xmlDF, file = 'GDP_XML_3_27.csv', row.names = FALSE)
