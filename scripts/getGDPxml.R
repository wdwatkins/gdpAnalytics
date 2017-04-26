#read in jobs df (from JSON)
library(data.table)
library(digest)
inFile <- 'uniqueDF_4_21.csv'
outFile <- 'GDP_XML_4_21.csv'

jobsDF <- fread(inFile, stringsAsFactors = FALSE,
                   colClasses = "character")
xmlDF <- data.frame(matrix(nrow = nrow(jobsDF), ncol = 8)) #preallocate
names(xmlDF) <- c("requestLink", "alg_ver", "start", "end", "data_uri", 
                  "variable_names", "nvars", "md5")
library(geoknife)
for(i in 36900:nrow(jobsDF)) {
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
                       digest(xml(job)), stringsAsFactors = FALSE)
  
  xmlDF[i,] <- linkDF
  if(i %% 100 == 0) {
    print(paste("Finished ", i))
  }
}

fwrite(xmlDF, file = outFile, quote = TRUE)
