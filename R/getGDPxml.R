#read in jobs df (from JSON)
library(data.table)
library(digest)
library(assertthat)
library(dplyr)
xmlPath <- 'canonicalData/xml.csv'
jsonFile <- 'canonicalData/uniqueDF_json.csv'
existingXML <- fread(xmlPath)
existingXML <- mutate(existingXML, requestId = sub(".*=", "", requestLink))
jobsDF <- fread(jsonFile, stringsAsFactors = FALSE,
                   colClasses = "character")

#check which Ids are in JSON and not xml
links_download <- jobsDF$requestLink[!jobsDF$requestId %in% existingXML$requestId] 
assert_that(length(links_download) == (nrow(jobsDF) - nrow(existingXML)))

xmlDF <- data.frame(matrix(nrow = length(links_download), ncol = 8)) #preallocate
names(xmlDF) <- c("requestLink", "alg_ver", "start", "end", "data_uri", 
                  "variable_names", "nvars", "md5")
jobs_subset <- filter(jobsDF, requestLink %in% links_download)
message(paste(length(links_download), "new jobs to download"))
library(geoknife)
for(i in 1:length(links_download)) {
  link <- links_download[i]
  
  print("starting")
  #some failed jobs don't have XML
  if(jobs_subset$status[i] == "FAILED") {
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
  print(i)
  if(i %% 100 == 0) {
    print(paste("Finished ", i))
  }
}

#if xml download/parsing hangs up
xmlDF_na <- xmlDF %>% mutate(na_count = rowSums(is.na(select(., everything())))) %>% filter(na_count <8)
#now bind to exisiting
all_xml <- bind_rows(existingXML, xmlDF)
assert_that(nrow(all_xml) == nrow(jobsDF))

fwrite(all_xml, file = xmlPath, quote = TRUE)
