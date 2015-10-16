library(rjson)
library(pscl)

# Trim white space off of strings
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Take the JSON from voteview an roll a rollcall vote data frame and
# rollcall description data frame
read.voteview.json <- function(json) { 
  suppressWarnings(data <- fromJSON(json)) # readLines gives missing end of line warning
  data$votematrix <- as.data.frame(data$votematrix, stringsAsFactors = F)[, data$vmNames]
  data$rollcalls <- as.data.frame(data$rollcalls, stringsAsFactors = F)[, data$rcNames]
  data$vmNames <- NULL
  data$rcNames <- NULL
  return( data )
}

# Take the JSON from voteview and translate it in to a PSCL rollcall object
voteview2rollcall <- function(json) {
  data <- read.voteview.json(json)
  print( data )
  dat  <- data$votematrix[,grep("^V\\d+", names(data$votematrix))]
  rnames <- sprintf("%s %s - %s", trim(data$votematrix$name),
                                  data$votematrix$cqlabel,
                                  data$votematrix$icpsr)
  rownames(dat) <- rnames
  legis.data <- data$votematrix[, -grep("^V\\d+", names(data$votematrix))]
  rownames(legis.data) <- rnames
  
  rc <- rollcall(data = dat,
                 yea = c(1, 2, 3),
                 nay = c(4, 5, 6),
                 missing = c(NA, 7, 8, 9),
                 notInLegis = 0,
                 legis.data = legis.data,
                 vote.data = data$rollcalls,
                 source = "Download from VoteView")
  return(rc)
}

# Helper function that transforms a vector of lists into a dataframe
vlist2df <- function(rcs) {
  df <- list()
  flds <- names(rcs[[1]])
  
  for (f in flds) {
    md <- ifelse(class(rcs[[1]][[f]]) == "character", "character", "integer")
    df[[f]] <- vector(mode = md,length = length(rcs))
  }
  
  for (i in 1:length(rcs)) {
     for (f in flds) {
       # There are some missing fields for votes in the  113 or 114 congresses
       if (!is.null(rcs[[i]][[f]])) {
         df[[f]][i] <- rcs[[i]][[f]]
       }
     }
  }
  
  return( as.data.frame(df, stringsAsFactors = FALSE) )
}

# Function to run a voteview query, returning a dataframe of matching votes and
# basic data about those votes
voteview.search <- function(query,
                            startdate = NULL, enddate = NULL, chamber = NULL) {
   theurl <- sprintf("http://leela.sscnet.ucla.edu/voteview/search?q=%s",
                     URLencode(query))
   if (!(is.null(startdate))) {
     theurl <- sprintf("%s&startdate=%s", theurl, startdate)
   }
   if (!(is.null(enddate))) {
     theurl <- sprintf("%s&enddate=%s", theurl, enddate)
   }
   if (!(is.null(chamber))) {
     theurl <- sprintf("%s&chamber=%s", theurl, chamber)
   }   
   conn <- url(theurl)
   suppressWarnings(resjson <- fromJSON(readLines(conn)))
   cat(sprintf("Query '%s' returned %i votes...\n", query, resjson$recordcount))
   close(conn)
   return( vlist2df(resjson$rollcalls) )
}

# Function to download voteview rollcall data in JSON format
voteview.download <- function(ids) {
  theurl <- sprintf("http://leela.sscnet.ucla.edu/voteview/download?ids=%s&xls=F",
                     paste(ids , collapse = ","))
  conn <- url(theurl)
  suppressWarnings(resjson <- readLines(conn))
  close(conn)
  return( resjson )
}

# Test function
test <- function() {
  res <- voteview.search("Iraq")
  json <- voteview.download(res$id)
#  return( read.voteview.json(json) )
  return( voteview2rollcall(json) )
}

