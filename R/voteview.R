#' @import pscl
#' @import rjson

# Trim white space off of strings
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Take the JSON from voteview an roll a rollcall vote data frame and
# rollcall description data frame
#' Read Voteview JSON String
#' 
#' Takes JSON that has been downloaded using \code{voteview.download} and
#' creates a rollcall vote data frame and a rollcall description data frame.
#' This is used by \code{voteview2rollcall} to generate the data frames that
#' are turned in to \code{rollcall} objects in \code{pscl}.
#' 
#' 
#' @param json A string of JSON downloaded from the voteview server using
#' \code{voteview.download}.
#' @return A list of two data frames: 
#' \item{votematrix }{A data frame of roll call votes with unique legislators
#'  as rows and ICPSR number, state, district, name, and votes among the 
#'  columns.} 
#' \item{rollcalls }{A data frame of information about the roll calls including
#' the vote number that matches the bove matrix, the chamber, session of 
#' congress, date, and a description.}
#' @seealso '\link{voteview.download}','\link{voteview2rollcall}'.
#' @examples
#' 
#' ## Search for sample roll calls
#' res <- voteview.search("Iraq")
#'   
#' ## Use the ids from that search to create a json string
#' json <- voteview.download(res$ids)
#'   
#' ## Get these data frames from the JSON
#' data <- read.voteview.json(json)
#'   
#' \dontrun{
#' ## NOTE: This function is generally meant to be called within the
#' ## voteview2rollcall function.
#' rollcalls <- voteview2rollcall(json)
#' }
#' @export
#' 
read.voteview.json <- function(json) { 
  suppressWarnings(data <- fromJSON(json)) # readLines gives missing end of line warning
  data$votematrix <- as.data.frame(data$votematrix, stringsAsFactors = F)[, data$vmNames]
  data$rollcalls <- as.data.frame(data$rollcalls, stringsAsFactors = F)[, data$rcNames]
  data$vmNames <- NULL
  data$rcNames <- NULL
  return( data )
}

# Take the JSON from voteview and translate it in to a PSCL rollcall object
#' Transforms Voteview JSON to a rollcall object
#' 
#' Takes JSON that has been downloaded using \code{voteview.download} and
#' creates a \code{pscl} \code{rollcall} object that can then be analyzed using
#' methods in the \code{pscl} package.
#' 
#' 
#' @param json A string of JSON downloaded from the voteview server using
#' \code{voteview.download}.
#' @return A \code{rollcall} object from the \code{pscl} package.
#' @seealso
#' '\link{voteview.search}','\link{voteview.download}','\link{read.voteview.json}'.
#' @examples
#' 
#' ## Search for example roll calls
#' res <- voteview.search("Iraq")
#'   
#' ## Use the ids from that search to create a json string
#' json <- voteview.download(res$ids)
#'   
#' ## Get roll call object
#' rc <- voteview2rollcall(json)
#'   
#' ## Summarize the roll call object
#' summary(rc)
#' summary(rc, verbose = T)
#' @export
#' 
voteview2rollcall <- function(json) {
  data <- read.voteview.json(json)
  #print( data )
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
#' Transform vector of lists to data frame
#' 
#' This is a helper function to transform the vector of lists that were
#' constructed by \code{voteview.search} in to a data frame that
#' \code{voteview.search} returns. This function should probably not be called
#' by itself.
#' 
#' 
#' @param rcs A vector of lists that is built within \code{voteview.search},
#' where each list corresponds to a roll call.
#' @return A data.frame with the following columns: 
#' \itemize{
#' \item{\code{descriptionShort} }{A short description of the bill.}
#' \item{\code{description} }{Official description of the bill.}
#' \item{\code{no} }{The total number of 'Nay' votes }
#' \item{\code{yea} }{The total number of 'Yea' votes }
#' \item{\code{chamber} }{The chamber the roll call was held in. Either "House"
#' or "Senate"} 
#' \item{\code{session} }{The session of congress the roll call was held in.}
#' \item{\code{rollnumber} }{The roll call number of the vote.}
#' \item{\code{date} }{The date the roll call was held, in string "yyyy-mm-dd"
#' format.}
#' \item{\code{id} }{Unique identifier for the roll call that allows
#' \code{voteview} to query the individual congress people's votes and other
#' details.}
#' }
#' @seealso '\link{voteview.search}'.
#' @export
#' 
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
#' Query the Voteview Database with a String
#' 
#' Searches the Voteview database with a string and returns a data frame with
#' bill IDs, summary vote statistics, and other identifying information.
#' 
#' @param query A string that will search the voteview database.
#' @param startdate A string of the format \code{"yyyy-mm-dd"} that is the
#' earliest possible date to search for a roll call. Please note for now that
#' dates are not available for roll calls from the 113 and 114 sessions of
#' congress and those results will be ommitted if you include a start or end
#' date in the query.
#' @param enddate CURRENTLY NOT FUNCTIONAL. A string of the format
#' "yyyy-mm-dd" that is the earliest possible date to search for a roll
#' call. Please note for now that dates are not available for roll calls from
#' the 113 and 114 sessions of congress and those results will be ommitted if
#' you include a start or end date in the query.
#' @param chamber Can be a string in \code{c("House", "Senate")}. The default
#' NULL value returns results from both chambers of congress.
#' @return A data.frame with the following columns: 
#' \itemize{
#' \item{\code{descriptionShort} }{A short description of the bill.}
#' \item{\code{description} }{Official description of the bill.}
#' \item{\code{no} }{The total number of 'Nay' votes }
#' \item{\code{yea} }{The total number of 'Yea' votes }
#' \item{\code{chamber} }{The chamber the roll call was held in. Either "House"
#' or "Senate"} 
#' \item{\code{session} }{The session of congress the roll call was held in.}
#' \item{\code{rollnumber} }{The roll call number of the vote.}
#' \item{\code{date} }{The date the roll call was held, in string "yyyy-mm-dd"
#' format.}
#' \item{\code{id} }{Unique identifier for the roll call that allows
#' \code{voteview} to query the individual congress people's votes and other
#' details.}
#' }
#' @seealso
#' '\link{voteview.download}','\link{voteview2rollcall}','\link{vlist2df}'.
#' @examples
#' 
#' ## Search for example roll calls
#' res <- voteview.search("Iraq")
#' res
#' 
#' \dontrun{
#' ## Search for votes with a start date
#' res <- voteview.search("Iraq", startdate = "2005-01-01")
#' res
#'  
#' ## Search for votes with an end date in just the house
#' res <- voteview.search("Iraq", startdate = "2005-01-01", chamber = "House")
#' res
#' }
#' @export
#' 
voteview.search <- function(query,
                            startdate = NULL, enddate = NULL,
                            chamber = NULL) {
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
#' Download JSON from voteview server
#' 
#' Takes roll call IDs and returns a JSON string that contains data on the
#' description, date, all individual legislator's votes, and more on each roll
#' call.
#' 
#' 
#' @param ids A string or vector of strings, where each string is a roll call
#' ID, such as "S1110326". These ids can be found using the
#' \code{voteview.search} function, if they are not already known.
#' @return A single string of JSON code.
#' @seealso '\link{voteview.search}','\link{voteview2rollcall}'.
#' @examples
#' 
#' ## Search for sample roll calls
#' res <- voteview.search("Iraq")
#'   
#' ## Use the ids from that search to create a json string
#' json <- voteview.download(res$ids)
#' @export
#' 
voteview.download <- function(ids) {
  theurl <- sprintf("http://leela.sscnet.ucla.edu/voteview/download?ids=%s&xls=F",
                     paste(ids , collapse = ","))
  conn <- url(theurl)
  suppressWarnings(resjson <- readLines(conn))
  close(conn)
  return( resjson )
}

# Test function
#test <- function() {
#  res <- voteview.search("Iraq")
#  json <- voteview.download(res$id)
#  return( read.voteview.json(json) )
#  return( voteview2rollcall(json) )
#}

