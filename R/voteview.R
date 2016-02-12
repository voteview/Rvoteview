#' @import pscl
#' @import rjson
#' @import httr
#' @import fastmatch

# Trim white space off of strings
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Function to run a voteview query, returning a dataframe of matching votes and
# basic data about those votes
#' Query the Voteview Database with a String
#' 
#' Searches the Voteview database with a string and returns a data frame with
#' bill IDs, summary vote statistics, and other identifying information.
#' 
#' @param alltext A string or character vector that searches all text fields in
#' the voteview datase. Each element is a specific phrase joined together with
#' an AND statement.
#' @param startdate A string of the format \code{"yyyy-mm-dd"} that is the
#' earliest possible date to search for a roll call.
#' @param enddate A string of the format
#' "yyyy-mm-dd" that is the earliest possible date to search for a roll
#' call.
#' @param session A numeric vector of the sessions of congress to constrain the search to. The default is all sessions.
#' @param chamber A string in \code{c("House", "Senate")}. The default
#' NULL value returns results from both chambers of congress.
#' @param maxsupport Support is the share of Yea votes over total Yea and Nay votes. \code{maxsupport} is a number specifying the maximum support allowed for returned votes.
#' @param minsupport A number specifying the minimum support allowed for returned votes.
#' @param query A string that can specify a more complex search of all of the
#' text fields. See examples for usage and syntax. Can only be used if 
#' \code{alltext} is not specified.
#' @return A data.frame with the following columns: 
#' \itemize{
#' \item{\code{description} }{Official description of the bill.}
#' \item{\code{shortdescription} }{A short description of the bill.}
#' \item{\code{date} }{The date the roll call was held, in string "yyyy-mm-dd"
#' format.}
#' \item{\code{bill} }{Bill name abbreviation.}
#' \item{\code{chamber} }{The chamber the roll call was held in. Either "House"
#' or "Senate"} 
#' \item{\code{session} }{The session of congress the roll call was held in.}
#' \item{\code{rollnumber} }{The roll call number of the vote.}
#' \item{\code{yea} }{The total number of 'Yea' votes.}
#' \item{\code{nay} }{The total number of 'Nay' votes.}
#' \item{\code{support} }{Percent of 'Yea' votes out of all 'Yea' or 'Nay' 
#' votes (excludes absent).}
#' \item{\code{id} }{Unique identifier for the roll call that allows
#' \code{voteview} to query the individual congress people's votes and other
#' details.}
#' }
#' @details
#' \code{query} uses the following general syntax, \code{field:query AND (query OR query)}. For example, if you wanted to find votes with "war" and either "iraq" or "afghanistan" in any text field, you could set the query to be \code{"alltext:war AND (iraq OR afghanistan)"}. If you wanted to do the same query but only return the votes with "defense" in the description field, the query would become \code{"alltext:war AND (iraq OR afghanistan) description:defense"}
#' 
#' The fields that can be searched with text are \code{code.Clausen}, \code{code.Peltzman}, \code{code.Issue}, \code{description}, \code{shortdescription}, \code{bill}, and \code{alltext}. The fields that can be searched by number are \code{session}, \code{yea}, \code{nay}, and \code{support}.
#' 
#' @seealso
#' '\link{voteview_download}','\link{voteview2rollcall}','\link{vlist2df}'.
#' @examples
#' 
#' ## Search for example roll calls
#' res <- voteview_search("Iraq")
#' res
#' 
#' \dontrun{
#' ## Search for votes with a start date
#' res <- voteview_search("Iraq", startdate = "2005-01-01")
#' res
#'  
#' ## Search for votes with an end date in just the house
#' res <- voteview_search("Iraq", startdate = "2005-01-01", chamber = "House")
#' res
#' 
#' #' ## Search for votes with an end date in just the house in 112 session
#' res <- voteview_search("Iraq", startdate = "2005-01-01", session = 112, chamber = "House")
#' res
#' }
#' @export
#' 
voteview_search <- function(alltext = NULL,
                            startdate = NULL,
                            enddate = NULL,
                            session = NULL,
                            chamber = NULL,
                            maxsupport = NULL,
                            minsupport = NULL,
                            query = NULL) {
  
  # todo: allow or, and and allow "phrase"
  
  # Input validation
  if ((is.null(alltext) & is.null(query)) | (!is.null(alltext) & !is.null(query))) stop("Must specify 'alltext' or 'query', but not both")

  # Start query
  if (is.character(alltext)){
    query_string <- paste0("alltext:", paste(alltext, collapse = " and "))
  } else {
    query_string <- query
  }
  
  dates <- c(as.character(startdate), as.character(enddate))
  if (length(grep("^[0-9]{4}($|-(0[0-9]|1[0-2])($|-([0-2][0-9]|3[0-1])))",
                  c(startdate, enddate))) != length(dates)){
    stop("A date is formatted incorrectly. Please use yyyy, yyyy-mm, or yyyy-mm-dd format")
  }
  
  if (!is.null(chamber)) {
    if (!(tolower(chamber) %in% c("house", "senate"))) stop("Chamber must be either 'House' or 'Senate'")
  }
  
  # todo: check to make sure just vector
  if (!is.null(session)) {

    if (any(session < 0 | session > 999)) {
      stop("Session must be a positive number or vector of positive numbers greater than 0 and less than 1000")
    }
    
    # Add session to query (if session is one number, ignores second paste)
    query_string <- paste0(query_string, " session:", paste(session, collapse = " and "))
  }
  
  if (!is.null(maxsupport) | !is.null(minsupport)) {
    
    if (any(c(minsupport, maxsupport) < 0 | c(minsupport, maxsupport) > 100)) {
      stop("Min and max support must be between 0 and 100")
    }
    
    if(is.null(minsupport)) minsupport <- 0
    if(is.null(maxsupport)) maxsupport <- 100
    
    if (maxsupport < minsupport) stop("maxsupport must be greater than minsupport")
    
    query_string <- paste0(query_string, " support:[", paste(c(minsupport, maxsupport), collapse = " to "), "]")
  }
  
  theurl <- "http://leela.sscnet.ucla.edu/voteview/search"
  message(query_string)
  resp <- POST(theurl, body = list(q = query_string,
                                   startdate = startdate,
                                   enddate = enddate,
                                   chamber = chamber))

  if (substr(content(resp, as = "text"), 1, 1) != "{") {
    stop(content(resp, as = "text"))
  }
  
  suppressWarnings(resjson <- fromJSON(content(resp,
                                               as = "text",
                                               encoding = "utf-8")))
  
  message(sprintf("Query '%s' returned %i rollcalls...\n", query_string, resjson$recordcount))
  # todo: also return in printout the date and chamber parameters
  if(!is.null(resjson$errormessage)) warning(resjson$errormessage)
  if(resjson$recordcount == 0) stop("No rollcalls found")
  
  # todo: add ability to store the final mongo query string
  return( vlist2df(resjson$rollcalls) )
}

# Internal function to get a roll call from the web api
#' @export
voteview_getvote <- function(id) {
  theurl <- "http://leela.sscnet.ucla.edu/webvoteview/api/getvote/"
  resp <- GET(paste0(theurl, id, "/R"), timeout(1))
}

# Function to download roll calls and turn them in to a voteview object
#' Download rollcalls as voteview object
#' @export
#' 
voteview_download <- function(ids) {
  
  uniqueids <- unique(ids)

  if (length(uniqueids) < length(ids)) message("Duplicated ids dropped")
  
  dat <- vector("list", length(uniqueids))
  dat <- build_votelist(dat, uniqueids)

  return( voteview2rollcall(votelist2voteview(dat)) )
}

# Function to build voteview object from voteview list
#' Internal function to get rollcalls and build voteview list
#' @export
#' 
build_votelist <- function(votelist, ids) {
  
  message(sprintf("Downloading %d rollcalls", length(ids)))
  pb <- txtProgressBar(min = 0, max = length(ids), style = 3)
  
  unretrievedcount <- 0
  unretrievedids <- NULL
  
  # Download votes
  for (i in 1:length(ids)) {
    attempts <- 1
    vote <- "ERROR"
    
    while (vote[1] == "ERROR" & attempts < 5) {
      
      vote <- tryCatch( {
        resp <- voteview_getvote(ids[i])
        fromJSON(content(resp, as = "text"))
      },
      error = function(e) {
        attempts <<- attempts + 1
        Sys.sleep(0.5)
        return("ERROR")
      },
      warning = function(w) {
        lastwarning <<- w
        
        attempts <<- attempts + 1
        Sys.sleep(0.5)
        return("ERROR")
      },
      interrupt = function(x) {
        message(sprintf("Process interrupted, using completed %d rollcalls", i-1))
        return("INTERRUPT")
      })
    }
    
    if (vote[1] == "INTERRUPT") {
      return(list(votelist = votelist,
                  unretrievedids = c(unretrievedids, ids[i:length(ids)])))
    }
    
    if (vote[1] == "ERROR") {
      
      errmess <- geterrmessage()

      if ("unexpected character" == substr(errmess, 1, 20)) {
         message(sprintf("Rollcall %s could not be found, continuing with remaining rollcalls.", ids[i]))
         unretrievedcount <- unretrievedcount + 1
         unretrievedids[unretrievedcount] <- ids[i]
       } else {
         if (i == 1) {
           stop(sprintf("Cannot connect to server. No downloads possible."))
         }
         if ("Couldn't resolve host name" == errmess) {
           warning(sprintf("Cannot connect to server. Will output the %d completed downloads.", i-1-unretrievedcount))
         } else {
           warning("Unexpected warning: ", errmess)
           warning(sprintf("Cannot connect to server. Will output the %d completed downloads.", i-1-unretrievedcount))
         }
         return(list(votelist = votelist,
                     unretrievedids = c(unretrievedids, ids[i:length(ids)])))
       }
    } else {
      votelist[[i]] <- vote
    }
    
    setTxtProgressBar(pb, i)
  }
  
  return(list(votelist = votelist,
              unretrievedids = unretrievedids))
}

# Function to build voteview object from voteview list
#' BUild voteview object from downloaded votelist
#' @export
#' 
votelist2voteview <- function(dat) {
  
  if(is.null(dat$unretrievedids)) {
    votelist <- dat$votelist
  } else{
    warning("Not all ids were able to be downloaded. Check 'unretrievedids' in your rollcall object. You can use complete_download to try and retrieve these again.")
    votelist <- dat$votelist[!sapply(dat$votelist, is.null)]
  }
  
  if(!length(votelist)) stop("Votelist is empty.")
  
  # Get unique list of members
  # 'unlist' because if voters are not all the same, then sapply returns list
  # 'c' in case voters ARE all the same, then sapply returns array
  members <- unique(c(unlist(sapply( votelist, function(vote) sapply(vote$votes, function(member) member$icpsr)))))

    # Data to keep from return
  # todo: add option to keep nominate data for plot method
  rollcalldatanames <- setdiff(names(votelist[[1]]),
                               c("votes", "apitype", "nominate"))
  votedatanames <- setdiff(names(votelist[[1]]$votes[[1]]),
                           c("y", "x", "vote", "icpsr"))
  
  # Generic vote names, i.e. V1, V2
  votenames <- paste0(rep("V", times = length(votelist)), 1:length(votelist))
  
  data <- list()
  data$votematrix <- data.frame(icpsr = as.numeric(members))
  data$votematrix[, c(votedatanames, votenames)] <- NA
  data$rollcalls <- data.frame(vote = votenames)
  data$rollcalls[, rollcalldatanames] <- NA
  
  message(sprintf("Building the voteview object with %d rollcalls", length(votelist)))
  pb <- txtProgressBar(min = 0, max = length(votelist), style = 3)
  
  # option to replace any that are Missing with newer passes
  
  
  for (i in 1:length(votelist)) {
    for (member in votelist[[i]]$votes) {
      vname <- paste0("V", i)
      # Find member from roll call in output data
      memberpos <- fmatch(member$icpsr, data$votematrix$icpsr)
      # If the legislator data is not entered yet, enter it as well as the vote
      if (is.na(data$votematrix$name[memberpos])) {
        vdat <- member[votedatanames]
        vdat[sapply(vdat, is.null)] <- NA
        data$votematrix[memberpos, c(votedatanames, vname)] <- c(unlist(vdat), member$vote)
      } else { # else, just enter the vote
        data$votematrix[memberpos, vname] <- member$vote
      }
    }
    
    # If vote is NA, replace it with 0 for not in legislature
    data$votematrix[, paste0("V", i)] <- ifelse(is.na(data$votematrix[, vname]),
                                                0,
                                                data$votematrix[, vname])
    
    # Add rollcall data
    data$rollcalls[i, rollcalldatanames] <- c(votelist[[i]][rollcalldatanames])
    setTxtProgressBar(pb, i)
  }
  
  data$vmNames <- NULL
  data$rcNames <- NULL
  data$unretrievedids <- dat$unretrievedids
  class(data) <- "voteview"
  
  return(data)
}

# Function to restart interrupted voteview_download processes
#' Complete rollcall download
#' @export
#' 
complete_download <- function(dat) {
  
  if(class(dat) != "rollcall") stop("restart_download only takes rollcall objects downloaded from VoteView.")
  if(dat$source != "Download from VoteView") stop("restart_download only takes rollcall objects downloaded from VoteView.")
  
  if(is.null(dat$unretrievedids)) stop("No unretrieved ids associated with this rollcall object.")

  rc_new <- voteview_download(dat$unretrievedids)
  
  return( rc %+% rc_new )
}

# Takes a voteview object and translate it in to a PSCL rollcall object
#' Transforms Voteview object to a rollcall object
#' 
#' Takes a voteview object that has been downloaded using \code{voteview_download} and
#' creates a \code{pscl} \code{rollcall} object that can then be analyzed using
#' methods in the \code{pscl} package.
#' 
#' 
#' @param data A voteview object downloaded from the voteview server using
#' \code{voteview_download}.
#' @return A \code{rollcall} object from the \code{pscl} package.
#' @seealso
#' '\link{voteview_search}','\link{voteview_download}'.
#' @examples
#' 
#' ## NOTE: SIMPLY USE voteview_download INSTEAD
#' ## Search for example roll calls
#' res <- voteview_search("Iraq")
#'   
#' ## Use the ids from that search to download votes and create voteview object
#' vv <- voteview_download(res$id)
#'   
#' ## Get roll call object
#' rc <- voteview2rollcall(vv)
#'   
#' ## Summarize the roll call object
#' summary(rc)
#' summary(rc, verbose = T)
#' @export
#' 
voteview2rollcall <- function(data) {
  #check type of obj

  dat  <- as.matrix(data$votematrix[,grep("^V\\d+", names(data$votematrix))])
  rnames <- sprintf("%s %s - %s", trim(data$votematrix$name),
                                  data$votematrix$cqlabel,
                                  data$votematrix$icpsr)
  rownames(dat) <- rnames
  legis.data <- data$votematrix[, -grep("^V\\d+", names(data$votematrix))]

  rc <- rollcall(data = dat,
                 yea = c(1, 2, 3),
                 nay = c(4, 5, 6),
                 missing = c(NA, 7, 8, 9),
                 notInLegis = 0,
                 legis.data = legis.data,
                 legis.names = rnames,
                 vote.data = data$rollcalls,
                 source = "Download from VoteView")
  
  rc[["unretrievedids"]] <- data$unretrievedids
  return(rc)
}

# Helper function that transforms a vector of lists into a dataframe
#' Transform vector of lists to data frame
#' 
#' This is a helper function to transform the vector of lists that were
#' constructed by \code{voteview_search} in to a data frame that
#' \code{voteview_search} returns. This function should probably not be called
#' by itself. See \code{voteview_search} for more information.
#' 
#' 
#' @param rcs A vector of lists that is built within \code{voteview_search},
#' where each list corresponds to a roll call.
#'
#' @seealso '\link{voteview_search}'.
#' @export
#' 
vlist2df <- function(rcs) {
  df <- list()
  # drop lists from return for now
  notlists <- sapply(rcs[[1]], function(x) class(x) != "list")
  flds <- names(rcs[[1]][notlists])

    for (f in flds) {
    md <- ifelse(class(rcs[[1]][[f]]) == "character", "character", "integer")
    df[[f]] <- vector(mode = md,length = length(rcs))
  }
  
  for (i in 1:length(rcs)) {
     for (f in flds) {
       if(!is.null(rcs[[i]][[f]])) {
         df[[f]][i] <- rcs[[i]][[f]]
       }
     }
  }
  
  # reorder columns explicitly
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  return( df[, c("description", "shortdescription", "date", "bill", "chamber",
                 "session",  "rollnumber", "yea", "nay", "support", "id")])
}

# Eventually this function will properly merge RC files
#' @export
"%+%" <- function(rc1, rc2) {
  
  # todo: input validation
  # todo: naming votes V1, V2, ...
  
  # Identifying vectors
  votes.ids1 <- paste0(rc1$vote.data$chamber, rc1$vote.data$rollnumber)
  votes.ids2 <- paste0(rc2$vote.data$chamber, rc2$vote.data$rollnumber)
  legis.inrc1 <- rc2$legis.data$icpsr %in% rc1$legis.data$icpsr
  new.votes <- !(votes.ids2 %in% votes.ids1)
  
  # Building the new vote matrix
  old.votedat <- data.frame(rc1$votes, icpsr = rc1$legis.data$icpsr)
  new.votedat <- data.frame(rc2$votes[, new.votes], icpsr = rc2$legis.data$icpsr)
  allvote <- merge(old.votedat, new.votedat, by = "icpsr", all = T)
  # todo: What about those who switch parties or any other changes in legis.data
  # what does that look like in the db?
  
  # Building metadata matrices
  allvote.data <- rbind(rc1$vote.data, rc2$vote.data[new.votes, ])
  allvote.data$vote <- paste0("V", 1:nrow(allvote.data))
  
  alllegis.data <- rbind(rc1$legis.data, rc2$legis.data[!legis.inrc1, ])
  alllegis.data <- alllegis.data[match(allvote$icpsr, alllegis.data$icpsr), ]
  
  # Clean up new vote matrix
  allvote <- as.matrix(allvote[, colnames(allvote) != "icpsr"])
  allvote[is.na(allvote)] <-  rc1$codes$notInLegis 
  
  rnames <- sprintf("%s %s - %s", alllegis.data$name,
                    alllegis.data$cqlabel,
                    alllegis.data$icpsr)
  rownames(allvote) <- rnames
  
  # todo: sort votes by date  
  
  rcout <- rollcall(data = allvote,
                  yea = rc1$codes$yea,
                  nay = rc1$codes$nay,
                  missing = rc1$codes$missing,
                  notInLegis = rc1$codes$notInLegis,
                  legis.data = alllegis.data,
                  legis.names = rnames,
                  vote.data = allvote.data,
                  source = "Download from VoteView")
  rcout$unretrievedids <- rc2$unretrievedids #todo: better handling of unretrieved ids

  return(rcout)
  
}