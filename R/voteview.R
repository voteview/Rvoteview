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
#' the voteview datase. All words are searched independently and joined with 
#' an OR statement. Case-insensitive.
#' @param startdate A string of the format \code{"yyyy-mm-dd"} that is the
#' earliest possible date to search for a roll call.
#' @param enddate A string of the format
#' "yyyy-mm-dd" that is the earliest possible date to search for a roll
#' call.
#' @param session A numeric vector of the sessions of congress to constrain the search to. The default is all sessions.
#' @param chamber A string in \code{c("House", "Senate")}. The default
#' NULL value returns results from both chambers of congress.
#' @param maxsupport NOT CURRENTLY SUPPORTED. Support is the share of Yea votes 
#' over total Yea and Nay votes. \code{maxsupport} is a number specifying the 
#' maximum support allowed for returned votes.
#' @param minsupport NOT CURRENTLY SUPPORTED. A number specifying the minimum 
#' support allowed for returned votes.
#' @param query NOT CURRENTLY SUPPORTED. A string that can specify a more 
#' complex search of all of the text fields. See examples for usage and syntax.
#'  Can only be used if \code{alltext} is not specified.
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
#' Each of the fields is currently joined together with an AND statement.
#' 
#' \code{query} uses the following general syntax, \code{field:query AND (query OR query)}. For example, if you wanted to find votes with "war" and either "iraq" or "afghanistan" in any text field, you could set the query to be \code{"alltext:war AND (iraq OR afghanistan)"}. If you wanted to do the same query but only return the votes with "defense" in the description field, the query would become \code{"alltext:war AND (iraq OR afghanistan) description:defense"}
#' 
#' The fields that can be searched with text are \code{code.Clausen}, \code{code.Peltzman}, \code{code.Issue}, \code{description}, \code{shortdescription}, \code{bill}, and \code{alltext}. The fields that can be searched by number are \code{session}, \code{yea}, \code{nay}, and \code{support}.
#' 
#' @seealso
#' '\link{voteview_download}'.
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
    query_string <- paste0("alltext:", paste(alltext, collapse = " "))
  } else {
    query_string <- query
  }
  
  # Check date is well formatted (2014, "2014-01", "2014-01-30")
  dates <- c(as.character(startdate), as.character(enddate))
  if (length(grep("^[0-9]{4}($|-(0[0-9]|1[0-2])($|-([0-2][0-9]|3[0-1])))",
                  c(startdate, enddate))) != length(dates)){
    stop("A date is formatted incorrectly. Please use yyyy, yyyy-mm, or yyyy-mm-dd format")
  }
  
  # Check input for chamber
  if (!is.null(chamber)) {
    if (!(tolower(chamber) %in% c("house", "senate"))) stop("Chamber must be either 'House' or 'Senate'")
  }
  
  # Check session within range
  if (!is.null(session)) {

    if (any(session < 0 | session > 999)) {
      stop("Session must be a positive number or vector of positive numbers greater than 0 and less than 1000")
    }
    
    # Add session to query (if session is one number, ignores second paste)
    query_string <- paste0(query_string, " session:", paste(session, collapse = " "))
  }
  
  # Check support is in correct range and sensible
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
  message(query_string) # Print out query to user
  resp <- POST(theurl, body = list(q = query_string,
                                   startdate = startdate,
                                   enddate = enddate,
                                   chamber = chamber))

  # If the return is not JSON, print out result to see error
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
#' Internal function to retrieve roll call json
#' @export
#' 
voteview_getvote <- function(ids) {
  theurl <- "http://leela.sscnet.ucla.edu/webvoteview/api/getvote/"
  resp <- GET(paste0(theurl, paste0(ids, collapse = ","), "/R"), timeout(5))
}

# Function to download roll calls and turn them in to a rollcall object
#' Download rollcalls as a rollcall object
#' 
#' Finds rollcalls by ID and builds a \code{rollcall} object from the 
#' \code{pscl} package.
#' 
#' @param ids A character vector of vote ids.
#' @param perrequest An integer that sets the number of votes requested at a
#' time from the voteview database. Default is 15, and is roughly the fastest.
#' Max is 100.
#' @return A modified \code{pscl} \code{rollcall} object.
#' 
#' @details
#' This function returns a modified \code{rollcall} object. It inherits all of
#' the relevant methods from \code{pscl} but has two additional features. First,
#' interrupted downloads can be resumed because there is a new 
#' \code{unretrievedids} vector in the \code{rollcall} object, which, if 
#' non-null, can be used in \link{complete_download}. Second, there will
#' eventually be a plot method.
#' 
#' @seealso
#' '\link[pscl]{rollcall}','\link{complete_download}','\link{voteview_search}'.
#' @examples
#' 
#' ## Search for example roll calls
#' res <- voteview_search("Rhodesia")
#' 
#' ## Download first 50
#' rc <- voteview_download(res$id[1:50])
#' summary(rc)
#' 
#' \dontrun{
#' ## Lower the number of rollcalls downloaded at a time
#' rc <- voteview_download(res$id[1:50], perrequest = 5)
#' 
#' ## Continue possibly broken/interrupted download
#' complete_rc <- complete_download(rc)
#' }
#' @export
#' 
voteview_download <- function(ids, perrequest = 15) {
  
  if (!is.character(ids)) stop("ids must be a character vector.")
  
  if (perrequest < 1 | perrequest > 100) stop("Max 100 requests at a time. Server will reject higher requests.")
  
  uniqueids <- unique(ids)

  if (length(uniqueids) < length(ids)) message("Duplicated ids dropped")
  
  dat <- vector("list", length(uniqueids))
  dat <- build_votelist(dat, uniqueids, perrequest = perrequest)

  return( voteview2rollcall(votelist2voteview(dat)) )
}

# Function to build voteview object from voteview list
#' Internal function to get rollcalls and build voteview list
#' @export
#' 
build_votelist <- function(votelist, ids, perrequest) {
  
  message(sprintf("Downloading %d rollcalls", length(ids)))
  pb <- txtProgressBar(min = 0, max = length(ids), style = 3)
  
  unretrievedcount <- 0
  unretrievedids <- NULL
  
  place <- 0
  
  idchunks <- split(ids, ceiling(seq_along(ids)/perrequest))
  
  # Download votes
  for (i in 1:length(idchunks)) {
    attempts <- 1
    votes <- "ERROR"
    
    while (votes[1] == "ERROR" & attempts < 5) {

      
      votes <- tryCatch( {
        resp <- voteview_getvote(idchunks[[i]])
        fromJSON(content(resp,
                         as = "text",
                         encoding = "utf-8"))
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
        return("INTERRUPT")
      })
    }
    
    # Have to return outside the trycatch or else it stores it as votes
    if (votes[1] == "INTERRUPT") {
      return(list(votelist = votelist,
                  unretrievedids = c(unretrievedids,
                                     unlist(idchunks[i:length(idchunks)], 
                                            use.names = F))))
    }
    
    if (votes[1] == "ERROR") {
      
      errmess <- geterrmessage()

      if (i == 1) {
        stop(sprintf("Cannot connect to server. No downloads possible."))
      }
      if ("Couldn't resolve host name" == errmess) {
        warning(sprintf("Cannot connect to server. Will output the %d completed downloads.",
                        length(unlist(idchunks[1:(i-1)]))))
      } else {
        warning("Unexpected warning: ", errmess)
        warning(sprintf("Cannot connect to server. Will output the %d completed downloads.",
                        length(unlist(idchunks[1:(i-1)]))))
      }
      return(list(votelist = votelist,
                  unretrievedids = c(unretrievedids,
                                     unlist(idchunks[i:length(idchunks)],
                                            use.names = F))))
    } else {
      if(length(votes$rollcalls)) {
        found_ids <- setdiff(idchunks[i], unlist(votes$errormeta, use.names = F))
        for(j in 1:length(votes$rollcalls)) {
          votelist[[place + j]] <- votes$rollcalls[[j]]
        }
      }
      place <- place + length(votes$rollcalls)
      if (!is.null(votes$errormessage)) {
        warning(votes$errormessage, call. = FALSE)
      }
      unretrievedids <- c(unretrievedids, unlist(votes$errormeta, use.names = F))
    }
    setTxtProgressBar(pb, place + length(unretrievedids) + length(votes$errormeta))
  }
  
  return(list(votelist = votelist,
              unretrievedids = unretrievedids))
}

# Internal function to build voteview object from voteview list
#' Internal function to build a voteview object from downloaded votelist
#' @export
#' 
votelist2voteview <- function(dat) {
  
  if(is.null(dat$unretrievedids)) {
    votelist <- dat$votelist
  } else{
    warning(sprintf("%d id(s) could not be downloaded", length(dat$unretrievedids)), call. = FALSE)
    warning(paste0("The unretrieved ids are: ", paste(dat$unretrievedids, collapse = ", "), ". Check 'unretrievedids' in your rollcall object. You can use complete_download to try and retrieve these again."), call. = FALSE)
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
                               c("votes", "apitype", "nominate", "errormessage", "errormeta", "id"))
  memberdatanames <- setdiff(names(votelist[[1]]$votes[[1]]),
                           c("y", "x", "vote", "icpsr"))

  # use IDs for votenames
  votenames <- c(unlist(sapply(votelist, function(vote) vote$id)))

  data <- list()
  data$votematrix <- data.frame(matrix(NA,
                                       ncol = length(c(memberdatanames, votenames)),
                                       nrow = length(members),
                                       dimnames = list(as.numeric(members),
                                                       c(memberdatanames, votenames))))
  data$rollcalls <- data.frame(matrix(NA,
                                      ncol = length(rollcalldatanames),
                                      nrow = length(votenames),
                                      dimnames = list(NULL, rollcalldatanames)))

  message(sprintf("Building the voteview object with %d rollcalls", length(votelist)))
  pb <- txtProgressBar(min = 0, max = length(votelist), style = 3)
  
  # option to replace any that are Missing with newer passes
  
  for (i in 1:length(votelist)) {
    for (member in votelist[[i]]$votes) {
      vname <- votelist[[i]]$id
      # Find member from roll call in output data
      memberpos <- fmatch(member$icpsr, row.names(data$votematrix))
      # If the legislator data is not entered yet, enter it as well as the vote
      if (is.na(data$votematrix$name[memberpos])) {
        vdat <- member[memberdatanames]
        vdat[sapply(vdat, is.null)] <- NA
        data$votematrix[memberpos, c(memberdatanames, vname)] <- c(unlist(vdat), member$vote)
      } else { # else, just enter the vote
        data$votematrix[memberpos, vname] <- member$vote
      }
    }
    
    # If vote is NA, replace it with 0 for not in legislature
    data$votematrix[, vname] <- ifelse(is.na(data$votematrix[, vname]),
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


# Internal function to transform a voteview object in to a pscl rollcall object
#' Internal function that transforms Voteview object to a rollcall object
#' @export
#' 
voteview2rollcall <- function(data) {
  #check type of obj
  
  voteindices <- grep("^[HS]\\d+", names(data$votematrix))
  
  dat  <- as.matrix(data$votematrix[, voteindices])
  
  legis.data <- data$votematrix[, -voteindices]
  
  rc <- rollcall(data = dat,
                 yea = c(1, 2, 3),
                 nay = c(4, 5, 6),
                 missing = c(7, 8, 9),
                 notInLegis = 0,
                 legis.data = legis.data,
                 legis.names = rownames(dat),
                 vote.data = data$rollcalls,
                 vote.names = colnames(dat),
                 source = "Download from VoteView")
  
  rc[["unretrievedids"]] <- data$unretrievedids
  return(rc)
}


# Function to restart interrupted voteview_download processes
#' Complete rollcall download if it was interrupted
#' 
#' @param rc A rollcall object downloaded from VoteView that has unretrieved ids.
#' @return A modified \code{pscl} \code{rollcall} object including the original
#' rollcalls and any ones that succeeded the second time.
#' 
#' @seealso
#' '\link[pscl]{rollcall}','\link{complete_download}','\link{voteview_search}'.
#' @examples
#' 
#' ## Search for example roll calls
#' res <- voteview_search("Rhodesia")
#' 
#' ## Download some ids and an invalid id
#' rc <- voteview_download(c(res$id[1:10], "fakeid", res$id[11:15]))
#' summary(rc)
#' 
#' rc$unretrievedids
#' 
#' ## Complete download. Wont work because 'fakeid' will always be invalid,
#' ## however this is just to show usage
#' complete_rc <- complete_download(rc)
#' @export
#' 
complete_download <- function(rc) {
  
  if (class(rc) != "rollcall") stop("complete_download only takes rollcall objects downloaded from VoteView.")
  if (rc$source != "Download from VoteView") stop("complete_download only takes rollcall objects downloaded from VoteView.")
  
  if (is.null(rc$unretrievedids)) stop("No unretrieved ids associated with this rollcall object.")

  rc_new <- voteview_download(rc$unretrievedids)
  
  return(rc %+% rc_new)
}

# Function to turn roll call object into long_rollcall object
#' Melt rollcall object to long rollcall data frame
#' 
#' This function takes a rollcall object and melts it into a long format, merging
#' attributes of the rollcalls and legislators. This can be used for legislator-rollcall
#' analyses.
#' 
#' @param rc A rollcall object.
#' @param keepvote A character vector of vote names to keep. Defaults to all.
#' @param legiscols A character vector of column names from \code{legis.data} to
#' include. Defaults to all columns.
#' @param votecols A character vector of column names from \code{vote.data} to
#' include. Defaults to all columns.
#' @param dropNotInLegis Boolean to drop votes that were "notInLegis". Defaults
#' to TRUE.
#' @return A data.frame with the following columns:
#' \itemize{
#' \item{\code{legisid} }{Legislator names from rollcall object}
#' \item{\code{voteid} }{Vote names from rollcall object}
#' \item{\code{legiscols} }{All columns selected from \code{legis.data}}
#' \item{\code{votecols} }{All columns selected from \code{vote.data}}
#' \item{\code{vote} }{Numeric vote for legislator-rollcall pair}
#' }
#' @details 
#' This function works for any \code{rollcall} object that meets the following
#' two criteria. The \code{vote.names}, or the row names of the \code{vote.data}
#' matrix within the \code{rollcall} object, must match the column names of the 
#' \code{votes} matrix. Similarly, the 
#' \code{legis.names}, or the row names of the \code{legis.data} matrix within 
#' the \code{rollcall} object, must match the row names of the \code{votes} matrix.
#' 
#' @seealso
#' '\link[pscl]{rollcall}'.
#' @examples
#' 
#' ## Search for example roll calls
#' res <- voteview_search("Rhodesia")
#' 
#' ## Download some ids
#' rc <- voteview_download(res$id[1:10])
#' 
#' ## Create long rollcall object, wihtout long description fields
#' rclong <- melt_rollcall(rc, votecols = c("chamber", "session"))
#' 
#' @export
#' 
melt_rollcall <- function(rc,
                          #keepicpsr = rc$legis.data$icpsr,
                          keepvote = rownames(rc$vote.data),
                          legiscols = colnames(rc$legis.data),
                          votecols = colnames(rc$vote.data),
                          dropNotInLegis = TRUE) {
  
  if(class(rc) != "rollcall") stop("melt_rollcall only takes rollcall objects.")
  
  # Only keep votes user wants
  votes <- rc$votes[, keepvote]

  # Modified from reshape2, Hadley Wickham
  # https://github.com/hadley/reshape
  dn <- dimnames(votes)
  names(dn) <- c("legisid", "voteid")
  
  # Build matrix of all legislator-rollcalls
  labels <- expand.grid(dn, KEEP.OUT.ATTRS = FALSE,
                        stringsAsFactors = FALSE)
  
  if (dropNotInLegis) {
    # Drop missing from data
    missing <- votes %in% rc$codes$notInLegis
    votes <- votes[!missing]
    # Drop corresponding rows from matrix of legislator-rollcalls
    labels <- labels[!missing, ]
  }
  
  # Turn votes in to a vector, will have same pattern as labels
  value_df <- setNames(data.frame(as.numeric(votes), stringsAsFactors = FALSE),
                       "vote")

  long_rc <- cbind(labels, value_df)
  
  # Add legislator data
  long_rc <- merge(long_rc, rc$legis.data[, legiscols],
                   by.x = "legisid", by.y = "row.names")
  
  # Add roll call data
  long_rc <- merge(long_rc, rc$vote.data[, votecols],
                   by.x = "voteid", by.y = "row.names")
  
  return(long_rc[, c("legisid", "voteid", votecols, legiscols, "vote")])
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

# Joins two rollcall objects
#' Full outer join of two rollcall objects
#' 
#' This function takes two rollcall objects as its parameters and performs a
#' full outer join on them, returning a new rollcall object that is the union
#' of the two.
#' 
#' @param rc1 The first rollcall object.
#' @param rc2 The second rollcall object.
#' @return The union of the two rollcall objects.
#' 
#' @examples
#' 
#' ## Search for rollcalls about Rhodesia in 100th and 101st Congress
#' res <- voteview_search("Rhodesia", session = c(99, 100))
#' 
#' rc1 <- voteview_download(res$id)
#' summary(rc1)
#' 
#' ## Search for rollcalls about Rhodesia in 101st and 102nd Congress
#' res <- voteview_search("Rhodesia", session = c(100, 101))
#' 
#' rc2 <- voteview_download(res$id)
#' summary(rc2)
#' 
#' ## Union of the two results
#' rc <- rc1 %+% rc2
#' 
#' @export
#' 
"%+%" <- function(rc1, rc2) {
  
  # todo: input validation

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