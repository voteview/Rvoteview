#' @import pscl
#' @import rjson
#' @import httr
#' @import fastmatch
#' 
# Trim white space off of strings
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Function to run a voteview query, returning a dataframe of matching votes and
# basic data about those votes
#' Query the Voteview Database for Roll Calls
#' 
#' Searches the Voteview database for roll calls and returns a data frame with
#' bill IDs, the breakdown of voting, and other descriptive information. Takes
#' any one or more of the arguments.
#' 
#' @param q A string that is passed directly to the Voteview query parser. It
#' can either specify parameters for the search, incorporate complex boolean
#' logic, or be a simple string that will search all text fields. See Details.
#' @param startdate A string of the format \code{"yyyy-mm-dd"} that is the
#' earliest possible date to search for a roll call.
#' @param enddate A string of the format
#' "yyyy-mm-dd" that is the latest possible date to search for a roll
#' call.
#' @param congress A numeric vector of the congresses to constrain the 
#' search to. The default NULL value returns results from all congresses.
#' @param chamber A string in \code{c("House", "Senate")}. The default
#' NULL value returns results from both chambers of congress.
#' @param maxsupport Support is the share of Yea votes 
#' over total Yea and Nay votes. \code{maxsupport} is a number specifying the 
#' maximum support allowed for returned votes.
#' @param minsupport A number specifying the minimum 
#' support allowed for returned votes.
#' @return A data.frame with the following columns: 
#' \itemize{
#' \item{\code{description} }{Official description of the bill.}
#' \item{\code{shortdescription} }{A short description of the bill.}
#' \item{\code{date} }{The date the roll call was held, in string "yyyy-mm-dd"
#' format.}
#' \item{\code{bill} }{Bill name abbreviation.}
#' \item{\code{chamber} }{The chamber the roll call was held in. Either "House"
#' or "Senate"} 
#' \item{\code{congress} }{The congress the roll call was held in.}
#' \item{\code{rollnumber} }{The roll call number of the vote.}
#' \item{\code{yea} }{The total number of 'Yea' votes.}
#' \item{\code{nay} }{The total number of 'Nay' votes.}
#' \item{\code{support} }{Percent of 'Yea' votes out of all 'Yea' or 'Nay' 
#' votes (excludes absent).}
#' \item{\code{id} }{Unique identifier for the roll call that allows
#' \item Other columns that depend on the query
#' \code{voteview} to query the individual congress people's votes and other
#' details.}
#' }
#' @details
#' This function requires at least one argument. The user can use the \code{q} field either to search across all text fields or to pass a more complicated advanced query. This is essentially like a "search box" where the user can just put in some key words, specific phrases in quotes, or can use notation like "support:[10 to 90]" along with boolean logic to build complicated queries.
#' 
#' Complete documentatino is forthcoming for the query syntax. You can also see the vignette for other examples. In general, the following syntax is used, \code{field:specific phrase (field:other phrase OR field:second phrase)}. For example, if you wanted to find votes with "war" and either "iraq" or "afghanistan" in any text field, you could set the query to be \code{"alltext:war AND (alltext:iraq OR alltext:afghanistan)"}. Note that the \code{AND} in the above is redundant as fields are joined by \code{AND} by default. If you wanted to do the same query but only return the votes with "defense" in the description field, the query would become \code{"alltext:war (alltext:iraq OR alltext:afghanistan) description:defense"}. Numeric fields can be searched in a similar way, although users can also use square brackets and "to" for ranges of numbers. For example, the query for all votes about taxes in the 100th to 102nd congress could be expressed either using \code{"alltext:taxes congress:100 OR congress:101 OR congress:102"} or using \code{"alltext:taxes congress:[100 to 102]"}. Note that if you want to restrict search to certain dates, the \code{startdate} and \code{enddate} fields shuld still be used. Furthermore, users can specify exact phrases that they want to search like \code{"budget 'estate tax'"}.
#' 
#' The fields that can be searched with text are \code{codes}, \code{code.Clausen}, \code{code.Peltzman}, \code{code.Issue}, \code{description}, \code{shortdescription}, \code{bill}, and \code{alltext}. The code and bill fields are searched exactly using regular expressions while in the other fields words are stemmed and searched anywhere in the field specified (unless the query is in quotes). The fields that can be searched numerically are \code{congress}, \code{yea}, \code{nay}, and \code{support}. Searching by individual legislator will be implemented soon.
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
#'  
#' ## Search for votes with an end date in just the house
#' res <- voteview_search("Iraq", enddate = "2005-01-01", chamber = "House")
#' 
#' ## Search for votes with a start date in just the house in the 110th or 112th congress
#' res <- voteview_search("Iraq", startdate = "2005-01-01", congress = c(110, 112), chamber = "House")
#' 
#' ## Search for "war on terrorism" AND iraq
#' res <- voteview_search("'war on terrorism' iraq")
#' 
#' }
#' @export
#' 
voteview_search <- function(q = NULL,
                            startdate = NULL,
                            enddate = NULL,
                            chamber = NULL,
                            congress = NULL,
                            maxsupport = NULL,
                            minsupport = NULL) {
  
  # Input validation
  if (is.null(c(q, startdate, enddate, congress, chamber, maxsupport, minsupport))) 
    stop("Must specify at least one argument")
  
  # Start query
  if (!is.null(q)) {
    if (is.null(c(startdate, enddate, congress, chamber, maxsupport, minsupport))) {
      # Query text and no arguments
      query_string <- q
    } else {
      # Query text and arguments
      if (grepl(":", q)) {
        # Query text and arguments and colon
        query_string <- paste0("(", q, ")")
      } else {
        # Query text and arguments and no colon
        # todo: what if text is not first? what if they added alltext to query
        # just advise against this in the intro
        query_string <- paste0("(alltext:", q, ")")
      }
    }
    ## Replace single quotes ' with double quotes for parser, try to avoid
    ## apostrophes
    query_string <- gsub("(?=[^:\\s])\\'", '\\"', query_string, perl=TRUE)
    query_string <- gsub("\\'(?=[\\s$])", '\\"', query_string, perl=TRUE)
  } else {
    query_string <- "()" # This ensures string does not start with boolean
  }
  ## todo:build full query string from options so that user sees full query

  # Check date is well formatted (2014, "2014-01", "2014-01-30")
  if (!is.null(c(startdate, enddate))) {
    dates <- c(as.character(startdate), as.character(enddate))
    if (length(grep("^[0-9]{4}($|-(0[0-9]|1[0-2])($|-([0-2][0-9]|3[0-1])))",
                    c(startdate, enddate))) != length(dates)){
      stop("A date is formatted incorrectly. Please use yyyy, yyyy-mm, or yyyy-mm-dd format. Note that if months or days are excluded, they default to the earliest values, so '2013' defaults to '2013-01-01'.")
    }
  }
  
  # Check congress within range
  if (!is.null(congress)) {
    
    if (any(congress < 0 | congress > 999)) {
      stop("Congress must be a positive number or vector of positive numbers greater than 0 and less than 1000")
    }
    
    # Add congress to query (if congress is one number, ignores second paste)
    query_string <- paste0(query_string, " AND (congress:", paste(congress, collapse = " "), ")")
  }
  
  
  # Check support is in correct range and sensible
  if (!is.null(maxsupport) | !is.null(minsupport)) {
    
    if (any(c(minsupport, maxsupport) < 0 | c(minsupport, maxsupport) > 100)) {
      stop("Min and max support must be between 0 and 100")
    }
    
    if (is.null(minsupport)) minsupport <- 0
    if (is.null(maxsupport)) maxsupport <- 100
    
    if (maxsupport < minsupport) stop("maxsupport must be greater than minsupport")
    
    query_string <- paste0(query_string, " AND (support:[", paste(c(minsupport, maxsupport), collapse = " to "), "])")
  }
  
  
  # Check input for chamber
  if (!is.null(chamber)) {
    if (!(tolower(chamber) %in% c("house", "senate"))) stop("Chamber must be either 'House' or 'Senate'")
  }
  
  message(query_string) # Print out query to user
  
  theurl <- "https://voteview.polisci.ucla.edu/api/search"
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
                                               encoding = "UTF-8")))
  
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
  theurl <- "https://voteview.polisci.ucla.edu/api/download?rollcall_id="
  resp <- GET(paste0(theurl, paste0(ids, collapse = ","), "&apitype=R"), timeout(5))
}

# Function to download roll calls and turn them in to a rollcall object
#' Download rollcalls as a rollcall object
#' 
#' Finds rollcalls by ID and builds a modified \code{rollcall} object from the 
#' \code{pscl} package.
#' 
#' @param ids A character vector of vote ids.
#' @param perrequest An integer that sets the number of votes requested at a
#' time from the voteview database. Default is 15, and is roughly the fastest.
#' Max is 100.
#' @param keeplong A boolean for whether long format of the votes are kept and
#' whether a unique data frame of legislator data is kept. Default is TRUE
#' although this can result in a very large object being returned. See 'Details'
#' for more information.
#' @return A modified \code{pscl} \code{rollcall} object.
#' 
#' @details
#' This function returns a modified \code{rollcall} object. It inherits all of
#' the relevant methods from \code{pscl} but has a few additional features. First,
#' interrupted downloads can be resumed because there is a new 
#' \code{unretrievedids} vector in the \code{rollcall} object, which, if 
#' non-null, can be used in \link{complete_download}. Furthermore, the row names
#' of the vote matrix are icpsr numbers and the column names are the rollcall
#' id numbers that are used in the voteview database.
#' 
#' If \code{keeplong = TRUE}, then the \code{rollcall} object will have two 
#' additional data frames. The \code{votes.long} data frame consists of four 
#' columns, \code{id}, \code{icpsr}, \code{vote}, and \code{vname}, which are
#' a unique id, that legislator's icpsr number, how they voted, and
#' which roll call that vote belongs to. Thus this is a data frame of unique
#' id-rollcalls. Note that icpsr numbers are constant across party
#' changes and across congresses, while \code{id} will always be consistent with
#' party and congress. This is important because we can then map votes to the
#' second additional data frame, \code{legis.long.dynamic}. This data frame
#' consists of data for unique members as identified by \code{id}, and not 
#' \code{icpsr} number. Thus this provides unique covariates by legislator-party-congress. This enables us
#' to also store their congress-specific NOMINATE score. We retain this data frame
#' because the default \code{legis.data} data frame is only unique to icpsr number,
#' which may not have a unique party and is constant across congresss. 
#' 
#' To that end, there is a column \code{ambiguity} in the \code{legis.data} data
#' frame that denotes whether there is some ambiguity in the legislator's record.
#' This means that perhaps across the votes that you downloaded, a legislator changed
#' party, constituency or some other value. When that ambiguity occurs, we fill
#' \code{legis.data} with the modal value in \code{legis.long.dynamic} and set
#' \code{ambiguity} to 1.
#' 
#' @seealso
#' '\link[pscl]{rollcall}','\link{complete_download}','\link{voteview_search}'.
#' @examples
#' 
#' ## Search for example roll calls
#' res <- voteview_search("Rhodesia")
#' 
#' ## Download first 25
#' rc <- voteview_download(res$id[1:25])
#' summary(rc)
#' 
#' \dontrun{
#' ## Lower the number of rollcalls downloaded at a time
#' rc <- voteview_download(res$id[1:25], perrequest = 5)
#' 
#' ## Continue possibly broken/interrupted download
#' complete_rc <- complete_download(rc)
#' }
#' @export
#' 
voteview_download <- function(ids, perrequest = 15, keeplong = T) {
  
  ## Input validation
  if (!is.character(ids)) 
    stop("ids must be a character vector.")
  
  if (perrequest < 1 | perrequest > 100) 
    stop("Max 100 requests at a time. Server will reject higher requests.")
  
  uniqueids <- unique(ids)
  
  if (length(uniqueids) < length(ids)) 
    message("Duplicated ids dropped")
  
  ## Container for list returned from server
  dat <- vector("list", length(uniqueids))
  dat <- build_votelist(dat, uniqueids, perrequest = perrequest)
  
  ## Build voteview and rollcall objects
  vv <- votelist2voteview(dat)
  rc <- voteview2rollcall(vv, keeplong = keeplong)
  
  return( rc )
}

# Function to build voteview object from voteview list
#' Internal function to get rollcalls and build voteview list
#' @export
#' 
build_votelist <- function(votelist, ids, perrequest) {
  
  message(sprintf("Downloading %d rollcalls", length(ids)))
  pb <- txtProgressBar(min = 0, max = length(ids), style = 3)
  
  ## Keep track of failed retrieval
  unretrievedcount <- 0
  unretrievedids <- NULL
  ## Needed so that we do not augment place in output list when a retrieval fails
  place <- 0
  
  idchunks <- split(ids, ceiling(seq_along(ids)/perrequest))
  
  # Download votes
  for (i in 1:length(idchunks)) {
    attempts <- 1
    votes <- "ERROR"
    
    ## Loop that will try to download votes 5 times before throwing error
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
                                     unlist(idchunks[i:length(idchunks)], F, F))))
    }
    
    if (votes[1] == "ERROR") {
      
      errmess <- geterrmessage()
      
      # If it fails on the first try and it can't connect to the server, don't try the rest
      if (i == 1 & errmess == "Couldn't resolve host name") {
        stop(sprintf("Cannot connect to server. No downloads possible."))
      }
      if (errmess == "Couldn't resolve host name") {
        warning(sprintf("Cannot connect to server. Will output the %d completed downloads.",
                        length(unlist(idchunks[1:(i-1)]))))
      } else {
        warning("Unexpected warning: ", errmess)
        warning(sprintf("Will output the %d completed downloads.",
                        length(unlist(idchunks[1:(i-1)]))))
      }
      return(list(votelist = votelist,
                  unretrievedids = c(unretrievedids,
                                     unlist(idchunks[i:length(idchunks)],
                                            use.names = F))))
    } else { # Not an error
      
      # If there are rollcalls to add, add them. This chunk prevents errors
      # when votes$rollcalls doesn't work because votes are all errors
      if(length(votes$rollcalls)) {
        for(j in 1:length(votes$rollcalls)) {
          votelist[[place + j]] <- votes$rollcalls[[j]]
        }
      }
      
      # Change where you are in the outcome list
      place <- place + length(votes$rollcalls)
      if (!is.null(votes$errormessage)) {
        warning(votes$errormessage, call. = FALSE)
      }
      # Add failed ids to the list
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
  
  ## Warn user about failed downloads
  if(is.null(dat$unretrievedids)) {
    votelist <- dat$votelist
  } else{
    warning(sprintf("%d id(s) could not be downloaded", length(dat$unretrievedids)), call. = FALSE)
    warning(paste0("The unretrieved ids are: ", paste(dat$unretrievedids, collapse = ", "), ". Check 'unretrievedids' in your rollcall object. You can use complete_download to try and retrieve these again."), call. = FALSE)
    votelist <- dat$votelist[!sapply(dat$votelist, is.null)]
  }
  
  if(!length(votelist)) stop("Votelist is empty.")
  
  ## Get unique list of members
  ## 'unlist' because if voters are not all the same, then sapply returns list
  ## 'c' in case voters ARE all the same, then sapply returns array
  allmembers <- c(unlist(sapply( votelist, function(vote) sapply(vote$votes, function(member) member$id)), F, F))
  members <- unique(allmembers)
  
  ## Data to keep from return
  ## todo: check if these fields are consistent, what if first return has missing
  ## data. Maybe best fixed server side, as in always return most number of fields
  firstlevelnames <- setdiff(names(votelist[[1]]),
                               c("votes", "apitype", "nominate", "errormessage", "errormeta"))
  ## Use firstlevel names to get sublist names
  rollcalldatanames <- names(unlist(votelist[[1]][firstlevelnames]))
  votelongdatanames <- c("id", "icpsr", "vote")
  legislongdatanames <- setdiff(names(votelist[[1]]$votes[[1]]),
                                c("vote", "id"))
  
  ## use IDs for votenames
  votenames <- c(unlist(sapply(votelist, function(vote) vote$id), F, F))
  
  ## Set up matrices and data frame for rollcall object
  data <- list()
  data$legislong <- matrix(NA,
                           ncol = length(legislongdatanames),
                           nrow = length(members),
                           dimnames = list(NULL,
                                           legislongdatanames))
  data$rollcalls <- data.frame(matrix(NA,
                                      ncol = length(rollcalldatanames) + 2,
                                      nrow = length(votenames),
                                      dimnames = list(NULL,
                                                      c(rollcalldatanames, "nomslope", "nomintercept"))))
  
  
  data$votelong <- matrix(NA,
                          ncol = length(votelongdatanames) + 1,
                          nrow = length(allmembers),
                          dimnames = list(NULL,
                                          c(votelongdatanames, "vname")))
  
  message(sprintf("Reading vote data for %d rollcalls", length(votelist)))
  pb <- txtProgressBar(min = 0, max = length(votelist), style = 3)
  
  ## todo: option to replace any fields that are missing with newer passes over same
  ## legislator or vote. Shouldn't be necessary with high data integrity. 

  votelegis <- 1
  for (i in 1:length(votelist)) {
    for (member in votelist[[i]]$votes) {
      vname <- votelist[[i]]$id
      ## Find member from roll call in output data
      memberpos <- fmatch(member$id, members)
      ## If the legislator icpsr is not entered yet, enter it to the long legis matrix
      if (is.na(data$legislong[memberpos, "icpsr"])) {
        ldat <- member[legislongdatanames]
        ## Replace fields missing in DB with NA
        ldat[sapply(ldat, is.null)] <- NA
        data$legislong[memberpos, ] <- unlist(ldat, F, F)
      } 
      
      data$votelong[votelegis, ] <- c(member$id,
                                      member$icpsr,
                                      ## If vote is NA, replace it with 0 for not in legislature
                                      ## todo: check how missingness is handled in DB
                                      ifelse(is.na(member$vote), 0 , member$vote),
                                      vname)

      votelegis <- votelegis + 1
    }
    
    ## Some votes (like unanimous votes) will not have cut lines
    if(is.null(votelist[[i]]$nominate$slope)) nominate <- c(NA, NA)
    else nominate <- unlist(votelist[[i]]$nominate, F, F)
    
    # Add rollcall data
    data$rollcalls[i, ] <- c(unlist(votelist[[i]])[rollcalldatanames],
                             nominate)
    setTxtProgressBar(pb, i)
  }
  
  data$votelong <- data.frame(data$votelong[1:(votelegis - 1), ], stringsAsFactors = F)
  data$legislong <- data.frame(data$legislong,
                               id = members, stringsAsFactors = F)
  data$vmNames <- NULL
  data$rcNames <- NULL
  data$unretrievedids <- dat$unretrievedids
  class(data) <- "voteview"
  return(data)
}

# Internal function to find mode of vector
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Internal function to transform a voteview object in to a pscl rollcall object
#' Internal function that transforms Voteview object to a rollcall object
#' @export
#' 
voteview2rollcall <- function(data, keeplong = T) {
  ## check type of obj
  uniqueicpsr <- unique(data$legislong$icpsr)
  
  votenames <- data$rollcalls$id

  votemat <- matrix(0,
                    nrow = length(uniqueicpsr),
                    ncol = length(votenames),
                    dimnames = list(uniqueicpsr,
                                    votenames))
  
  votevec <- numeric(length(uniqueicpsr))
  
  
  # This loop fills out vectors of votes for each rollcall and then puts these
  # vectors in to the vote matrix. This is a massive performance boost over
  # filing in votes element-wise
  
  # Get first vote name
  votename <- votenames[1]
  
  message(sprintf("Building vote matrix"))
  pb <- txtProgressBar(min = 0, max = nrow(data$votelong), style = 3)
  
  for( i in 1:nrow(data$votelong)) {

    # Check if vote name is different, implying old vote is finished
    if(data$votelong$vname[i] != votename) {
      # Store old vote vector in overall matrix
      votemat[, votename] <- votevec
      # Re-initialize vote vector
      votevec <- numeric(length(uniqueicpsr))
      # Update vote name
      votename <- data$votelong$vname[i]
    }
    
    # Fill vote vector with votes
    memberpos <- fmatch(data$votelong$icpsr[i], uniqueicpsr)
    votevec[memberpos] <- as.numeric(data$votelong$vote[i])
    setTxtProgressBar(pb, i)
    
  }
  ## Write last vote to column
  votemat[, votename] <- votevec
  
  ## Replace missing with not in legislature value
  votemat[is.na(votemat)] <- 0
  
  legiscols <-  c("name", "state", "cqlabel", "party")
  legis.data <- matrix(NA,
                       nrow = length(uniqueicpsr),
                       ncol = length(legiscols) + 1,
                       dimnames = list(NULL,
                                       c(legiscols, "ambiguity")))
  
  
  
  message(sprintf("Building legis.data matrix"))
  pb <- txtProgressBar(min = 0, max = nrow(legis.data), style = 3)
  
  ## Fill legislator data matrix
  for (i in 1:nrow(legis.data)) {
    memberrows <- fmatch(uniqueicpsr[i], data$legislong$icpsr)
    
    if (length(memberrows) == 1) {
      legis.data[i,] <- c(unlist(data$legislong[memberrows, legiscols], F, F), 0)
    } else {
      legis.data[i,] <- c(apply(data$legislong[, legiscols], 2, Mode), 1)
    }
    setTxtProgressBar(pb, i)
  }
  legis.data <- data.frame(legis.data, icpsr = uniqueicpsr, stringsAsFactors = FALSE)

  ## Rename vote id for consistency
  names(data$rollcalls)[names(data$rollcalls) == "id"] <- "vname"
  
  message(sprintf("Building rollcall object, may take some time..."))
  
  rc <- rollcall(data = votemat,
                 yea = c(1, 2, 3),
                 nay = c(4, 5, 6),
                 missing = c(7, 8, 9),
                 notInLegis = 0,
                 legis.data = legis.data,
                 legis.names = uniqueicpsr,
                 vote.data = data$rollcalls,
                 vote.names = colnames(votemat),
                 source = "Download from VoteView")
  
  rc[["unretrievedids"]] <- data$unretrievedids
  if (keeplong) {
    rc[["votes.long"]] <- data$votelong
    #data$legislong$id <- row.names(data$legislong)
    #row.names(data$legslong) <- NULL
    rc[["legis.long.dynamic"]] <- data$legislong
  }
  
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
  
  if (class(rc) != "rollcall") 
    stop("complete_download only takes rollcall objects downloaded from VoteView.")
  if (rc$source != "Download from VoteView") 
    stop("complete_download only takes rollcall objects downloaded from VoteView.")
  
  if (is.null(rc$unretrievedids)) 
    stop("No unretrieved ids associated with this rollcall object.")
  
  if (is.null(rc$votes.long))
    keeplong <- F
  
  rc_new <- voteview_download(rc$unretrievedids, keeplong = keeplong)
  
  return(rc %+% rc_new)
}

# Forthcoming, function to get member data
# voteview_getmember <- function(id, congress = NULL, perrequest = 20) {
# }

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
#' This will work for all \code{rollcall} objects downloaded from the voteview database.
#' If \code{legis.long.dynamic} and \code{votes.long} are included, meaning \code{keeplong == TRUE},
#' then the melted data frame will be unique by internal id and roll call. Otherwise
#' the melted data frame will be unique by icpsr and roll call.
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
#' rclong <- melt_rollcall(rc, votecols = c("chamber", "congress"))
#' 
#' @export
#' 
melt_rollcall <- function(rc,
                          keepvote = rownames(rc$vote.data),
                          legiscols = NULL,
                          votecols = NULL,
                          dropNotInLegis = TRUE) {
  
  if(class(rc) != "rollcall") stop("melt_rollcall only takes rollcall objects.")
  
  ## If long versions of the data are available, use them
  if(!is.null(rc$votes.long)) {
    
    ## If no specified columns, take all of them
    if(is.null(votecols)) {
      votelongcols <- colnames(rc$votes.long)
      votecols <- colnames(rc$vote.data)
    } else {
      ## Otherwise, find the ones they specified if they exist
      votelongcols <- intersect(colnames(rc$legis.long.dynamic), votecols)
      votecols <- intersect(colnames(rc$vote.data), votecols)
      ## todo: add warning about columns specified by user but not found
    }
    
    if(is.null(legiscols)) {
      legiscols <- colnames(rc$legis.long.dynamic)
    }
    
    ## Only keep icpsr once
    if(("icpsr" %in% legiscols) & ("icpsr" %in% votecols)) 
      legiscols <- setdiff(legiscols, "icpsr")
    
    ## Only keep votes user wants
    votedat <- rc$votes.long[rc$votes.long$vname %in% keepvote,
                             names(rc$votes.long) != "icpsr"] 
    ## Otherwise icpsr duplicates in merge below
    
    long_rc <- merge(votedat, rc$legis.long.dynamic[, legiscols],
                     by = "id")
    long_rc <- merge(long_rc, rc$vote.data[, unique(c(votecols, "vname"))],
                     by = "vname", sort = F)
    
    return(long_rc[, unique(c(votecols, votelongcols, legiscols))])
  } else {
    
    votes <- rc$votes[, keepvote]
    
    if(is.null(legiscols)) legiscols <- colnames(rc$legis.data)
    if(is.null(votecols)) votecols <- colnames(rc$vote.data)
    
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
                     by.x = "voteid", by.y = "vname", sort = F)

    return(long_rc[, c("legisid", "voteid", votecols, legiscols, "vote")])
  }
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
  
  ## To return the following in order
  orderCols <- c("description", "shortdescription", "date", "bill", "chamber",
                 "congress",  "rollnumber", "yea", "nay", "support", "id")
  
  ## Returns it with those columns first in order and append remaining data
  return( df[, c(orderCols, setdiff(names(df), orderCols))] )
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
#' ## Search for rollcalls about Rhodesia in 95th and 96th Congress
#' res <- voteview_search("Rhodesia", congress = c(95, 96))
#' 
#' rc1 <- voteview_download(res$id)
#' summary(rc1)
#' 
#' ## Search for rollcalls about Rhodesia in  90th and 92nd Congress
#' res <- voteview_search("Rhodesia", congress = c(90, 92))
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
  
  # todo: replacing merge with manual match might be faster
  # todo: could be faster if we treated rc2 as smaller bc more searching on that object
  if((class(rc1) != "rollcall") | (class(rc2) != "rollcall")) 
    stop("Both objects must be rollcall objects.")
  if(is.null(rc1$votes.long) != is.null(rc2$votes.long)) 
    stop("Only one rollcall has the long dataframes. Either both or neither may have long dfs; use keeplong in the download function.")
  

   # Building new vote data matrix
   newvotes.data <- merge(rc1$vote.data, rc2$vote.data, all = T)
   
   # Building the new vote matrix
   old.votedat <- data.frame(rc1$votes, icpsr = rownames(rc1$votes), stringsAsFactors = F)
   new.votedat <- data.frame(rc2$votes[, setdiff(colnames(rc2$votes),
                                                 colnames(rc1$votes))],
                             icpsr = rownames(rc2$votes),
                             stringsAsFactors = F)
   
   newvotes <- merge(old.votedat, new.votedat, by = "icpsr", all = T)

   # Building new legis.long.dynamic and legis.data data frames if keeplong
   if(!is.null(rc1$legis.long.dynamic)) {
     idsfound <- fmatch(rc2$legis.long.dynamic$id, rc1$legis.long.dynamic$id)

     newlegis.long.dynamic <- rbind(rc1$legis.long.dynamic[-idsfound[!is.na(idsfound)],],
                                    rc2$legis.long.dynamic)
     

     uniqueicpsr <- unique(newlegis.long.dynamic$icpsr)

     legiscols <-  c("name", "state", "cqlabel", "party")
     alllegis.data <- data.frame(matrix(NA,
                                     nrow = length(uniqueicpsr),
                                     ncol = length(legiscols) + 1,
                                     dimnames = list(uniqueicpsr,
                                                     c(legiscols, "ambiguity"))))
     
     
     
     # Fill 
     for (i in 1:nrow(alllegis.data)) {
       memberrows <- fmatch(uniqueicpsr[i], newlegis.long.dynamic$icpsr)
       
       if (length(memberrows) == 1) {
         alllegis.data[i,] <- c(unlist(newlegis.long.dynamic[memberrows, legiscols], use.names = F), 0)
       } else {
         alllegis.data[i,] <- c(apply(newlegis.long.dynamic[, legiscols], 2, Mode), 1)
       }
     }
     alllegis.data$icpsr <- rownames(alllegis.data)
   } else { # if keeplong = F just keep rc values as default, throw warning
     legis.notinrc1 <- setdiff(rownames(rc2$legis.data), rownames(rc1$legis.data))
     alllegis.data <- rbind(rc1$legis.data, rc2$legis.data[legis.notinrc1, ])
     alllegis.data <- alllegis.data[fmatch(newvotes$icpsr, rownames(alllegis.data)), ]
     alllegis.data$icpsr <- rownames(alllegis.data)
     warning("If overlap in icpsr, values of first rollcall object are preferred for legis.data. Use keeplong = T to ensure this is handled correctly.")
   }
   
   # todo: sort votes by date  
  
   # Clean up new votes matrix
   newvotemat <- as.matrix(newvotes[, colnames(newvotes) != "icpsr"])
   newvotemat[is.na(newvotemat)] <-  rc1$codes$notInLegis 

    rcout <- rollcall(data = newvotemat,
                      yea = rc1$codes$yea,
                      nay = rc1$codes$nay,
                      missing = rc1$codes$missing,
                      notInLegis = rc1$codes$notInLegis,
                      legis.data = alllegis.data,
                      legis.names = newvotes$icpsr,
                      vote.data = newvotes.data,
                      vote.names = colnames(newvotemat),
                      source = "Download from VoteView")
  
    rcout$unretrievedids <- setdiff(unique(c(rc2$unretrievedids, rc1$unretrievedids)),
                                    colnames(newvotemat))
  if(is.null(rc1$votes.long)) {
    return(rcout)
  } else {
    rcout$votes.long <- rbind(rc1$votes.long, rc2$votes.long[!fmatch(paste0(rc2$votes.long$vname, rc2$votes.long$id),
                                                                paste0(rc1$votes.long$vname, rc1$votes.long$id)),])
    rcout$legis.long.dynamic <- newlegis.long.dynamic
    return(rcout)
  }
}