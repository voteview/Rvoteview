## This file contains the functions used to search for rollcalls and members
## Functions:
##            voteview_search
##            member_search
##            cleanDf
##            jlist2df

# Function to run a voteview query, returning a dataframe of matching votes and
# basic data about those votes
#' Query the Voteview Database for Roll Calls
#' 
#' Searches the Voteview database for roll calls and returns a data frame with
#' bill IDs, the breakdown of voting, and other descriptive information. Takes
#' any one or more of the arguments. See \href{https://github.com/JeffreyBLewis/Rvoteview/wiki/Query-Documentation}{the GitHub Wiki here} for more complete documentation.
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
#' @param minsupport A number specifying the minimum 
#' support allowed for returned votes.
#' @param maxsupport Support is the share of Yea votes 
#' over total Yea and Nay votes. \code{maxsupport} is a number specifying the 
#' maximum support allowed for returned votes.
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
#' \item Other columns that depend on the query. For example \code{score} is the
#' value assigned to a roll call when searching using key words. Higher scores
#' mean better matches for the key words used in the search.
#' \code{voteview} to query the individual congress people's votes and other
#' details.}
#' 
#' Also returned as the "qstring" attribute of the data.frame is the exact
#' query string used in the search that can be copied in to the web interface
#' or used in future queries.
#' }
#' @details
#' This function requires at least one argument. The user can use the \code{q} field either to search across all text fields or to pass a more complicated advanced query. This is essentially like a "search box" where the user can just put in some key words, specific phrases in quotes, or can use notation like "support:[10 to 90]" along with boolean logic to build complicated queries.
#' 
#' Complete documentation for the query syntax can be found at \href{https://github.com/JeffreyBLewis/Rvoteview/wiki/Query-Documentation}{the GitHub Wiki here}. You can also see the vignette for other examples. In general, the following syntax is used, \code{field:specific phrase (field:other phrase OR field:second phrase)}. For example, if you wanted to find votes with "war" and either "iraq" or "afghanistan" in any text field, you could set the query to be \code{"alltext:war AND (alltext:iraq OR alltext:afghanistan)"}. Note that the \code{AND} in the above is redundant as fields are joined by \code{AND} by default. Numeric fields can be searched in a similar way, although users can also use square brackets and "to" for ranges of numbers. For example, the query for all votes about taxes in the 100th to 102nd congress could be expressed either using \code{"alltext:taxes congress:100 OR congress:101 OR congress:102"} or using \code{"alltext:taxes congress:[100 to 102]"}. Furthermore, users can specify exact phrases that they want to search like \code{"description:'estate tax'"}.
#' 
#' The fields that can be searched with text are \code{codes}, \code{codes.Clausen}, \code{codes.Peltzman}, \code{codes.Issue}, \code{description}, \code{shortdescription}, \code{bill}, and \code{alltext}. The code and bill fields are searched exactly using regular expressions while in the other fields words are stemmed and searched anywhere in the field specified (unless the query is in quotes). The fields that can be searched numerically are \code{congress}, \code{yea}, \code{nay}, and \code{support}. Users can also search for stashed votes using the \code{saved} field. Searching by individual legislator will be implemented soon.
#' 
#' @seealso
#' '\link{voteview_download}'.
#' @examples
#' 
#' ## Search for example roll calls
#' res <- voteview_search("Iraq")
#' res
#' 
#' ## Return exact string used in search
#' attr(res, "qstring")
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
#' ## Search for "war on terror" AND iraq
#' res <- voteview_search("description:'war on terror' alltext:iraq")
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
      query_string <- sprintf("(%s)", q)
    }
    ## Replace single quotes ' with double quotes for parser, try to avoid
    ## apostrophes
    query_string <- gsub("(?=[^:\\s])\\'", '\\"', query_string, perl=TRUE)
    query_string <- gsub("\\'(?=[\\s$])", '\\"', query_string, perl=TRUE)
  } else {
    query_string <- "()" # This ensures string does not start with boolean
  }
  
  # Check date is well formatted (2014, "2014-01", "2014-01-30")
  if (!is.null(c(startdate, enddate))) {
    dates <- c(as.character(startdate), as.character(enddate))
    if (length(grep("^[0-9]{4}($|-(0[0-9]|1[0-2])($|-([0-2][0-9]|3[0-1])))",
                    c(startdate, enddate))) != length(dates)){
      stop("A date is formatted incorrectly. Please use yyyy, yyyy-mm, or yyyy-mm-dd format. Note that if months or days are excluded, they default to the earliest values, so '2013' defaults to '2013-01-01'.")
    }
    if (!is.null(startdate)) {
      query_string <- sprintf("%s AND (startdate:%s)", query_string, startdate)
    }
    if (!is.null(enddate)) {
      query_string <- sprintf("%s AND (enddate:%s)", query_string, enddate)
    }
  }
  
  # Check congress within range
  if (!is.null(congress)) {
    
    if (any(congress < 0 | congress > 999)) {
      stop("Congress must be a positive number or vector of positive numbers greater than 0 and less than 1000")
    }
    
    # Add congress to query (if congress is one number, ignores second paste)
    query_string <- sprintf("%s AND (congress:%s)", query_string, paste(congress, collapse = " "))
  }
  
  
  # Check support is in correct range and sensible
  if (!is.null(maxsupport) | !is.null(minsupport)) {
    
    if (any(c(minsupport, maxsupport) < 0 | c(minsupport, maxsupport) > 100)) {
      stop("Min and max support must be between 0 and 100")
    }
    
    if (is.null(minsupport)) minsupport <- 0
    if (is.null(maxsupport)) maxsupport <- 100
    
    if (maxsupport < minsupport) stop("maxsupport must be greater than minsupport")
    
    query_string <- sprintf("%s AND (support:[%s to %s])", query_string, minsupport, maxsupport)
  }
  
  
  # Check input for chamber
  if (!is.null(chamber)) {
    if (!(tolower(chamber) %in% c("house", "senate"))) stop("Chamber must be either 'House' or 'Senate'")
    query_string <- sprintf("%s AND (chamber:%s)", query_string, tolower(chamber))
  }
  
  theurl <- paste0(baseurl(), "/api/search")

  resp <- POST(theurl, body = list(q = stri_escape_unicode(query_string)))
  # If the return is not JSON, print out result to see error
  if (substr(content(resp, as = "text", encoding = "UTF-8"), 1, 1) != "{") {
    stop(content(resp, as = "text", encoding = "UTF-8"))
  }
  
  suppressWarnings(resjson <- fromJSON(content(resp,
                                               as = "text",
                                               encoding = "UTF-8"),
                                       flatten = T))

  message(sprintf("Query '%s' returned %i rollcalls...\n", query_string, resjson$recordcount))
  
  if(!is.null(resjson$errormessage)) warning(resjson$errormessage)
  if(resjson$recordcount == 0) stop("No rollcalls found")

  res <- resjson$rollcalls

  orderCols <- c("id", "congress", "chamber", "rollnumber", "date", "bill",
                 "yea_count", "nay_count", "support", "description",
                 "shortdescription")
  dropCols <- c("result")
  
  attr(res, "qstring") <- query_string
  
  return( cleanDf(res, orderCols, dropCols) )
}

#' Query the Voteview Database for Members
#' 
#' This function allows you to search for various members of congress and 
#' presidents using several key fields.
#' 
#' @param name A string with the name of the member of congress that you would like to search.
#' If it is only one word, it searches by last name. If there are multiple words
#' it uses a text index of all of the name fields and returns the best matches.
#' @param icpsr A string or number with the icpsr number you would like to search
#' by. Can also be the internal id that we use for unique legislator-congress-party records.
#' @param state A string or number that is either be the ICPSR state number, the two letter state code,
#' or the full state name.
#' @param congress A numeric vector of the congresses to constrain the 
#' search to.
#' @param cqlabel A string with the cqlabel "(PA-1)".
#' @param chamber A string that is either "House" or "Senate".
#' @param distinct Either a 0 or a 1. A 0 Returns all records that match your query
#' while a 1 only returns the first record that has a specific icpsr number. Note
#' that our database has multple records per icpsr number as our records are
#' unique to legislator-congress-party while icpsr number construction tries
#' to follow a legislator through congresses and across meaningless party 
#' switches.
#' @return A data.frame with data for members of Congress. The columns will be
#' described in forthcoming data documentation. Importantly, \code{id} is the
#' internal id that our system uses. 
#' 
#' @details 
#' The arguments are joined by an AND command, and all arguments can only be of
#' length 1 except for the congress argument which can take a vector. This method
#' will only return 5000 records at a time.
#' 
#' @seealso
#' '\link{voteview_search}'.
#' @examples
#' 
#' ## Search for Obama
#' res <- member_search("obama")
#' 
#' @export
#' 
member_search <- function(name = NULL,
                          icpsr = NULL,
                          state = NULL,
                          congress = NULL,
                          cqlabel = NULL,
                          chamber = NULL,
                          distinct = 0) {
  
  ## Input validation
  if(is.null(c(name, icpsr, state, congress, cqlabel))) {
    stop("Must specify at least one of (name, icpsr, state, congress, cqlabel).")
  }
  
  if(any(sapply(c(name, icpsr, state, cqlabel, chamber), length) > 1)) {
    stop("All arguments besides congress and distinct only take strings, not vectors of length >= 2.")
  }
  
  ##Check for numeric state 
  if(!is.null(state)) {
    suppressWarnings({stateNum <- as.numeric(state)})
    if(!is.na(stateNum)) {
      data(states, envir=environment())
      stateAbbr <- states[states$stateICPSR == stateNum, "stateMail"]
      if(length(stateAbbr)) {
        state <- stateAbbr
      } else {
        stop("State ICPSR number not found. Consider using two letter state abbreviations instead.")
      }
    }
  }
  
  # Check congress within range
  if (!is.null(congress)) {

    if (any(congress < 0 | congress > 999)) {
      stop("Congress must be a positive number or vector of positive numbers greater than 0 and less than 1000")
    }
    
    # turn vector in to 
    congress <- paste(congress, collapse = " ")
  }
  
  # Check distinct
  if (!(distinct %in% c(0, 1))) {
    stop("Distinct must be either 0 or 1.")
  }

  # todo: should we consider similar search syntax to voteview_search?
  
  theurl <- "https://voteview.polisci.ucla.edu/api/getmembers"
  resp <- POST(theurl, body = lapply(list(name = name,
                                          icpsr = icpsr,
                                          state = state,
                                          congress = congress,
                                          cqlabel = cqlabel,
                                          chamber = chamber,
                                          api = "R",
                                          distinct = distinct),
                                     stri_escape_unicode))

  res <- fromJSON(content(resp,
                          as = "text",
                          encoding = "UTF-8"),
                  flatten = T)$results
  
  orderCols <- c("id", "icpsr", "bioName", "fname", "partyname", "cqlabel")
  
  if(length(res) == 0)
    stop("No results found.")
  
  return( cleanDf(res, orderCols) )
}

# Helper function to order and drop fields
# Note that dropcols drops all columns that start with those characters
cleanDf <- function(df, orderCols = NULL, dropCols = NULL) {
  if(!is.null(dropCols)) {
    dropIndex <- grep(paste0("^(", paste0(dropCols, collapse = "|"), ")"), names(df))
    if(length(dropIndex) != 0) {
      df <- df[, -dropIndex]
    }
  }

  df[, c(intersect(orderCols, names(df)), setdiff(names(df), orderCols))]
}


# Helper function that transforms a vector of lists into a dataframe
#' Transform vector of lists to data frame (DEPRECATED)
#' 
#' This function has been deprecated in favor of using fromJSON in jsonlite. This is a helper function to transform the vector of lists that were
#' constructed by \code{voteview_search} and \code{member_search} in to a data frame.
#' This function should probably not be called
#' by itself. See \code{voteview_search} and \code{member_search} for more information.
#' 
#' 
#' @param rcs A vector of lists that is built in a json download
#' where each list corresponds to a roll call or a member depending on the
#' function that called it.
#' @param ordercols A order of columns so that the returned data.frame is easy
#' to read.
#'
#' @seealso '\link{voteview_search}', '\link{member_search}'.
#' @export
#' 
jlist2df <- function(rcs, ordercols = NULL) {
  
  res <- list()
  # drop lists from return for now
  notlists <- sapply(rcs[[1]], function(x) class(x) != "list")
  flds <- names(rcs[[1]][notlists])
  
  for (f in flds) {
    md <- ifelse(class(rcs[[1]][[f]]) == "character", "character", "integer")
    res[[f]] <- vector(mode = md,length = length(rcs))
  }
  
  for (i in 1:length(rcs)) {
    for (f in flds) {
      if(!is.null(rcs[[i]][[f]])) {
        res[[f]][i] <- rcs[[i]][[f]]
      }
    }
  }
  
  
  
  # reorder columns explicitly
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  
  if(nrow(res) == 0)
    stop("No results found.")
  
  ## Returns it with ordercols first and append remaining data
  return( res[, c(ordercols, setdiff(names(res), ordercols))] )
}