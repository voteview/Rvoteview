## This file contains the functions used to download roll call data
## Functions:
##            voteview_download
##            build_votelist
##            voteview_getvote
##            votelist2voteview
##            voteview2rollcall
##            complete_download
##            download_metadata

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
#' columns, \code{id}, \code{icpsr}, \code{vname}, and \code{vote}, which are
#' a unique id for the legislator-congress, that legislator's icpsr number,
#' which roll call that vote belongs to, and that vote itself. Thus this is a data frame of unique
#' id-rollcalls. Note that icpsr numbers can be constant across party
#' changes and across chambers and congresses, while \code{id} will change with changes in chamber, congress, or party.
#' This is important because we can then map votes to the
#' second additional data frame, \code{legis.long.dynamic}. This data frame
#' consists of data for unique members as identified by \code{id}, and not
#' \code{icpsr} number. Thus this provides unique covariates by legislator-party-congress. This enables us
#' to also store their district number for that congress as well as their name and party_code by congress, should those change over time.
#' We retain this data frame because the default \code{legis.data} data frame is only unique to icpsr number, which can refer to the same person but in different chambers, congresses, districts, and more.
#'
#' To that end, there is a column \code{ambiguity} in the \code{legis.data} data
#' frame that denotes whether there is some ambiguity in the legislator's record.
#' This means that perhaps across the votes that you downloaded, a legislator changed
#' party, constituency or some other value. When that ambiguity occurs, we fill
#' \code{legis.data} with the modal value in \code{legis.long.dynamic} and set
#' \code{ambiguity} to 1.
#'
#' Please note that some rollcalls are coded into two groups by the Issue or Peltzman codes. To deal with this, the code columns in \code{vote.data} data.frame contain concatenated strings where the groups are separated by a semi-colon and a space. For example, if a rollcall has both 'Defense Policy Resolutions' and 'Domestic Social Policy'as Peltzman codes, we return 'Defense Policy Resolutions; Domestic Social Policy' as the Peltzman code for that rollcall.
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

  idchunks <- split(ids, ceiling(seq_along(ids) / perrequest))

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
                         encoding = "UTF-8"))
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
    } else { # Not an error in R

      if (!is.null(votes$errormessage)) stop(votes$errormessage)

      # If there are rollcalls to add, add them. This chunk prevents errors
      # when votes$rollcalls doesn't work because votes are all errors
      if(nrow(votes$rollcalls)) {
        for(j in 1:nrow(votes$rollcalls)) {
          votelist[[place + j]] <- votes$rollcalls[j, ]
        }
      }

      # Change where you are in the outcome list
      place <- place + nrow(votes$rollcalls)
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

# Internal function to get a roll call from the web api
#' Internal function to retrieve roll call json
#' @export
#'
voteview_getvote <- function(ids) {
  theurl <- paste0(baseurl(), "/api/download?rollcall_id=")
  resp <- GET(paste0(theurl, paste0(ids, collapse = ","), "&apitype=R"),
              timeout(100))
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
  allmembers <- c(unlist(sapply( votelist, function(vote) vote$votes[[1]]$id), F, F))
  members <- unique(allmembers)
  ## Data to keep from return
  ## todo: check if these fields are consistent, what if first return has missing
  ## data. Maybe best fixed server side, as in always return most number of fields.
  ## Will increase amount o:x:f data sent but should not slow too much as I can strip
  ## out fields below
  firstlevelnames <- setdiff(names(votelist[[1]]),
                               c("votes", "apitype", "nominate", "errormessage", "errormeta"))
  ## Use firstlevel names to get sublist names (burrow down to get "codes")
  rollcalldatanames <- names(unlist(votelist[[1]][firstlevelnames]))
  votelongdatanames <- c("id", "icpsr", "vote")
  legislongdatanames <- setdiff(names(votelist[[1]]$votes[[1]]),
                                c("cast_code", "id"))

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
    for (x in 1:nrow(votelist[[i]]$votes[[1]])) {
      member <- votelist[[i]]$votes[[1]][x, ]
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
                                      ## only occurs when there is no record for legislator-vote
                                      ## meaning it was a congress the legislator was not in
                                      ifelse(is.na(member$cast_code), 0 , member$cast_code),
                                      votelist[[i]]$id)

      votelegis <- votelegis + 1
    }

    ## Some votes (like unanimous votes) will not have cut lines
    if(is.null(votelist[[i]]$nominate$nomslope)) nominate <- c(NA, NA)
    else nominate <- round(unlist(votelist[[i]]$nominate[c("nomslope", "nomintercept")], F, F), 3)

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

  legiscols <-  c("name", "state_abbrev", "party_code", "dim1", "dim2")
  legis.data <- matrix(NA,
                       nrow = length(uniqueicpsr),
                       ncol = length(legiscols) + 1,
                       dimnames = list(NULL,
                                       c(legiscols, "ambiguity")))

  message(sprintf("Building legis.data matrix"))
  pb <- txtProgressBar(min = 0, max = nrow(legis.data), style = 3)

  data$legislong[, c("dim1", "dim2")]<- apply(data$legislong[, c("dim1", "dim2")],
                                               2,
                                               as.numeric)
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

  ## Change class of some variables
  legis.data$ambiguity <- as.numeric(legis.data$ambiguity)
  data$rollcalls[, c("congress", "yea_count", "nay_count", "nomslope", "nomintercept")] <-
    apply(data$rollcalls[, c("congress", "yea_count", "nay_count", "nomslope", "nomintercept")],
          2,
          as.numeric)
  data$votelong$vote <- as.numeric(data$votelong$vote)


  ## Re-ordering some columns (explicit ordering, additional vars added to the end
  ## by using setdiff)
  ## Try to reorder, if some fields explicitly stated aren't returned, then this will be skipped
  try({
    legis.long.order <- c("id", "icpsr", "name", "party_code", "state_abbrev", "cqlabel")
    legis.long.names <- c(legis.long.order, setdiff(colnames(data$legislong), legis.long.order))
    votes.long.order <- c("id", "icpsr", "vname", "vote")
    votes.long.names <- c(votes.long.order, setdiff(colnames(data$votelong), votes.long.order))
    legis.data.order <- c("icpsr", "name", "party_code", "state_abbrev", "ambiguity", "dim1", "dim2")
    legis.data.names <- c(legis.data.order, setdiff(colnames(legis.data), legis.data.order))
    vote.data.order <- c("vname", "rollnumber", "chamber", "date", "congress", "description", "question", "yea_count", "nay_count", "vote_result",
                         "codes.Issue", "codes.Peltzman", "codes.Clausen",
                         "nomslope", "nomintercept")
    vote.data.names <- c(intersect(vote.data.order, colnames(data$rollcalls)), setdiff(colnames(data$rollcalls), vote.data.order))
  })

  message(sprintf("Building rollcall object, may take some time..."))

  rc <- rollcall(data = votemat,
                 yea = c(1, 2, 3),
                 nay = c(4, 5, 6),
                 missing = c(7, 8, 9),
                 notInLegis = 0,
                 legis.data = legis.data[, legis.data.names],
                 legis.names = uniqueicpsr,
                 vote.data = data$rollcalls[, vote.data.names],
                 vote.names = colnames(votemat),
                 source = "Download from VoteView")

  rc[["unretrievedids"]] <- data$unretrievedids
  if (keeplong) {
    rc[["votes.long"]] <- data$votelong[, votes.long.names]
    #data$legislong$id <- row.names(data$legislong)
    #row.names(data$legislong) <- NULL
    rc[["legis.long.dynamic"]] <- data$legislong[, legis.long.names]
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

  keeplong <- !is.null(rc$votes.long)

  rc_new <- voteview_download(rc$unretrievedids, keeplong = keeplong)

  return(rc %+% rc_new)
}

# Function to download only metadata for either members, rollcalls, or
# parties
#' Download the metadata found on the \href{https://voteview.polisci.ucla.edu/data}{VoteView Data page}
#' for members, rollcalls, or parties.
#'
#' @param type A string that is either 'member', 'rollcall', or 'party'.
#' @param chamber A string that defines the chamber for which data should be
#' returned, either 'both', 'house', or 'senate'. Defaults to 'both' and is
#' ignored if the \code{type} is 'party'.
#' @param congress Either 'all' or a numeric vector that denotes the congresses
#' for which data can be returned. Defaults to 'all'
#'
#' @return A data frame with all relevant metadata.
#'
#' @export
download_metadata <- function(type,
                              chamber = 'both',
                              congress = 'all',
                              usetemp = TRUE) {

  dd_api_url <- paste0(baseurl(), "/api/downloaddata")

  if (type == 'party' & congress != 'all')
    stop('congress must be "all" if type is "party"')

  file_ret <- fromJSON(content(
    POST(url = dd_api_url,
                   body = list(datType = 'csv',
                               type = type,
                               chamber = ifelse(type == 'party',
                                                '',
                                                chamber),
                               congress = ifelse(congress == 'all',
                                                 congress,
                                                 sprintf('%03d', as.numeric(congress))))),
    as = "text",
    encoding = "UTF-8"
  ))

  if ("errorMessage" %in% names(file_ret)) {
    stop(file_ret[["errorMessage"]])
  } else {

    fsplit <- strsplit(file_ret[['file_url']], '/')[[1]]
    fname <- fsplit[length(fsplit)]

    print(file.path(tempdir(), fname))
    if (usetemp && file.exists(file.path(tempdir(), fname))) {
      print('loading')
      return(read.csv(file.path(tempdir(), fname), stringsAsFactors = FALSE))
    }

    file_out <- read.csv(file_ret[['file_url']], stringsAsFactors = FALSE)

    if (usetemp) {
      write.csv(file_out, file = file.path(tempdir(), fname), row.names = FALSE)
    }

    return(file_out)
  }
}
