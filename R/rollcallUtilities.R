## This file contains additional functions for editing rollcall objects
## Functions:
##            melt_rollcall
##            %+%

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
#' \item{\code{id} }{Legislator ids from rollcall object}
#' \item{\code{vname} }{Vote names from rollcall object}
#' \item{\code{vote} }{Numeric vote for legislator-rollcall pair}
#' \item{\code{votecols} }{All columns selected from \code{vote.data}}
#' \item{\code{legiscols} }{All columns selected from \code{legis.data}}
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
  
  # Check vote names (keepvote)
  if(length(intersect(keepvote, rownames(rc$vote.data))) == 0) {
    stop("No votes requested by keepvote are found in rollcall object")
  }
  if(length(setdiff(keepvote, rownames(rc$vote.data))) > 0) {
    warning("Argument keepvote has some votes not found in rollcall object.")
    
    keepvote <- intersect(keepvote, rownames(rc$vote.data))
  }
  
  ## If long versions of the data are available, use them
  if(!is.null(rc$votes.long)) {
    
    ## If no specified columns, take all of them
    if(is.null(votecols)) {
      votelongcols <- colnames(rc$votes.long)
      votecols <- colnames(rc$vote.data)
    } else {
      ## Otherwise, find the ones they specified if they exist and warn if they do not
      votecolsMissing <- setdiff(votecols, c(colnames(rc$vote.data), colnames(rc$legis.long.dynamic)))
      if(length(votecolsMissing) > 0)
        warning(sprintf("Following specified votecols (%s) not found in the rollcall object.", paste0(votecolsMissing, collapse = ", ")))
      
      votelongcols <- intersect(colnames(rc$legis.long.dynamic), votecols)
      votecols <- intersect(colnames(rc$vote.data), votecols)
    }
    ## If no specified columns, take all of them
    if(is.null(legiscols)) {
      legiscols <- colnames(rc$legis.long.dynamic)
    } else {
      ## Otherwise, find the ones they specified if they exist and warn if they do not
      legiscolsMissing <- setdiff(legiscols, c(colnames(rc$legis.long.dynamic)))
      if(length(legiscolsMissing) > 0)
        warning(sprintf("Following specified legiscols (%s) not found in the rollcall object.", paste0(legiscolsMissing, collapse = ", ")))
      
      legiscols <- intersect(colnames(rc$legis.long.dynamic), legiscols)
    }
    
    ## Only keep icpsr once
    if(("icpsr" %in% legiscols) & ("icpsr" %in% votecols)) 
      legiscols <- setdiff(legiscols, "icpsr")
    
    ## Make sure to always keep id
    if(!("id" %in% legiscols))
      legiscols <- c("id", legiscols)
    
    ## Only keep votes user wants
    votedat <- rc$votes.long[rc$votes.long$vname %in% keepvote,
                             names(rc$votes.long) != "icpsr",
                             drop = F] 
    ## Otherwise icpsr duplicates in merge below
    
    long_rc <- merge(votedat, rc$legis.long.dynamic[, legiscols, drop = F],
                     by = "id")
    long_rc <- merge(long_rc, rc$vote.data[, unique(c(votecols, "vname")), drop = F],
                     by = "vname", sort = F)
    
    return(long_rc[, unique(c("id", "vname", "vote", votecols, votelongcols, legiscols))])
  } else { # If keeplong = F
    
    votes <- rc$votes[, keepvote, drop = F]
    
    ## If not user specified, return all legislator metadata
    if(is.null(legiscols)) {
      legiscols <- setdiff(colnames(rc$legis.data), "icpsr")
    } else {
      ## If specified, check for any requests not in data, return all metadata found
      legiscolsMissing <- setdiff(legiscols, colnames(rc$legis.data))
      if(length(legiscolsMissing) > 0)
        warning(sprintf("Following specified legiscols (%s) not found in the rollcall object.", paste0(legiscolsMissing, collapse = ", ")))
      
      legiscols <- intersect(colnames(rc$legis.data), legiscols)
    }
    
    ## If not user specified, return all roll call metadata
    if(is.null(votecols)) {
      votecols <- colnames(rc$vote.data)
    } else {
      ## If specified, check for any requests not in data, return all metadata found
      votecolsMissing <- setdiff(votecols, colnames(rc$vote.data))
      if(length(votecolsMissing) > 0)
        warning(sprintf("Following specified votecols (%s) not found in the rollcall object.", paste0(votecolsMissing, collapse = ", ")))
      
      votecols <- intersect(colnames(rc$vote.data), votecols)
    }
    
    # Modified from reshape2, Hadley Wickham
    # https://github.com/hadley/reshape
    dn <- dimnames(votes)
    names(dn) <- c("icpsr", "vname")
    
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
    long_rc <- merge(long_rc, rc$legis.data[, legiscols, drop = F],
                     by.x = "icpsr", by.y = "row.names")
    
    # Add roll call data
    long_rc <- merge(long_rc, rc$vote.data[, unique(c("vname", votecols)), drop = F],
                     by = "vname", sort = F)
    
    return(long_rc[, c("icpsr", "vname", "vote", votecols, legiscols)])
  }
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
    
    # merge back in data that was deduplicated in to legis.data
    newlegis.long.dynamic <- rbind(rc1$legis.long.dynamic[-idsfound[!is.na(idsfound)], ],
                                   rc2$legis.long.dynamic)
    
    uniqueicpsr <- unique(newlegis.long.dynamic$icpsr)
    
    legiscols <-  c("name", "state_abbrev", "party_code", "dim1", "dim2")
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
    rcout$legis.long.dynamic <- newlegis.long.dynamic[, c(!(names(newlegis.long.dynamic) %in% c('dim1', 'dim2')))]
    return(rcout)
  }
}