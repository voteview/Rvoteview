\name{voteview.search}
\alias{voteview.search}
\title{
  Query the Voteview Database with a String
}
\description{
  Searches the Voteview database with a string and returns a data frame with bill IDs, summary vote statistics, and other identifying information.
}
\usage{
  voteview.search(query, startdate = NULL, enddate = NULL, chamber = NULL)
}
\arguments{
  \item{query}{
    A string that will search the voteview database.
  }
  \item{startdate}{
    A string of the format \code{"yyyy-mm-dd"} that is the earliest possible date to search for a roll call. Please note for now that dates are not available for roll calls from the 113 and 114 sessions of congress and those results will be ommitted if you include a start or end date in the query.
  }
  \item{enddate}{
    CURRENTLY NOT FUNCTIONAL. A string of the format \code{"yyyy-mm-dd"} that is the earliest possible date to search for a roll call. Please note for now that dates are not available for roll calls from the 113 and 114 sessions of congress and those results will be ommitted if you include a start or end date in the query.
  }
  \item{chamber}{
    Can be a string in \code{c("House", "Senate")}. The default NULL value returns results from both chambers of congress.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A data.frame with the following columns:
  \itemize{
    \item{\code{descriptionShort}}{A short description of the bill.}
    \item{\code{description}}{Official description of the bill.}
    \item{\code{no}}{The total number of 'Nay' votes}
    \item{\code{yea}}{The total number of 'Yea' votes}
    \item{\code{chamber}}{The chamber the roll call was held in. Either "House" or "Senate"}
    \item{\code{session}}{The session of congress the roll call was held in.}
    \item{\code{rollnumber}}{The roll call number of the vote}
    \item{\code{date}}{The date the roll call was held, in string "yyyy-mm-dd" format.}
    \item{\code{id}}{Unique identifier for the roll call that allows \code{voteview} to query the individual congress people's votes and other details.}
  }
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}

\seealso{
  '\link{voteview.download}','\link{voteview2rollcall}'.
}
\examples{
  # Search for sample roll calls
  res <- voteview.search("Iraq")
  res
  # Search for votes with an end date
  res <- voteview.search("Iraq", enddate = "2008-01-01")
  res
  # Search for votes with an end date in just the house
  res <- voteview.search("Iraq", enddate = "2008-01-01", chamber = "House")
  res
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
