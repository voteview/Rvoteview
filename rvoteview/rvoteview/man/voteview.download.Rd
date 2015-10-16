\name{voteview.download}
\alias{voteview.download}
\title{
  Download JSON from voteview server
  }
\description{
  Takes roll call IDs and returns a JSON string that contains data on the description, date, all individual legislator's votes, and more on each roll call.
}
\usage{
  voteview.download(ids)
}
\arguments{
  \item{ids}{
    A string or vector of strings, where each string is a roll call ID, such as "S1110326". These ids can be found using the \code{voteview.search} function, if they are not already known.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A single string of JSON code.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}

\seealso{
  '\link{voteview.search}','\link{voteview2rollcall}'.
}
\examples{
  # Search for sample roll calls
  res <- voteview.search("Iraq")
  # Use the ids from that search to create a json string
  json <- voteview.download(res$ids)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
