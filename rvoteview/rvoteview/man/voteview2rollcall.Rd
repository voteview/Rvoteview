\name{voteview2rollcall}
\alias{voteview2rollcall}
\title{
  Transforms Voteview JSON to a rollcall object
  }
\description{
  Takes JSON that has been downloaded using \code{voteview.download} and creates a \code{pscl} \code{rollcall} object that can then be analyzed using methods in the \code{pscl} package.
}
\usage{
  voteview2rollcall(json)
}
\arguments{
  \item{json}{
    A string of JSON downloaded from the voteview server using \code{voteview.download}.
  }
}
\value{
  A \code{rollcall} object from the \code{pscl} package.
}
\seealso{
  '\link{voteview.search}','\link{voteview.download}','\link{read.voteview.json}'.
}
\examples{
  ## Search for example roll calls
  res <- voteview.search("Iraq")
  
  ## Use the ids from that search to create a json string
  json <- voteview.download(res$ids)
  
  ## Get roll call object
  rc <- voteview2rollcall(json)
  
  ## Summarize the roll call object
  summary(rc)
  summary(rc, verbose = T)
}