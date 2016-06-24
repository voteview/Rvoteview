## ----echo=F--------------------------------------------------------------
knitr::opts_chunk$set(comment = "#>", fig.align='center',
                      fig.width = 7, fig.height = 5)
library(Rvoteview)

## ----install, eval=F-----------------------------------------------------
#  devtools::install_github("JeffreyBLewis/Rvoteview")

## ----search-ex1, eval=F--------------------------------------------------
#  library(Rvoteview)
#  res <- voteview_search("'terrorism'")
#  ## 164 found rollcalls
#  res <- voteview_search("terrorism")
#  ## 238 found rollcalls

## ----search-ex2, eval=F--------------------------------------------------
#  res <- voteview_search("terrorism iraq")
#  ## 546 found rollcalls

## ----search-ex2-alltext, eval=F------------------------------------------
#  res <- voteview_search("alltext:terrorism iraq")
#  ## 546 found rollcalls

## ----search-ex3, eval=F--------------------------------------------------
#  res <- voteview_search("description:'terror' description:'iraq'")
#  ## 11 found rollcalls

## ----search-ex4, eval=F--------------------------------------------------
#  ## Search for votes with a start date
#  ## Note that because tax is not in quotes, it searches the text index and not for
#  ## exact matches
#  res <- voteview_search("tax", startdate = "2005-01-01")
#  
#  ## Search for votes with an end date in just the House
#  res <- voteview_search("tax", enddate = "2005-01-01", chamber = "House")
#  
#  ## Search for votes with a start date in just the house in the 110th or 112th Congress
#  res <- voteview_search("tax",
#                         startdate = "2000-12-20",
#                         congress = c(110, 112),
#                         chamber = "House")

## ----or-text, eval = F---------------------------------------------------
#  qString <- "description:'war' (description:'iraq' OR description:'afghanistan')"
#  res <- voteview_search(q = qString)

## ----adv-search-ex, eval=F-----------------------------------------------
#  ## Search for "war" AND ("iraq" or "afghanistan") in the description field in 2003
#  res <- voteview_search(q = qString,
#                         startdate = "2003-01-01",
#                         enddate = "2003-12-31")

## ----echo=F--------------------------------------------------------------
summary(rc)

## ---- include=F----------------------------------------------------------
try({detach("package:ggplot2", unload=TRUE)}, silent = T)

