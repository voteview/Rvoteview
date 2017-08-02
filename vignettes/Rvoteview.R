## ----echo=F--------------------------------------------------------------
knitr::opts_chunk$set(comment = "#>", fig.align='center',
                      fig.width = 7, fig.height = 5)
library(Rvoteview)

## ----install, eval=F-----------------------------------------------------
#  devtools::install_github("JeffreyBLewis/Rvoteview")

## ----search-ex1, eval=F--------------------------------------------------
#  library(Rvoteview)
#  res <- voteview_search("'terrorism'") # exact
#  res <- voteview_search("terrorism")   # index based

## ----search-ex2, eval=F--------------------------------------------------
#  res <- voteview_search("terrorism iraq") # index based search

## ----search-ex2-alltext, eval=F------------------------------------------
#  res <- voteview_search("alltext:terrorism iraq")

## ----search-ex3, eval=F--------------------------------------------------
#  res <- voteview_search("vote_desc:'iraq'")

## ----search-ex4, results='hide'------------------------------------------
## Search for votes with a start date
## Note that because tax is not in quotes, it searches the text index and not for
## exact matches
res <- voteview_search("tax", startdate = "2005-01-01")

## Search for votes with an end date in just the House
res <- voteview_search("tax", enddate = "2005-01-01", chamber = "House")

## Search for votes with a start date in just the house in the 110th or 112th Congress
res <- voteview_search("tax",
                       startdate = "2000-12-20",
                       congress = c(110, 112),
                       chamber = "House")

## ----attribute-----------------------------------------------------------
attr(res, "qstring")

## ----or-text-------------------------------------------------------------
qString <- "alltext:war iraq (enddate:1993 OR startdate:2000)"
res <- voteview_search(q = qString)


## ----or-text-2-----------------------------------------------------------
qString <- "alltext:tax iraq (congress:[100 to 102] AND support:[45 to 55])"
res <- voteview_search(q = qString)

## ----echo=F--------------------------------------------------------------
summary(rc)

## ---- include=F----------------------------------------------------------
try({detach("package:ggplot2", unload=TRUE)}, silent = T)

## ----outerjoin, results='hide', message=F, warning=F---------------------
## Search all votes with exact phrase "estate tax"
res <- voteview_search("'estate tax' congress:105")

## Download first 10 votes
rc1 <- voteview_download(res$id[1:10])
## Download another 10 votes with some overlap
rc2 <- voteview_download(res$id[5:14])

## Merge them together
rcall <- rc1 %+% rc2

rcall$m # The number of total votes

## ----echo=F--------------------------------------------------------------
rcall$m

## ----melt-rollcall-------------------------------------------------------
## Default is to retain all data
rc_long <- melt_rollcall(rcall)
rc_long[1:3, ]

## Retaining fewer columns
rc_long <- melt_rollcall(rcall, votecols = c("chamber", "congress"))
rc_long[1:3, ]

## ----failed-dld, eval = F------------------------------------------------
#  rc_fail <- voteview_download(res$id)

## ----complete-dld, eval = F----------------------------------------------
#  rc <- complete_download(rc_fail)

## ----member-search-ex----------------------------------------------------
clintons <- member_search("clinton")

## Drop the bio field because it is quite long
clintons[1:7, names(clintons) != "bio"]

## ----member-search-ex-distinct-------------------------------------------
clintons <- member_search("clinton",
                          state = "NY",
                          distinct = 1)

## Drop the bio field because it is quite long
clintons[, names(clintons) != "bio"]

## ----bernie--------------------------------------------------------------
sanders <- member_search("sanders",
                         state = "VT")

## Drop the bio field because it is quite long
sanders[, names(sanders) != "bio"]

