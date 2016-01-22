<!-- README.md is generated from README.Rmd. Please edit that file -->
Rvoteview
=========

**WARNING: This package is in flux, as is the server and data. Please limit the number of roll calls you request at a time and be prepared for queries to break or behave strangely. See usage below.**

This is a package that enables you to query the Voteview database for roll calls and work with data frames or a `pscl` `rollcall` object.

To install this package, ensure you have `devtools` installed. If you do not, run `install.packages("devtools")` before doing the following:

``` r
devtools::install_github("JeffreyBLewis/Rvoteview")
```

Using Rvoteview
---------------

To use `Rvoteview`, you generally want to query the database to get a list of vote ids and then use those to return the individual votes. We query the database with a search term like so:

``` r
library(Rvoteview)
  
res <- voteview.search("Iraq")
#> Query 'Iraq' returned 318 votes...
names(res)
#> [1] "descriptionShort" "description"      "no"              
#> [4] "yea"              "chamber"          "session"         
#> [7] "rollnumber"       "date"             "id"
  
## I will drop description since it is a very long field
head(res[, -2])
#>                                       descriptionShort  no yea chamber
#> 1 INVOKE CLOTURE ON OMNIBUS TRADE BILL KUWAITI TANKERS  41  53  Senate
#> 2    SUPPORT A CEASEFIRE & SETTLEMENT IN IRAN-IRAQ WAR   0  96  Senate
#> 3               CONDEMN IRAQ CHEM WEAPONS AGAINST IRAN   0  91  Senate
#> 4              CHINA STOP SELLING MISSILES IRAN & IRAQ   0  97  Senate
#> 5        IRAQ SANCTIONS B/C USE OF CHEM WEAPONS (PASS)  16 389   House
#> 6   FOREIGN AID--US ASST CHINA MISSILE PROG ON CERT. C 237  23   House
#>   session rollnumber       date       id
#> 1     100        189 1987-07-14 S1000189
#> 2     100        231 1987-08-07 S1000231
#> 3     100        621 1988-06-24 S1000621
#> 4     100        678 1988-07-27 S1000678
#> 5     100        828 1988-09-27 H1000828
#> 6     100        850 1988-09-30 H1000850
```

Using `res$id` we can get a `rollcall` object (from the [`pscl` package](https://cran.r-project.org/web/packages/pscl/index.html)) that contains the full set of votes and data for each roll call. Eventually we will either develop additional methods for the `pscl` `rollcall` object.

``` r
## Get a rollcall object using the ids, please limit to a few ids for now!
rc <- voteview.download(res$id[1:10])

## Now this object can be used in many 'pscl' methods
summary(rc)
#> Source:       Download from VoteView 
#> 
#> Number of Legislators:        540
#> Number of Roll Call Votes:    10
#> 
#> 
#> Using the following codes to represent roll call votes:
#> Yea:      1 2 3 
#> Nay:      4 5 6 
#> Abstentions:  NA 7 8 9 
#> Not In Legislature:   0 
#> 
#> Party Composition:
#>  100  200  328  329 <NA> 
#>  311  227    1    1    0 
#> 
#> Vote Summary:
#>                Count Percent
#> 0 (notInLegis)  3728    69.0
#> 1 (yea)         1055    19.5
#> 3 (yea)            2     0.0
#> 4 (nay)            5     0.1
#> 6 (nay)          358     6.6
#> 9 (missing)      252     4.7
#> 
#> Use summary(rc,verbose=TRUE) for more detailed information.
```

Please see the help files for each function after you install the package to see a little more about how they work.
