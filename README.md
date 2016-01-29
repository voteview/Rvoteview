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
#> Query 'alltext:Iraq' returned 334 votes...
names(res)
#>  [1] "description"      "shortdescription" "date"            
#>  [4] "bill"             "chamber"          "session"         
#>  [7] "rollnumber"       "yea"              "nay"             
#> [10] "support"          "id"
  
## I will drop description since it is a very long field
head(res[, -1])
#>                             shortdescription       date          bill
#> 1         WITHDRAW FROM IRAQ BY JUNE 30 2008 2007-10-03     H.R. 3222
#> 2                         REDUCE SDI FUNDING 1992-09-17              
#> 3                   INCREASES TAXES FOR IRAQ 2004-06-17          2400
#> 4    CAMPAIGN MEDAL FOR IRAQ AND AFGHANISTAN 2004-05-18       R. 3104
#> 5                   OPPOSE THE SURGE IN IRAQ 2007-02-17        S. 574
#> 6 BUDGET RESOLUTION -- INTERNATIONAL AFFAIRS 2008-03-14 S.Con.Res. 70
#>   chamber session rollnumber yea nay   support       id
#> 1  Senate     110        362  28  69  28.86598 S1100362
#> 2  Senate     102        494  48  51  48.48485 S1020494
#> 3  Senate     108        589  44  53  45.36082 S1080589
#> 4  Senate     108        555  98   0 100.00000 S1080555
#> 5  Senate     110         51  56  35  61.53846 S1100051
#> 6  Senate     110        525  73  23  76.04167 S1100525
```

Using `res$id` we can get a `rollcall` object (from the [`pscl` package](https://cran.r-project.org/web/packages/pscl/index.html)) that contains the full set of votes and data for each roll call. Eventually we will either develop additional methods for the `pscl` `rollcall` object.

``` r
## Get a rollcall object using the ids, please limit to a few ids for now!
rc <- voteview.download(res$id[1:10])
#> No encoding supplied: defaulting to UTF-8.

## Now this object can be used in many 'pscl' methods
summary(rc)
#> Source:       Download from VoteView 
#> 
#> Number of Legislators:        194
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
#>  100  200  328 <NA> 
#>   94   98    2    0 
#> 
#> Vote Summary:
#>                Count Percent
#> 0 (notInLegis)   930    47.9
#> 1 (yea)          664    34.2
#> 6 (nay)          301    15.5
#> 9 (missing)       45     2.3
#> 
#> Use summary(rc,verbose=TRUE) for more detailed information.
```

You can also search by start and end date, session, and chamber. Please see the help files for each function after you install the package to see a little more about how they work.

``` r
## Voteview search with options
res <- voteview.search("Iraq", chamber = "House", session = 110,
                       startdate = 2008, enddate = "2008-04-20")
#> Query 'alltext:Iraq session:110' returned 1 votes...
head(res[, -1])
#>                                                  shortdescription
#> 1 IRAQ & AFGHAN. FALLEN MILITARY HEROES POST OFFICE LOUISVILLE KY
#>         date     bill chamber session rollnumber yea nay support       id
#> 1 2008-02-28 H R 4454   House     110       1263 404   0     100 H1101263

res <- voteview.search("Iraq", session = 109:112)
#> Query 'alltext:Iraq session:109 and 110 and 111 and 112' returned 165 votes...
head(res[, -1])
#>                                                         shortdescription
#> 1                        SUPPLEMENTAL APPS FOR IRAQ & AFGHANISTAN (PROC)
#> 2                   SUPPLEMENTAL APPS FOR IRAQ & AFGHANISTAN -- RECOMMIT
#> 3          SUPPLEMENTAL APPS IRAQ & AFGHANISTAN & KATRINA -- COAST GUARD
#> 4    SUPPLEMENTAL APPS IRAQ & AFGHANISTAN & KATRINA -- ELECTION INFRAST.
#> 5 SUPPLEMENTAL APPS IRAQ & AFGHANISTAN & KATRINA -- COMMUN. BLOCK GRANTS
#> 6 SUPPLEMENTAL APPS IRAQ & AFGHANISTAN & KATRINA -- COMMUN. BLOCK GRANTS
#>         date      bill chamber session rollnumber yea nay  support
#> 1 2005-05-05 H RES 258   House     109        158 224 196 53.33333
#> 2 2005-05-05  H R 1268   House     109        159 201 225 47.18310
#> 3 2006-03-16  H R 4939   House     109        724 208 210 49.76077
#> 4 2006-03-16  H R 4939   House     109        726 194 227 46.08076
#> 5 2006-03-16  H R 4939   House     109        721 210 212 49.76303
#> 6 2006-03-16  H R 4939   House     109        720 174 248 41.23223
#>         id
#> 1 H1090158
#> 2 H1090159
#> 3 H1090724
#> 4 H1090726
#> 5 H1090721
#> 6 H1090720
```
