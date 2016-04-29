<!-- README.md is generated from README.Rmd. Please edit that file -->
Rvoteview
=========

**WARNING: This package is under construction. The server and database are currently being revamped. Not all features work, so please limit usage to the simple kind of examples you see below. Also, please limit the number of roll calls you request at a time and be prepared for queries to break or behave strangely.**

This is a package that enables you to query the Voteview database for roll calls and work with data frames or a `pscl` `rollcall` object.

To install this package, ensure you have `devtools` installed. If you do not, run `install.packages("devtools")` before doing the following:

``` r
devtools::install_github("JeffreyBLewis/Rvoteview")
```

For more thorough documentation, see the help files and the vignette included with the package:

``` r
vignette("voteview-vignette")
```

Using Rvoteview
---------------

To use `Rvoteview`, you generally want to query the database to get a list of vote ids and then use those to return the individual votes. We query the database with a search term and some parameters to constrain the search. For now we take a string that will seach for a bill that has ANY of the words in the string in ANY text field. Additional boolean and targeted searches have been implemented (and will be explained in the vignette and documentation soon).

``` r
library(Rvoteview)
  
res <- voteview_search("Iraq")
#> Iraq
#> No encoding supplied: defaulting to UTF-8.
#> Query 'Iraq' returned 318 rollcalls...
names(res)
#>  [1] "description"      "shortdescription" "date"            
#>  [4] "bill"             "chamber"          "session"         
#>  [7] "rollnumber"       "yea"              "nay"             
#> [10] "support"          "id"
  
## I will drop description since it is a very long field
head(res[, -1])
#>                                             shortdescription       date
#> 1                                     SANCTIONS AGAINST IRAQ 1990-08-02
#> 2                                         WITHDRAW FROM IRAQ 2005-11-15
#> 3                                         WITHDRAW FROM IRAQ 2005-11-15
#> 4                          WITHDRAW FROM IRAQ BY JULY 1 2007 2006-06-22
#> 5 SUPPLEMENT APPS IRAQ & AFGHANISTAN -- INTELLIGENCE ON IRAQ 2003-10-17
#> 6                                               CONDEMN IRAQ 1998-08-03
#>      bill chamber session rollnumber yea nay   support       id
#> 1           House     101        654 417   0 100.00000 H1010654
#> 2 S. 1042  Senate     109        322  40  58  40.81633 S1090322
#> 3 S. 1042  Senate     109        323  79  19  80.61224 S1090323
#> 4 S. 2766  Senate     109        547  13  87  13.00000 S1090547
#> 5    1689  Senate     108        395  67  32  67.67677 S1080395
#> 6           House     105        998 407   6  98.54722 H1050998
```

Using `res$id` we can get a `rollcall` object (from the [`pscl` package](https://cran.r-project.org/web/packages/pscl/index.html)) that contains the full set of votes and data for each roll call.

``` r
## Get a rollcall object using the ids, please limit to a few ids for now!
rc <- voteview_download(res$id[1:10])
#> Downloading 10 rollcalls
#> Reading vote data for 10 rollcalls
#> Building vote matrix
#> Building legis.data matrix
#> Building rollcall object, may take some time...
```

``` r
## Now this object can be used in many 'pscl' methods
summary(rc)
#> Source:       Download from VoteView 
#> 
#> Number of Legislators:        900
#> Number of Roll Call Votes:    10
#> 
#> 
#> Using the following codes to represent roll call votes:
#> Yea:      1 2 3 
#> Nay:      4 5 6 
#> Abstentions:  7 8 9 
#> Not In Legislature:   0 
#> 
#> Party Composition:
#>  100  200  328 <NA> 
#>  480  418    2    0 
#> 
#> Vote Summary:
#>                Count Percent
#> 0 (notInLegis)  6992    77.7
#> 1 (yea)         1623    18.0
#> 3 (yea)            2     0.0
#> 6 (nay)          301     3.3
#> 8 (missing)        1     0.0
#> 9 (missing)       81     0.9
#> 
#> Use summary(rc,verbose=TRUE) for more detailed information.
```

We have also begun extending the `pscl` `rollcall` object by allowing you to do a full outer join on two `rollcall` objects that were built using this package. For example:

``` r
rc2 <- voteview_download(res$id[c(5, 100)])
#> Downloading 2 rollcalls
#> Reading vote data for 2 rollcalls
#> Building vote matrix
#> Building legis.data matrix
#> Building rollcall object, may take some time...
```

``` r
## Now we can merge this with 'rc'
rcAll <- rc %+% rc2

rc$m
#> [1] 10
rc2$m
#> [1] 2
rcAll$m # rc$m + rc2$m - 1 because of the one vote overlap
#> [1] 11
```

You can also search by start and end date, congress, and chamber. Please see the help files for each function after you install the package to see a little more about how they work.

``` r
## Voteview search with options
res <- voteview_search("Iraq", chamber = "House", congress = 110,
                       startdate = 2008, enddate = "2008-04-20")
#> (alltext:Iraq) AND (session:110)
#> No encoding supplied: defaulting to UTF-8.
#> Query '(alltext:Iraq) AND (session:110)' returned 1 rollcalls...
head(res[, -1])
#>                                                  shortdescription
#> 1 IRAQ & AFGHAN. FALLEN MILITARY HEROES POST OFFICE LOUISVILLE KY
#>         date     bill chamber session rollnumber yea nay support       id
#> 1 2008-02-28 H R 4454   House     110       1263 404   0     100 H1101263

res <- voteview_search("Iraq", congress = 109:112)
#> (alltext:Iraq) AND (session:109 110 111 112)
#> No encoding supplied: defaulting to UTF-8.
#> Query '(alltext:Iraq) AND (session:109 110 111 112)' returned 157 rollcalls...
head(res[, -1])
#>                                             shortdescription       date
#> 1 SUPPLEMENTAL APPS IRAQ & AFGHANISTAN & KATRINA -- COLOMBIA 2006-03-16
#> 2                                   OPPOSE THE SURGE IN IRAQ 2007-02-05
#> 3                                   OPPOSE THE SURGE IN IRAQ 2007-02-01
#> 4                           DEFENSE APPS -- ARMORED VEHICLES 2005-10-05
#> 5                            WITHDRAW FROM IRAQ BY JUNE 2009 2008-05-22
#> 6                INVESTIGATE CONTRACTS IN IRAQ & AFGHANISTAN 2005-09-14
#>           bill chamber session rollnumber yea nay  support       id
#> 1     H R 4939   House     109        713 250 172 59.24171 H1090713
#> 2       S. 470  Senate     110         44  49  48 50.51546 S1100044
#> 3 S.Con.Res. 2  Senate     110         43   0  97  0.00000 S1100043
#> 4    H.R. 2863  Senate     109        248  56  43 56.56566 S1090248
#> 5    H.R. 2642  Senate     110        580  34  64 34.69388 S1100580
#> 6    H.R. 2862  Senate     109        228  44  53 45.36082 S1090228
```

We can also build complex queries manually (note session will be replaced with congress as soon as the database is updated).

``` r
## Voteview search with options
res <- voteview_search("((alltext:'estate tax' OR 'death tax') AND session:[100 to 114]) OR (alltext:'property tax' AND session:[95 to 100])")
#> ((alltext:"estate tax" OR "death tax") AND session:[100 to 114]) OR (alltext:"property tax" AND session:[95 to 100])
#> No encoding supplied: defaulting to UTF-8.
#> Query '((alltext:"estate tax" OR "death tax") AND session:[100 to 114]) OR (alltext:"property tax" AND session:[95 to 100])' returned 86 rollcalls...
head(res[, -1])
#>                                             shortdescription       date
#> 1                        BUDGET RECONCILIATION--ESTATE TAXES 1995-10-27
#> 2                                          REPEAL ESTATE TAX 2000-06-09
#> 3 REPEAL ESTATE TAX -- NO REPEAL CERTAIN POLITICAL ORGANIZAT 2000-06-09
#> 4                                          REPEAL ESTATE TAX 2000-09-07
#> 5                 REPEAL ESTATE TAX -- DEMOCRATIC SUBSTITUTE 2000-06-09
#> 6                                   REPEAL ESTATE TAX (PROC) 2000-06-08
#>   bill chamber session rollnumber yea nay  support       id
#> 1       Senate     104        546  72  27 72.72727 S1040546
#> 2        House     106        861 279 137 67.06731 H1060861
#> 3        House     106        860 202 216 48.32536 H1060860
#> 4        House     106       1064 274 157 63.57309 H1061064
#> 5        House     106        859 196 222 46.88995 H1060859
#> 6        House     106        855 225 199 53.06604 H1060855
tail(res[, -1])
#>                                     shortdescription       date      bill
#> 81                                                   2015-04-15 H RES 200
#> 82                                                   2015-04-15 H RES 200
#> 83                                                   2015-04-16  H R 1105
#> 84                                                   2015-03-26      None
#> 85   INCOME TAX CREDIT FOR SUPPORT OF PUBLIC SCHOOLS 1978-08-14   HR12050
#> 86 PROHIB SOME STATE AD V. PROP TAX ON GAS PIPELINES 1988-10-06          
#>    chamber session rollnumber yea nay  support       id
#> 81   House     114        154 242 182 57.07547 H1140154
#> 82   House     114        153 240 183 56.73759 H1140153
#> 83   House     114        159 186 232 44.49761 H1140159
#> 84  Senate     114        114  54  46 54.00000 S1140114
#> 85  Senate      95        950  21  71 22.82609 S0950950
#> 86   House     100        915 219 197 52.64423 H1000915
```
