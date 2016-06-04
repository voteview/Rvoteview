<!-- README.md is generated from README.Rmd. Please edit that file -->
Rvoteview
=========

**WARNING: This package is under construction. The server and database are currently being revamped. Some features may break. please limit the number of roll calls you request at a time and be prepared for queries to break or behave strangely.**

This is a package that enables you to query the Voteview database for roll calls and work with data frames or a `pscl` `rollcall` object.

To install this package, ensure you have `devtools` installed. If you do not, run `install.packages("devtools")` before doing the following:

``` r
devtools::install_github("JeffreyBLewis/Rvoteview")
```

For more thorough documentation, see the help files and the vignette included with the package:

``` r
vignette("Rvoteview")
```

You can also find the vignette [at this link here](https://github.com/JeffreyBLewis/Rvoteview/tree/master/vignettes).

Using Rvoteview
---------------

To use `Rvoteview`, you generally want to query the database to get a list of vote ids and then use those to return the individual votes. We query the database with a search term and some parameters to constrain the search. For now we take a string that will seach for a bill that has ANY of the words in the string in ANY text field. Additional boolean and targeted searches have been implemented and can be found in the vignette.

``` r
library(Rvoteview)
  
res <- voteview_search("Iraq")
#> Query 'Iraq' returned 318 rollcalls...
names(res)
#>  [1] "description"      "shortdescription" "date"            
#>  [4] "bill"             "chamber"          "congress"        
#>  [7] "rollnumber"       "yea"              "nay"             
#> [10] "support"          "id"               "score"           
#> [13] "result"
  
## I will drop description since it is a very long field
head(res[, -1])
#>                                             shortdescription       date
#> 1                                     SANCTIONS AGAINST IRAQ 1990-08-02
#> 2                                         WITHDRAW FROM IRAQ 2005-11-15
#> 3                                         WITHDRAW FROM IRAQ 2005-11-15
#> 4                          WITHDRAW FROM IRAQ BY JULY 1 2007 2006-06-22
#> 5 SUPPLEMENT APPS IRAQ & AFGHANISTAN -- INTELLIGENCE ON IRAQ 2003-10-17
#> 6                                               CONDEMN IRAQ 1998-08-03
#>      bill chamber congress rollnumber yea nay   support       id    score
#> 1           House      101        654 417   0 100.00000 H1010654 1.650000
#> 2 S. 1042  Senate      109        322  40  58  40.81633 S1090322 1.593750
#> 3 S. 1042  Senate      109        323  79  19  80.61224 S1090323 1.593750
#> 4 S. 2766  Senate      109        547  13  87  13.00000 S1090547 1.589130
#> 5    1689  Senate      108        395  67  32  67.67677 S1080395 1.541667
#> 6           House      105        998 407   6  98.54722 H1050998 1.539474
#>   result
#> 1      1
#> 2      0
#> 3      1
#> 4      0
#> 5      1
#> 6      1
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
#> Query '(alltext:Iraq) AND (congress:110)' returned 1 rollcalls...
head(res[, -1])
#>                                                  shortdescription
#> 1 IRAQ & AFGHAN. FALLEN MILITARY HEROES POST OFFICE LOUISVILLE KY
#>         date     bill chamber congress rollnumber yea nay support       id
#> 1 2008-02-28 H R 4454   House      110       1263 404   0     100 H1101263
#>   result
#> 1      1

res <- voteview_search("Iraq", congress = 109:112)
#> Query '(alltext:Iraq) AND (congress:109 110 111 112)' returned 157 rollcalls...
head(res[, -1])
#>                                             shortdescription       date
#> 1 SUPPLEMENTAL APPS IRAQ & AFGHANISTAN & KATRINA -- COLOMBIA 2006-03-16
#> 2                                   OPPOSE THE SURGE IN IRAQ 2007-02-05
#> 3                                   OPPOSE THE SURGE IN IRAQ 2007-02-01
#> 4                           DEFENSE APPS -- ARMORED VEHICLES 2005-10-05
#> 5                            WITHDRAW FROM IRAQ BY JUNE 2009 2008-05-22
#> 6                INVESTIGATE CONTRACTS IN IRAQ & AFGHANISTAN 2005-09-14
#>           bill chamber congress rollnumber yea nay  support       id
#> 1     H R 4939   House      109        713 250 172 59.24171 H1090713
#> 2       S. 470  Senate      110         44  49  48 50.51546 S1100044
#> 3 S.Con.Res. 2  Senate      110         43   0  97  0.00000 S1100043
#> 4    H.R. 2863  Senate      109        248  56  43 56.56566 S1090248
#> 5    H.R. 2642  Senate      110        580  34  64 34.69388 S1100580
#> 6    H.R. 2862  Senate      109        228  44  53 45.36082 S1090228
#>   result
#> 1      1
#> 2      1
#> 3      0
#> 4      1
#> 5      0
#> 6      0
```

We can also build complex queries manually.

``` r
## Voteview search with options
res <- voteview_search("(alltext:estate death tax congress:[100 to 114])")
#> Query '(alltext:estate death tax congress:[100 to 114])' returned 1661 rollcalls...
head(res[, -1])
#>                                      shortdescription       date
#> 1                         TAX CUT -- MARRIAGE PENALTY 2001-05-17
#> 2                        ESTATE TAX EXTENSION (TABLE) 2009-12-03
#> 3                             HONOR POPE JOHN-PAUL II 2005-04-05
#> 4                               TAX CUT -- ESTATE TAX 2001-05-22
#> 5                 REDUCE TAX CUT FUND LAW ENFORCEMENT 2003-03-25
#> 6 TAX RELIEF FOR MARRIED COUPLES -- PREVIOUS QUESTION 2002-09-25
#>                bill chamber congress rollnumber yea nay   support       id
#> 1 1836        S.AMD  Senate      107        113  27  73  27.00000 S1070113
#> 2          H R 4154   House      111        925 234 186  55.71429 H1110925
#> 3         S.Res. 95  Senate      109         82  98   0 100.00000 S1090082
#> 4              1836  Senate      107        134  30  69  30.30303 S1070134
#> 5       Con.Res. 23  Senate      108         92  49  50  49.49495 S1080092
#> 6         H RES 547   House      107        919 217 200  52.03837 H1070919
#>   result
#> 1      0
#> 2      1
#> 3      1
#> 4      0
#> 5      0
#> 6      1
tail(res[, -1])
#>                                   shortdescription       date
#> 1656          BUDGET RECONCILIATION--CLINICAL LABS 1995-11-17
#> 1657                   BUDGET RESOLUTION -- AMTRAK 2006-03-15
#> 1658                           PASS LINE ITEM VETO 1995-02-06
#> 1659 TAX DEDUCTION FOR DONATING COMPUTER EQUIPMENT 2000-03-01
#> 1660                      REPEAL ESTATE TAX (PROC) 2006-06-22
#> 1661                     BUSINESS TAXES -- CLOTURE 2004-03-24
#>               bill chamber congress rollnumber yea nay  support       id
#> 1656                Senate      104        583  54  45 54.54545 S1040583
#> 1657 S.Con.Res. 83  Senate      109        417  44  53 45.36082 S1090417
#> 1658                 House      104         90 294 134 68.69159 H1040090
#> 1659                Senate      106        392  96   2 97.95918 S1060392
#> 1660     H RES 885   House      109        976 226 194 53.80952 H1090976
#> 1661          1637  Senate      108        519  51  47 52.04082 S1080519
#>      result
#> 1656      1
#> 1657      0
#> 1658      1
#> 1659      1
#> 1660      1
#> 1661      1
```
