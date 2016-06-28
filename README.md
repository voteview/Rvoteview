<!-- README.md is generated from README.Rmd. Please edit that file -->
Rvoteview
=========

**WARNING: This package is under construction. Please be patient and feel free to contact [Luke Sonnet](mailto:luke.sonnet@gmail.com) with any questions.**

This is a package that enables you to query the Voteview database for roll calls and work with data frames or a `pscl` `rollcall` object.

To install this package, ensure you have `devtools` installed. If you do not, run `install.packages("devtools")` before doing the following:

``` r
devtools::install_github("JeffreyBLewis/Rvoteview")
```

For more thorough documentation, see the help files for individual functions and the vignette [at this link here](https://github.com/JeffreyBLewis/Rvoteview/tree/master/vignettes). Lastly, there is complete documentation for the searches you can make [at the wiki here](https://github.com/JeffreyBLewis/Rvoteview/wiki/Query-Documentation).

Quick Start: Using Rvoteview
----------------------------

To use `Rvoteview`, you generally want to search the database to get a list of vote ids and then use those to return the individual votes. We query the database with a search term and some parameters to constrain the search. The default behavior is to search any words as key words, returning the roll calls that best match any key words you enter. Again, for full documentation for the search [see here](https://github.com/JeffreyBLewis/Rvoteview/wiki/Query-Documentation). There are further examples in the [vignette](https://github.com/JeffreyBLewis/Rvoteview/tree/master/vignettes).

So let's start with a search for roll calls with the key word "Iraq".

``` r
library(Rvoteview)
  
res <- voteview_search("Iraq")
#> Query 'Iraq' returned 318 rollcalls...
names(res)
#>  [1] "description"      "shortdescription" "date"            
#>  [4] "bill"             "chamber"          "congress"        
#>  [7] "rollnumber"       "yea"              "nay"             
#> [10] "support"          "id"               "score"
  
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

You also have lots of metadata on roll calls and legislators in various data frames. For example, we can see some legislator metadata:

``` r
head(rcAll$legis.long.dynamic)
#>           id icpsr                    name party state cqlabel   nom1
#> 1 MH00633101   633 Bennett, Charles Edward   100    43  (FL-3) -0.086
#> 2 MH01077101  1077     Brooks, Jack Bascom   100    49  (TX-9) -0.402
#> 3 MH01087101  1087   Broomfield, William S   200    23 (MI-18)  0.227
#> 4 MH02009101  2009      Conte, Silvio Otto   200     3  (MA-1) -0.025
#> 5 MH02605101  2605 Dingell, John David Jr.   100    23 (MI-16) -0.448
#> 6 MH03078101  3078    Fascell, Dante Bruno   100    43 (FL-19) -0.400
#>     nom2
#> 1  0.412
#> 2  0.707
#> 3 -0.305
#> 4 -0.677
#> 5  0.321
#> 6  0.230
```

You can also search by start and end date, congress, and chamber. Please see the help files for each function after you install the package to see a little more about how they work.

``` r
## Voteview search with options
res <- voteview_search("Iraq", chamber = "House", congress = 110,
                       startdate = 2008, enddate = "2008-04-20")
#> Query '(Iraq) AND (startdate:2008) AND (enddate:2008-04-20) AND (congress:110) AND (chamber:house)' returned 1 rollcalls...
head(res[, -1])
#>                                                  shortdescription
#> 1 IRAQ & AFGHAN. FALLEN MILITARY HEROES POST OFFICE LOUISVILLE KY
#>         date     bill chamber congress rollnumber yea nay support       id
#> 1 2008-02-28 H R 4454   House      110       1263 404   0     100 H1101263
#>      score
#> 1 1.105556

res <- voteview_search("Iraq", congress = 109:112)
#> Query '(Iraq) AND (congress:109 110 111 112)' returned 157 rollcalls...
head(res[, -1])
#>                      shortdescription       date      bill chamber
#> 1                  WITHDRAW FROM IRAQ 2005-11-15   S. 1042  Senate
#> 2                  WITHDRAW FROM IRAQ 2005-11-15   S. 1042  Senate
#> 3   WITHDRAW FROM IRAQ BY JULY 1 2007 2006-06-22   S. 2766  Senate
#> 4    FORMULATE IRAQ WITHDRAWAL POLICY 2006-06-15   S. 2766  Senate
#> 5   HAS CIVIL WAR BROKEN OUT IN IRAQ? 2006-09-06 H.R. 5631  Senate
#> 6 NO PERMANENT MILITARY BASES IN IRAQ 2007-07-25  H R 2929   House
#>   congress rollnumber yea nay  support       id    score
#> 1      109        322  40  58 40.81633 S1090322 1.593750
#> 2      109        323  79  19 80.61224 S1090323 1.593750
#> 3      109        547  13  87 13.00000 S1090547 1.589130
#> 4      109        540  93   6 93.93939 S1090540 1.468750
#> 5      109        599  54  44 55.10204 S1090599 1.458333
#> 6      110        711 399  24 94.32624 H1100711 1.437500
```

We can also build complex queries manually. Here's one example where we look for all roll calls with the key words "estate", "death", or "tax" and was held in the 100th to the 114th Congress.

``` r
## Voteview search with options
res <- voteview_search("(alltext:estate death tax congress:[100 to 114])")
#> Query '(alltext:estate death tax congress:[100 to 114])' returned 1661 rollcalls...
head(res[, -1])
#>                         shortdescription       date              bill
#> 1        BUDGET RESOLUTION -- ESTATE TAX 2007-03-23     S.Con.Res. 21
#> 2                TAX CUT -- ESTATE TAXES 2001-05-21 1836        S.AMD
#> 3 TAX CUT -- CREDIT FOR STATE ESTATE TAX 2001-05-22 1836        S.AMD
#> 4                  TAX CUT -- ESTATE TAX 2001-05-22 1836        S.AMD
#> 5    BUDGET RECONCILIATION--ESTATE TAXES 1995-10-27                  
#> 6        BUDGET RESOLUTION -- ESTATE TAX 2009-04-02     S.Con.Res. 13
#>   chamber congress rollnumber yea nay  support       id    score
#> 1  Senate      110        102  48  51 48.48485 S1100102 3.625000
#> 2  Senate      107        120  39  60 39.39394 S1070120 3.450000
#> 3  Senate      107        150  42  57 42.42424 S1070150 3.442029
#> 4  Senate      107        135  48  51 48.48485 S1070135 3.392857
#> 5  Senate      104        546  72  27 72.72727 S1040546 3.154412
#> 6  Senate      111        147  56  43 56.56566 S1110147 3.128049
tail(res[, -1])
#>                          shortdescription       date bill chamber congress
#> 1656 BUDGET RECONCILIATION--CLINICAL LABS 1995-11-17       Senate      104
#> 1657  WELFARE REFORM -- EN BLOC AMENDMENT 1995-03-22        House      104
#> 1658       OUTLAW CERTAIN ASSAULT WEAPONS 1993-11-17       Senate      103
#> 1659    JUVENILES DRUGS AND GANG ACTIVITY 1993-11-09       Senate      103
#> 1660                    CHILD PORNOGRAPHY 1993-11-04       Senate      103
#> 1661             CRIME CONTROL AMENDMENTS 1994-04-14        House      103
#>      rollnumber yea nay   support       id     score
#> 1656        583  54  45  54.54545 S1040583 0.5020833
#> 1657        250 249 177  58.45070 H1040250 0.5017668
#> 1658        375  57  43  57.00000 S1030375 0.5015873
#> 1659        360  60  38  61.22449 S1030360 0.5015674
#> 1660        350 100   0 100.00000 S1030350 0.5015432
#> 1661        698 390  25  93.97590 H1030698 0.5013298
```

We can also search for all bills with the keywords "war" and "terror" on or after 2008.

``` r
res <- voteview_search("(alltext:war terror startdate:2008-01-01)")
#> Query '(alltext:war terror startdate:2008-01-01)' returned 99 rollcalls...
head(res[, -1])
#>                                  shortdescription       date      bill
#> 1 SALUTE 69TH INFANTRY REGIMENT FOR WAR ON TERROR 2008-03-13 H RES 991
#> 2          ACQUIRE REVOLUTIONARY & 1812 WAR SITES 2009-03-03   H R 146
#> 3                      TERRORISM INSURANCE (PASS) 2014-12-10    S 2244
#> 4                      TERRORISM INSURANCE (PASS) 2014-07-17   S. 2244
#> 5          PRESERVE CIVIL WAR BATTLEFIELDS (PASS) 2013-04-09  H R 1033
#> 6                         HONOR COLD WAR VETERANS 2010-03-21 H RES 900
#>   chamber congress rollnumber yea nay   support       id    score
#> 1   House      110       1317 406   0 100.00000 H1101317 2.243590
#> 2   House      111         90 394  13  96.80590 H1110090 1.564286
#> 3   House      113       1195 417   7  98.34906 H1131195 1.532051
#> 4  Senate      113        522  93   4  95.87629 S1130522 1.523810
#> 5   House      113         90 283 122  69.87654 H1130090 1.443750
#> 6   House      111       1146 429   0 100.00000 H1111146 1.426724
tail(res[, -1])
#>                                shortdescription       date         bill
#> 94                                              2015-06-17 H CON RES 55
#> 95 USE OF CAPITOL VISITOR CENTER TO AWARD MEDAL 2011-09-21 S CON RES 28
#> 96   EXTEND CERTAIN SECTIONS PATRIOT ACT (PASS) 2011-02-15     H.R. 514
#> 97            NO GROUND FORCES IN LIBYA -- RULE 2011-06-03    H RES 294
#> 98                                              2016-01-12    H RES 583
#> 99                                              2016-01-12    H RES 583
#>    chamber congress rollnumber yea nay   support       id     score
#> 94   House      114        369 139 288  32.55269 H1140369 0.5161290
#> 95   House      112        713 424   0 100.00000 H1120713 0.5156250
#> 96  Senate      112         19  86  12  87.75510 S1120019 0.5147059
#> 97   House      112        408 257 156  62.22760 H1120408 0.5135135
#> 98   House      114        738 233 173  57.38916 H1140738 0.5135135
#> 99   House      114        739 239 183  56.63507 H1140739 0.5135135
```
