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
#> Warning: package 'Rvoteview' was built under R version 3.2.5
  
res <- voteview_search("Iraq")
#> Query 'Iraq' returned 318 rollcalls...
names(res)
#>  [1] "description"      "shortdescription" "date"            
#>  [4] "bill"             "chamber"          "congress"        
#>  [7] "rollnumber"       "yea"              "nay"             
#> [10] "support"          "id"               "score"
  
## I will drop description since it is a very long field
head(res[, -1])
#>                                       shortdescription       date
#> 1                                    PASS DEFENSE APPS 1992-10-03
#> 2                       END USIA 'WORLDNET' TV PROGRAM 1991-07-29
#> 3                  ABORTION SERVICES OVERSEAS MILITARY 1992-09-18
#> 4                         DEATH PENALTY FOR TERRORISTS 1991-02-20
#> 5                     DEFENSE APPS -- ARMORED VEHICLES 2005-10-05
#> 6 INVOKE CLOTURE ON OMNIBUS TRADE BILL KUWAITI TANKERS 1987-07-14
#>        bill chamber congress rollnumber yea nay  support       id
#> 1             House      102        875 304 100 75.24752 H1020875
#> 2            Senate      102        154  55  43 56.12245 S1020154
#> 3            Senate      102        500  37  55 40.21739 S1020500
#> 4            Senate      102         12  25  72 25.77320 S1020012
#> 5 H.R. 2863  Senate      109        248  56  43 56.56566 S1090248
#> 6            Senate      100        189  53  41 56.38298 S1000189
#>       score
#> 1 0.5045455
#> 2 0.5051020
#> 3 0.5064935
#> 4 0.5104167
#> 5 0.5104167
#> 6 0.5121951
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
#> Number of Legislators:        617
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
#>  354  261    2    0 
#> 
#> Vote Summary:
#>                Count Percent
#> 0 (notInLegis)  4828    78.2
#> 1 (yea)          822    13.3
#> 6 (nay)          453     7.3
#> 9 (missing)       67     1.1
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
#> 1 MH00633102   633 Bennett, Charles Edward   100    43  (FL-3) -0.086
#> 2 MH01077102  1077     Brooks, Jack Bascom   100    49  (TX-9) -0.403
#> 3 MH01087102  1087   Broomfield, William S   200    23 (MI-18)  0.227
#> 4 MH02605102  2605 Dingell, John David Jr.   100    23 (MI-16) -0.449
#> 5 MH03078102  3078    Fascell, Dante Bruno   100    43 (FL-19) -0.401
#> 6 MH06455102  6455    Michel, Robert Henry   200    21 (IL-18)  0.373
#>     nom2
#> 1  0.418
#> 2  0.712
#> 3 -0.302
#> 4  0.325
#> 5  0.235
#> 6 -0.157
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
#>                                shortdescription       date      bill
#> 1              DEFENSE APPS -- ARMORED VEHICLES 2005-10-05 H.R. 2863
#> 2                            IMMIGRATION DEBATE 2005-04-13 H.R. 1268
#> 3                      SUPPORT GENERAL PETRAEUS 2007-09-20 H.R. 1585
#> 4                  SUPPLEMENTAL APPS -- CLOTURE 2007-03-28 H.R. 1591
#> 5                      SUPPLEMENTAL APPS (PASS) 2007-03-29 H.R. 1591
#> 6 SYMPATHY FOR VICTIMS OF CAMP LIBERTY SHOOTING 2009-06-03 H RES 471
#>   chamber congress rollnumber yea nay   support       id     score
#> 1  Senate      109        248  56  43  56.56566 S1090248 0.5104167
#> 2  Senate      109         94  61  38  61.61616 S1090094 0.5131579
#> 3  Senate      110        344  72  25  74.22680 S1100344 0.5151515
#> 4  Senate      110        117  97   0 100.00000 S1100117 0.5263158
#> 5  Senate      110        126  51  48  51.51515 S1100126 0.5294118
#> 6   House      111        298 416   0 100.00000 H1110298 0.5294118
```

We can print out the exact query that the function builds using all of these arguments by retrieving the 'qstring' attribute of the returned data frame:

``` r
attr(res, "qstring")
#> [1] "(Iraq) AND (congress:109 110 111 112)"
```

We can assemble and use these complex queries ourselves. Here's one example where we look for all roll calls with the key words "estate", "death", or "tax" and was held in the 100th to the 114th Congress.

``` r
## Voteview search with options
res <- voteview_search("(alltext:estate death tax congress:[100 to 114])")
#> Query '(alltext:estate death tax congress:[100 to 114])' returned 1665 rollcalls...
head(res[, -1])
#>                                     shortdescription       date bill
#> 1               BUDGET RECONCILIATION--CLINICAL LABS 1995-11-17     
#> 2                         PASS BUDGET RECONCILIATION 1995-11-17     
#> 3 DEFENSE AUTH -- ABORTIONS DEFENSE FACILITIES OVERS 1995-06-15     
#> 4             DEFENSE AUTH -- PROCUREMENT PROCEDURES 1995-06-14     
#> 5                      BUDGET RESOLUTION -- INC CUTS 1994-03-11     
#> 6                  ENFORCE BUDGET AGREEMENT WITH LAW 1997-07-23     
#>   chamber congress rollnumber yea nay  support       id     score
#> 1  Senate      104        583  54  45 54.54545 S1040583 0.5020833
#> 2   House      104        794 237 190 55.50351 H1040794 0.5021552
#> 3   House      104        372 197 230 46.13583 H1040372 0.5021739
#> 4   House      104        362 420   1 99.76247 H1040362 0.5049020
#> 5   House      103        650 165 239 40.84158 H1030650 0.5073529
#> 6   House      105        298 148 279 34.66042 H1050298 0.5073529
tail(res[, -1])
#>                                                  shortdescription
#> 1660                BUDGET RECONCILIATION -- NO RETROACTIVE TAXES
#> 1661                            INCREASE NUMBER OF TAX COLLECTORS
#> 1662                                                             
#> 1663                 SWITCH TAX CUTS TO FUND HIRING MORE TEACHERS
#> 1664 LEGISLATIVE BRANCH APPROPRIATIONS -- REDUCE TAX COMM FUNDING
#> 1665         INDEX CAPITAL GAINS TAX & CUT CAPITAL GAINS TAX RATE
#>            date     bill chamber congress rollnumber yea nay  support
#> 1660 1993-06-24           Senate      103        186  46  52 46.93878
#> 1661 1990-09-11           Senate      101        540  35  64 35.35354
#> 1662 2015-02-13  H R 636   House      114         80 173 241 41.78744
#> 1663 1999-03-25           Senate      106         72  54  45 54.54545
#> 1664 2002-07-18 H R 5121   House      107        826 206 213 49.16468
#> 1665 1991-09-24           Senate      102        202  39  60 39.39394
#>            id     score
#> 1660 S1030186 0.6250000
#> 1661 S1010540 0.6250000
#> 1662 H1140080 0.5625000
#> 1663 S1060072 1.1071429
#> 1664 H1070826 0.5714286
#> 1665 S1020202 0.9166667
```

We can also search for all bills with the keywords "war" and "terror" on or after 2008.

``` r
res <- voteview_search("(alltext:war terror startdate:2008-01-01)")
#> Query '(alltext:war terror startdate:2008-01-01)' returned 102 rollcalls...
head(res[, -1])
#>                                             shortdescription       date
#> 1               USE OF CAPITOL VISITOR CENTER TO AWARD MEDAL 2011-09-21
#> 2                                                            2015-06-17
#> 3 SALTUE NATIONAL GUARD HELP MODERNIZE FOREIGN AGR PRACTICES 2010-03-21
#> 4                          NO GROUND FORCES IN LIBYA -- RULE 2011-06-03
#> 5     ECONOMIC STIMULUS PACKAGE -- DEFENSE DEPT. PROCUREMENT 2009-02-04
#> 6                NO MILITARY FORCES IN COMBAT IN IRAQ (PASS) 2014-07-25
#>            bill chamber congress rollnumber yea nay   support       id
#> 1  S CON RES 28   House      112        713 424   0 100.00000 H1120713
#> 2  H CON RES 55   House      114        369 139 287  32.62911 H1140369
#> 3    H RES 1075   House      111       1143 418   3  99.28741 H1111143
#> 4     H RES 294   House      112        408 257 156  62.22760 H1120408
#> 5        H.R. 1  Senate      111         41  38  59  39.17526 S1110041
#> 6 H CON RES 105   House      113       1090 370  40  90.24390 H1131090
#>       score
#> 1 0.5156250
#> 2 0.5161290
#> 3 0.5185185
#> 4 0.5135135
#> 5 0.5185185
#> 6 0.5208333
tail(res[, -1])
#>                                              shortdescription       date
#> 97         DEFENSE APPS -- REDUCE SPENDING ON AFGHANISTAN WAR 2011-07-07
#> 98         DEFENSE APPS -- REDUCE SPENDING ON AFGHANISTAN WAR 2011-07-07
#> 99   MILITARY CONSTRUCTION & VA APPS -- WAR POWERS RESOLUTION 2011-06-13
#> 100       COMPENSATE FILIPINO VETERANS OF WORLD WAR II (PASS) 2008-09-23
#> 101 DEFENSE APPS -- NO FUNDS FOR AFGHAN WAR AFTER 31 DEC 2014 2014-06-19
#> 102 DEFENSE APPS -- NO FUNDS FOR AFGHAN WAR AFTER 31 DEC 2014 2014-06-20
#>         bill chamber congress rollnumber yea nay  support       id
#> 97  H R 2219   House      112        500 133 295 31.07477 H1120500
#> 98  H R 2219   House      112        503 114 314 26.63551 H1120503
#> 99  H R 2055   House      112        413 248 163 60.34063 H1120413
#> 100 H R 6897   House      110       1799 392  23 94.45783 H1101799
#> 101 H R 4870   House      113        968 157 260 37.64988 H1130968
#> 102 H R 4870   House      113        970 153 260 37.04600 H1130970
#>         score
#> 97  0.5833333
#> 98  0.5833333
#> 99  0.5714286
#> 100 0.5714286
#> 101 0.5625000
#> 102 0.5625000
```

You can also search for member data using the `member_search` function.

``` r
res <- member_search("Paul",
                     state = "KY")

## Drop the bio field because it is quite long
head(res[, names(res) != "bio"])
#>           id icpsr                   bioName        fname  partyname
#> 1 MS41104114 41104                PAUL, Rand   Paul, Rand Republican
#> 2 MS41104113 41104                PAUL, Rand   Paul, Rand Republican
#> 3 MS41104112 41104                PAUL, Rand   Paul, Rand Republican
#> 4 MS15406111 15406 BUNNING, James Paul David Bunning, Jim Republican
#> 5 MS15406110 15406 BUNNING, James Paul David Bunning, Jim Republican
#> 6 MS15406109 15406 BUNNING, James Paul David Bunning, Jim Republican
#>   cqlabel stateName born startdate stateAbbr lastmeans occupancy state
#> 1    (KY)  Kentucky 1963         0        KY         1         0    51
#> 2    (KY)  Kentucky 1963         0        KY         1         0    51
#> 3    (KY)  Kentucky 1963         0        KY         1         0    51
#> 4    (KY)  Kentucky 1931         0        KY         1         0    51
#> 5    (KY)  Kentucky 1931         0        KY         1         0    51
#> 6    (KY)  Kentucky 1931         0        KY         1         0    51
#>   party                    website districtCode congress bioNameChanged
#> 1   200 http://www.paul.senate.gov            0      114              0
#> 2   200 http://www.paul.senate.gov            0      113              0
#> 3   200 http://www.paul.senate.gov            0      112              0
#> 4   200                                       0      111              0
#> 5   200                                       0      110              0
#> 6   200                                       0      109              0
#>   voteCount                bioNameOld geo enddate    name chamber
#> 1       443                PAUL, Rand   0       0    PAUL  Senate
#> 2       639                PAUL, Rand   0       0    PAUL  Senate
#> 3       462                PAUL, Rand   0       0    PAUL  Senate
#> 4       647 BUNNING, James Paul David   0       0 BUNNING  Senate
#> 5       636 BUNNING, James Paul David   0       0 BUNNING  Senate
#> 6       625 BUNNING, James Paul David   0       0 BUNNING  Senate
#>   dimweight stateCode
#> 1    0.4156         0
#> 2    0.4156         0
#> 3    0.4156         0
#> 4    0.4156         0
#> 5    0.4156         0
#> 6    0.4156         0

res <- member_search("Paul",
                     state = "KY",
                     distinct = 1)

## Drop the bio field because it is quite long
head(res[, names(res) != "bio"])
#>           id icpsr                   bioName        fname  partyname
#> 1 MS41104114 41104                PAUL, Rand   Paul, Rand Republican
#> 2 MS15406111 15406 BUNNING, James Paul David Bunning, Jim Republican
#>   cqlabel stateName born startdate stateAbbr lastmeans occupancy state
#> 1    (KY)  Kentucky 1963         0        KY         1         0    51
#> 2    (KY)  Kentucky 1931         0        KY         1         0    51
#>   party                    website districtCode congress bioNameChanged
#> 1   200 http://www.paul.senate.gov            0      114              0
#> 2   200                                       0      111              0
#>   voteCount                bioNameOld geo enddate    name chamber
#> 1       443                PAUL, Rand   0       0    PAUL  Senate
#> 2       647 BUNNING, James Paul David   0       0 BUNNING  Senate
#>   dimweight stateCode
#> 1    0.4156         0
#> 2    0.4156         0
```
