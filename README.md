<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/JeffreyBLewis/Rvoteview.svg?branch=master)](https://travis-ci.org/JeffreyBLewis/Rvoteview)

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
#> Query 'Iraq' returned 335 rollcalls...
names(res)
#>  [1] "id"                "congress"          "chamber"          
#>  [4] "rollnumber"        "date"              "bill"             
#>  [7] "yea"               "nay"               "support"          
#> [10] "vote_result"       "description"       "short_description"
#> [13] "question"          "text"              "score"            
#> [16] "key_flags"         "codes.Peltzman"    "codes.Clausen"    
#> [19] "codes.Issue"
  
## I will drop description since it is a very long field
head(res[, !(names(res) %in% c("description", "text"))])
#>          id congress chamber rollnumber       date bill yea nay support
#> 1 RH1020819      102   House        819 1992-09-22      394   1   99.75
#> 2 RH1020875      102   House        875 1992-10-03      304 100   75.25
#> 3 RH1020101      102   House        101 1991-05-22      161 265   37.79
#> 4 RS1020154      102  Senate        154 1991-07-29       55  43   56.12
#> 5 RS1020500      102  Senate        500 1992-09-18       37  55   40.22
#> 6 RS1000189      100  Senate        189 1987-07-14       53  41   56.38
#>                 vote_result
#> 1                    Passed
#> 2                    Passed
#> 3                    Failed
#> 4 Motion to Table Agreed to
#> 5        Amendment Rejected
#> 6                      <NA>
#>                                      short_description
#> 1            CLOSE CONF. COMM. DURING CLASSIFIED INFO.
#> 2                                    PASS DEFENSE APPS
#> 3                             TACTICAL MISSILE DEFENSE
#> 4                       END USIA 'WORLDNET' TV PROGRAM
#> 5                  ABORTION SERVICES OVERSEAS MILITARY
#> 6 INVOKE CLOTURE ON OMNIBUS TRADE BILL KUWAITI TANKERS
#>                               question     score key_flags
#> 1       CLOSING PORTIONS OF CONFERENCE 0.5045455      NULL
#> 2 On Agreeing to the Conference Report 0.5045455      NULL
#> 3         On Agreeing to the Amendment 0.5046296      NULL
#> 4               On the Motion to Table 0.5051020      NULL
#> 5                     On the Amendment 0.5064935      NULL
#> 6                                 <NA> 0.5121951      NULL
#>                                      codes.Peltzman
#> 1                             Internal Organization
#> 2                             Defense Policy Budget
#> 3                             Defense Policy Budget
#> 4                             Foreign Policy Budget
#> 5     Defense Policy Budget, Domestic Social Policy
#> 6 Internal Organization, Foreign Policy Resolutions
#>                codes.Clausen                        codes.Issue
#> 1       Miscellaneous Policy                               NULL
#> 2 Foreign and Defense Policy                               NULL
#> 3 Foreign and Defense Policy                               NULL
#> 4 Foreign and Defense Policy                               NULL
#> 5 Foreign and Defense Policy Abortion/Care of deformed newborns
#> 6       Miscellaneous Policy                               NULL
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
#> Number of Legislators:        612
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
#>  350  260    2    0 
#> 
#> Vote Summary:
#>                Count Percent
#> 0 (notInLegis)  4115    67.2
#> 1 (yea)         1255    20.5
#> 6 (nay)          641    10.5
#> 9 (missing)      109     1.8
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
#>           id icpsr                           name party_code state_abbrev
#> 2 MH10215090 15090 CALLAHAN, Herbert Leon (Sonny)        200           AL
#> 3 MH10210717 10717       DICKINSON, William Louis        200           AL
#> 4 MH10215632 15632             BROWDER, John Glen        100           AL
#> 5 MH10211000 11000                    BEVILL, Tom        100           AL
#> 6 MH10229100 29100   CRAMER, Robert E. (Bud), Jr.        100           AL
#> 7 MH10215022 15022                  ERDREICH, Ben        100           AL
#>   cqlabel district_code
#> 2 (AL-01)             1
#> 3 (AL-02)             2
#> 4 (AL-03)             3
#> 5 (AL-04)             4
#> 6 (AL-05)             5
#> 7 (AL-06)             6
```

You can also search by start and end date, congress, and chamber. Please see the help files for each function after you install the package to see a little more about how they work.

``` r
## Voteview search with options
res <- voteview_search("Iraq", chamber = "House", congress = 110,
                       startdate = 2008, enddate = "2008-04-20")
#> Query '(Iraq) AND (startdate:2008) AND (enddate:2008-04-20) AND (congress:110) AND (chamber:house)' returned 1 rollcalls...
head(res[, !(names(res) %in% c("description", "text"))])
#>          id congress chamber rollnumber       date yea nay support
#> 1 RH1101263      110   House       1263 2008-02-28 404   0     100
#>   vote_result
#> 1      Passed
#>                                                 short_description
#> 1 IRAQ & AFGHAN. FALLEN MILITARY HEROES POST OFFICE LOUISVILLE KY
#>                                  question    score          codes.Peltzman
#> 1 On Motion to Suspend the Rules and Pass 1.105556 Government Organization
#>           codes.Clausen
#> 1 Government Management

res <- voteview_search("Iraq", congress = 109:112)
#> Query '(Iraq) AND (congress:109 110 111 112)' returned 163 rollcalls...
head(res[, !(names(res) %in% c("description", "text"))])
#>          id congress chamber rollnumber       date yea nay support
#> 1 RS1090248      109  Senate        248 2005-10-05  56  43   56.57
#> 2 RS1090094      109  Senate         94 2005-04-13  61  38   61.62
#> 3 RS1090603      109  Senate        603 2006-09-07  45  51   46.88
#> 4 RS1100344      110  Senate        344 2007-09-20  72  25   74.23
#> 5 RS1100118      110  Senate        118 2007-03-28  74  23   76.29
#> 6 RS1100122      110  Senate        122 2007-03-29  93   0  100.00
#>              vote_result                                short_description
#> 1        Motion Rejected                 DEFENSE APPS -- ARMORED VEHICLES
#> 2    Amendment Agreed to                               IMMIGRATION DEBATE
#> 3 Motion to Table Failed    DEFENSE APPS -- COMBAT OPIUM AND HEROIN TRADE
#> 4    Amendment Agreed to                         SUPPORT GENERAL PETRAEUS
#> 5    Amendment Agreed to SUPPLEMENTAL APPS -- TIMBER PAYMENTS TO COUNTIES
#> 6    Amendment Agreed to         SUPPLEMENTAL APPS -- TRACK SEX OFFENDERS
#>                 question     score key_flags   codes.Issue
#> 1          On the Motion 0.5125000      NULL          Iran
#> 2       On the Amendment 0.5142857      NULL       Tariffs
#> 3 On the Motion to Table 0.5147059      NULL     Narcotics
#> 4       On the Amendment 0.5166667      NULL          Iran
#> 5       On the Amendment 0.5294118      NULL          NULL
#> 6       On the Amendment 0.5294118      NULL Public Safety
#>                                    codes.Peltzman
#> 1                           Defense Policy Budget
#> 2                           Internal Organization
#> 3                           Defense Policy Budget
#> 4                      Defense Policy Resolutions
#> 5                         Budget Special Interest
#> 6 Domestic Social Policy, Budget Special Interest
#>                codes.Clausen
#> 1 Foreign and Defense Policy
#> 2       Miscellaneous Policy
#> 3 Foreign and Defense Policy
#> 4 Foreign and Defense Policy
#> 5      Government Management
#> 6            Civil Liberties
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
#> Query '(alltext:estate death tax congress:[100 to 114])' returned 1766 rollcalls...
head(res[, !(names(res) %in% c("description", "text"))])
#>          id congress chamber rollnumber       date bill yea nay support
#> 1 RS1040583      104  Senate        583 1995-11-17       54  45   54.55
#> 2 RS1040570      104  Senate        570 1995-11-13       95   1   98.96
#> 3 RH1040802      104   House        802 1995-11-20      235 193   54.91
#> 4 RH1040372      104   House        372 1995-06-15      197 230   46.14
#> 5 RS1040556      104  Senate        556 1995-10-27       52  48   52.00
#> 6 RS1040499      104  Senate        499 1995-10-26       46  53   46.46
#>        vote_result                                  short_description
#> 1  Motion Rejected               BUDGET RECONCILIATION--CLINICAL LABS
#> 2 Motion Agreed to                             NURSING HOME STANDARDS
#> 3           Passed                         PASS BUDGET RECONCILIATION
#> 4           Failed DEFENSE AUTH -- ABORTIONS DEFENSE FACILITIES OVERS
#> 5      Bill Passed                         PASS BUDGET RECONCILIATION
#> 6  Motion Rejected               BUDGET RECONCILIATION--MEDICARE CUTS
#>                                 question     score key_flags
#> 1                          On the Motion 0.5020833      NULL
#> 2                          On the Motion 0.5021097      NULL
#> 3 On motion to agree to Senate Amendment 0.5021552      NULL
#> 4           On Agreeing to the Amendment 0.5021739      NULL
#> 5                 On Passage of the Bill 0.5021739      NULL
#> 6                          On the Motion 0.5022727      NULL
#>                          codes.Issue
#> 1  Budget resolution , Public Health
#> 2                               NULL
#> 3                 Budget resolution 
#> 4 Abortion/Care of deformed newborns
#> 5                 Budget resolution 
#> 6                 Budget resolution 
#>                                        codes.Peltzman
#> 1                             Budget General Interest
#> 2 Domestic Social Policy, Regulation Special Interest
#> 3                             Budget General Interest
#> 4                               Defense Policy Budget
#> 5                             Budget General Interest
#> 6                             Budget General Interest
#>                codes.Clausen
#> 1      Government Management
#> 2             Social Welfare
#> 3      Government Management
#> 4 Foreign and Defense Policy
#> 5      Government Management
#> 6      Government Management
tail(res[, !(names(res) %in% c("description", "text"))])
#>             id congress chamber rollnumber       date bill yea nay support
#> 1761 RS1100415      110  Senate        415 2007-12-06 <NA>  88   5   94.62
#> 1762 RH1101144      110   House       1144 2007-12-12 <NA> 226 194   53.81
#> 1763 RH1101139      110   House       1139 2007-12-12 <NA> 225 191   54.09
#> 1764 RH1101138      110   House       1138 2007-12-12 <NA> 222 193   53.49
#> 1765 RH1100941      110   House        941 2007-10-04 <NA> 386  27   93.46
#> 1766 RH1100940      110   House        940 2007-10-04 <NA> 201 212   48.67
#>      vote_result                  short_description
#> 1761 Bill Passed     ALTERNATIVE MINIMUM TAX (PROC)
#> 1762      Passed     ALTERNATIVE MINIMUM TAX (PASS)
#> 1763      Passed    ALTERNATIVE MINIMUM TAX -- RULE
#> 1764      Passed     ALTERNATIVE MINIMUM TAX (PROC)
#> 1765      Passed      FORECLOSURE TAX RELIEF (PASS)
#> 1766      Failed FORECLOSURE TAX RELIEF -- RECOMMIT
#>                                     question score key_flags
#> 1761                  On Passage of the Bill 0.625      NULL
#> 1762                              On Passage 0.625      NULL
#> 1763           On Agreeing to the Resolution 0.625      NULL
#> 1764       On Ordering the Previous Question 0.625      NULL
#> 1765                              On Passage 0.625      NULL
#> 1766 On Motion to Recommit with Instructions 0.625      NULL
#>               codes.Issue          codes.Peltzman         codes.Clausen
#> 1761            Tax rates Budget Special Interest Government Management
#> 1762            Tax rates Budget Special Interest Government Management
#> 1763            Tax rates Budget Special Interest Government Management
#> 1764            Tax rates Budget Special Interest Government Management
#> 1765 Tax rates, Judiciary Budget Special Interest Government Management
#> 1766 Tax rates, Judiciary Budget Special Interest Government Management
```

We can also search for all bills with the keywords "war" and "terror" on or after 2008.

``` r
res <- voteview_search("(alltext:war terror startdate:2008-01-01)")
#> Query '(alltext:war terror startdate:2008-01-01)' returned 108 rollcalls...
head(res[, -1])
#>   congress chamber rollnumber       date yea nay support
#> 1      112   House        408 2011-06-03 257 156   62.23
#> 2      114   House        368 2015-06-17 139 288   32.55
#> 3      113   House       1090 2014-07-25 370  40   90.24
#> 4      111   House       1081 2010-03-10 225 195   53.57
#> 5      111   House       1453 2010-07-27 222 196   53.11
#> 6      112  Senate        448 2012-11-29  67  29   69.79
#>           vote_result                             short_description
#> 1              Passed             NO GROUND FORCES IN LIBYA -- RULE
#> 2              Failed                                          <NA>
#> 3              Passed   NO MILITARY FORCES IN COMBAT IN IRAQ (PASS)
#> 4              Passed             WITHDRAW FROM AFGHANISTAN -- RULE
#> 5              Passed                 WITHDRAW TROOPS FROM PAKISTAN
#> 6 Amendment Agreed to NO INDEFINITE DETENTION U.S. CITIZENS IN U.S.
#>                                    question
#> 1             On Agreeing to the Resolution
#> 2             On Agreeing to the Resolution
#> 3 On Agreeing to the Resolution, As Amended
#> 4             On Agreeing to the Resolution
#> 5             On Agreeing to the Resolution
#> 6                          On the Amendment
#>                                                                                                                                                                                                                                                                                                                            text
#> 1 Providing for consideration of H.Res. 292 declaring that the President shall not deploy, establish, or maintain the presence of U.S. Armed Forces in Libya, and for consideration of H.Con.Res. 51 directing the President, pursuant to section 5(c) of the War Powers Resolution, to remove the U.S. Armed Forces from Libya
#> 2                                 Directing the President, pursuant to section 5(c) of the War Powers Resolution, to remove United States Armed Forces deployed to Iraq or Syria on or after August 7, 2014, other than Armed Forces required to protect United States diplomatic facilities and personnel, from Iraq and Syria
#> 3                                                                                                Directing the President, pursuant to section 5(c) of the War Powers Resolution, to remove United States Armed Forces, other than Armed Forces required to protect United States diplomatic facilities and personnel, from Iraq
#> 4                                                                                                           Providing for consideration of the concurrent resolution (H.Con.Res. 248) directing the President, pursuant to section 5(c) of the War Powers Resolution, to remove the United States Armed Forces from Afghanistan
#> 5                                                                                                              Providing for consideration of the concurrent resolution (H.Con.Res. 301) directing the President, pursuant to section 5(c) of the War Powers Resolution, to remove the United States Armed Forces from Pakistan
#> 6                                                                                              To clarify that an authorization to use military force, a declaration of war, or any similar authority shall not authorize the detention without charge or trial of a citizen or lawful permanent resident of the United States.
#>       score key_flags                                    codes.Peltzman
#> 1 0.5135135      NULL                        Defense Policy Resolutions
#> 2 0.5161290      NULL                                              NULL
#> 3 0.5208333        CQ Defense Policy Budget, Defense Policy Resolutions
#> 4 0.5217391      NULL                        Defense Policy Resolutions
#> 5 0.5217391      NULL                             Defense Policy Budget
#> 6 0.5238095      NULL                            Domestic Social Policy
#>                codes.Clausen codes.Issue
#> 1 Foreign and Defense Policy        NULL
#> 2                       NULL        NULL
#> 3 Foreign and Defense Policy        Iran
#> 4 Foreign and Defense Policy        NULL
#> 5 Foreign and Defense Policy        NULL
#> 6            Civil Liberties        NULL
tail(res[, -1])
#>     congress chamber rollnumber       date yea nay support
#> 103      112   House        506 2011-07-07 119 306   28.00
#> 104      112   House        505 2011-07-07 145 283   33.88
#> 105      112   House        500 2011-07-07 133 295   31.07
#> 106      110   House       1535 2008-05-22 242 168   59.02
#> 107      113   House        161 2013-05-20 390   3   99.24
#> 108      110  Senate        464 2008-02-13  51  46   52.58
#>                     vote_result
#> 103                      Failed
#> 104                      Failed
#> 105                      Failed
#> 106                   Agreed to
#> 107                      Passed
#> 108 Conference Report Agreed to
#>                                      short_description
#> 103 DEFENSE APPS -- REDUCE SPENDING ON AFGHANISTAN WAR
#> 104 DEFENSE APPS -- REDUCE SPENDING ON AFGHANISTAN WAR
#> 105 DEFENSE APPS -- REDUCE SPENDING ON AFGHANISTAN WAR
#> 106         LONG TERM COSTS OF IRAQ & AFGHANISTAN WARS
#> 107                             MARITIME SAFETY (PASS)
#> 108                   INTERROGATION OF TERROR SUSPECTS
#>                                    question
#> 103            On Agreeing to the Amendment
#> 104            On Agreeing to the Amendment
#> 105            On Agreeing to the Amendment
#> 106            On Agreeing to the Amendment
#> 107 On Motion to Suspend the Rules and Pass
#> 108                On the Conference Report
#>                                                                                                                                                                                                                                                                              text
#> 103                                                                                                                                                                                                                                                  Cohen of Tennessee Amendment
#> 104                                                                                                                                                                                                                                           Cicilline of Rhode Island Amendment
#> 105                                                                                                                                                                                                                                             Garamendi of California Amendment
#> 106                                                                                                                                                                                                                                                      Braley of Iowa Amendment
#> 107                                                                                                                                                                                            Nuclear Terrorism Conventions Implementation and Safety of Maritime Navigation Act
#> 108 A bill to authorize appropriations for fiscal year 2008 for intelligence and intelligence-related activities of the United States Government, the Community Management Account, and the Central Intelligence Agency Retirement and Disability System, and for other purposes.
#>         score key_flags             codes.Peltzman
#> 103 0.5833333      NULL      Defense Policy Budget
#> 104 0.5833333      NULL      Defense Policy Budget
#> 105 0.5833333      NULL      Defense Policy Budget
#> 106 0.5833333      NULL      Defense Policy Budget
#> 107 0.5625000      NULL      Defense Policy Budget
#> 108 0.6666667      NULL Defense Policy Resolutions
#>                  codes.Clausen             codes.Issue
#> 103 Foreign and Defense Policy                    NULL
#> 104 Foreign and Defense Policy                    NULL
#> 105 Foreign and Defense Policy                    NULL
#> 106 Foreign and Defense Policy                    Iran
#> 107 Foreign and Defense Policy       Shipping/Maritime
#> 108 Foreign and Defense Policy CIA/Spying/Intelligence
```

You can also search for member data using the `member_search` function.

``` r
res <- member_search("Paul",
                     state = "KY")

## Drop the bio field because it is quite long
head(res[, names(res) != "bio"])
#>           id icpsr                   bioname party_code cqlabel congress
#> 1 MS11441104 41104                PAUL, Rand        200    (KY)      114
#> 2 MS11341104 41104                PAUL, Rand        200    (KY)      113
#> 3 MS11241104 41104                PAUL, Rand        200    (KY)      112
#> 4 MS11115406 15406 BUNNING, James Paul David        200    (KY)      111
#> 5 MS11015406 15406 BUNNING, James Paul David        200    (KY)      110
#> 6 MS10915406 15406 BUNNING, James Paul David        200    (KY)      109
#>      state state_abbrev chamber  bioImgURL congresses district_code
#> 1 Kentucky           KY  Senate 041104.jpg   112, 114             0
#> 2 Kentucky           KY  Senate 041104.jpg   112, 114             0
#> 3 Kentucky           KY  Senate 041104.jpg   112, 114             0
#> 4 Kentucky           KY  Senate 015406.jpg   100, 111             0
#> 5 Kentucky           KY  Senate 015406.jpg   100, 111             0
#> 6 Kentucky           KY  Senate 015406.jpg   100, 111             0
#>   nominate.dim2 nominate.dim1 nominate.geo_mean_probability
#> 1        -0.434         0.901                         0.602
#> 2        -0.434         0.901                         0.814
#> 3        -0.434         0.901                         0.747
#> 4        -0.048         0.520                         0.854
#> 5        -0.048         0.520                         0.793
#> 6        -0.048         0.520                         0.861

res <- member_search("Paul",
                     state = "KY",
                     distinct = 1)

## Drop the bio field because it is quite long
head(res[, names(res) != "bio"])
#>           id icpsr                   bioname party_code cqlabel congress
#> 1 MS11441104 41104                PAUL, Rand        200    (KY)      114
#> 2 MS11115406 15406 BUNNING, James Paul David        200    (KY)      111
#>      state state_abbrev chamber  bioImgURL congresses district_code
#> 1 Kentucky           KY  Senate 041104.jpg   112, 114             0
#> 2 Kentucky           KY  Senate 015406.jpg   100, 111             0
#>   nominate.dim2 nominate.dim1 nominate.geo_mean_probability
#> 1        -0.434         0.901                         0.602
#> 2        -0.048         0.520                         0.854
```
