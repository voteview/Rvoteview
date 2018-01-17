<!-- README.md is generated from README.Rmd. Please edit that file -->
Rvoteview
=========

[![Travis-CI Build Status](https://travis-ci.org/voteview/Rvoteview.svg?branch=master)](https://travis-ci.org/voteview/Rvoteview) [![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/n13u8s0tnfsau1o6?svg=true)](https://ci.appveyor.com/project/lukesonnet/rvoteview)

**WARNING: This package is under construction. Please be patient and leave issues here on Github or contact [Luke Sonnet](mailto:luke.sonnet@gmail.com) with any questions.**

This is a package that enables you to query the Voteview database for roll calls and work with data frames or a `pscl` `rollcall` object.

To install this package, run the following (note you have to have devtools installed first):

``` r
# install.packages('devtools')
devtools::install_github("voteview/Rvoteview")
```

For more thorough documentation, see the help files for individual functions and the vignette [at this link here](https://github.com/voteview/Rvoteview/tree/master/vignettes).

Quick Start: Using Rvoteview
----------------------------

To use `Rvoteview`, you generally want to search the database to get a list of vote ids and then use those to return the individual votes. We query the database with a search term and some parameters to constrain the search. The default behavior is to search any words as key words, returning the roll calls that best match any key words you enter. Again, there are further examples in the [vignette](https://github.com/voteview/Rvoteview/tree/master/vignettes).

So let's start with a search for roll calls with the key word "Iraq".

``` r
library(Rvoteview)
  
res <- voteview_search("Iraq")
#> Warning in strptime(x, fmt, tz = "GMT"): unknown timezone 'default/America/
#> Los_Angeles'
#> Query 'Iraq' returned 345 rollcalls...
names(res)
#>  [1] "id"                "congress"          "chamber"          
#>  [4] "rollnumber"        "date"              "yea"              
#>  [7] "nay"               "support"           "vote_result"      
#> [10] "short_description" "question"          "text"             
#> [13] "key_flags"         "score"             "bill_number"      
#> [16] "sponsor"           "codes.Issue"       "codes.Peltzman"   
#> [19] "codes.Clausen"
  
## I will drop description since it is a very long field
res[1:5, 1:5]
#>          id congress chamber rollnumber       date
#> 1 RH1051102      105   House       1102 1998-10-05
#> 2 RH1010654      101   House        654 1990-08-02
#> 3 RH1020109      102   House        109 1991-05-22
#> 4 RH1020081      102   House         81 1991-05-09
#> 5 RH1050998      105   House        998 1998-08-03
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
#> Number of Legislators:        898
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
#> 100 200 328 
#> 455 442   1 
#> 
#> Vote Summary:
#>                Count Percent
#> 0 (notInLegis)  4638    51.6
#> 1 (yea)         3491    38.9
#> 3 (yea)            1     0.0
#> 6 (nay)          636     7.1
#> 8 (missing)        2     0.0
#> 9 (missing)      212     2.4
#> 
#> Use summary(rc,verbose=TRUE) for more detailed information.
```

You also have lots of metadata on roll calls and legislators in various data frames. For example, we can see some legislator metadata:

``` r
rc$legis.long.dynamic[1:5, 1:5]
#>           id icpsr                           name party_code state_abbrev
#> 1 MP10199908 99908    BUSH, George Herbert Walker        200          USA
#> 2 MH10115090 15090 CALLAHAN, Herbert Leon (Sonny)        200           AL
#> 3 MH10110717 10717       DICKINSON, William Louis        200           AL
#> 4 MH10115632 15632             BROWDER, John Glen        100           AL
#> 5 MH10111000 11000                    BEVILL, Tom        100           AL
```

You can also search by start and end date, congress, and chamber. Please see the help files for each function after you install the package to see a little more about how they work.

``` r
## Voteview search with options
res <- voteview_search(
  "Iraq", 
  chamber = "House",
  congress = 110:112,
  enddate = "2013-04-20"
  )
#> Query '(Iraq) AND (enddate:2013-04-20) AND (congress:110 111 112) AND (chamber:house)' returned 49 rollcalls...
res[1:5, 1:5]
#>          id congress chamber rollnumber       date
#> 1 RH1100711      110   House        711 2007-07-25
#> 2 RH1100418      110   House        418 2007-05-24
#> 3 RH1100419      110   House        419 2007-05-24
#> 4 RH1111048      111   House       1048 2010-02-24
#> 5 RH1100920      110   House        920 2007-10-02
```

We can print out the exact query that the function builds using all of these arguments by retrieving the 'qstring' attribute of the returned data frame:

``` r
attr(res, "qstring")
#> [1] "(Iraq) AND (enddate:2013-04-20) AND (congress:110 111 112) AND (chamber:house)"
```

We can assemble and use these complex queries ourselves. Here's one example where we look for all roll calls with the key words "estate", "death", or "tax" and was held in the 100th to the 114th Congress.

``` r
## Voteview search with options
res <- voteview_search("(alltext:estate death tax congress:[100 to 114])")
#> Query '(alltext:estate death tax congress:[100 to 114])' returned 2035 rollcalls...
res[1:5, 1:5]
#>          id congress chamber rollnumber       date
#> 1 RH1060861      106   House        861 2000-06-09
#> 2 RH1061064      106   House       1064 2000-09-07
#> 3 RH1060860      106   House        860 2000-06-09
#> 4 RH1060856      106   House        856 2000-06-08
#> 5 RH1060855      106   House        855 2000-06-08
```

You can also search for member data using the `member_search` function.

``` r
res <- member_search("Paul", state = "KY")

res[1:5, 1:5]
#>           id icpsr                   bioname party_code cqlabel
#> 1 MS11241104 41104                PAUL, Rand        200    (KY)
#> 2 MS11341104 41104                PAUL, Rand        200    (KY)
#> 3 MS11441104 41104                PAUL, Rand        200    (KY)
#> 4 MS11541104 41104                PAUL, Rand        200    (KY)
#> 5 MH10015406 15406 BUNNING, James Paul David        200 (KY-04)
```
