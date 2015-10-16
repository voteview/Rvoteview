<!-- README.md is generated from README.Rmd. Please edit that file -->
Rvoteview
=========

**NOTE: This package is in beta. Furthermore, much of the database has been rewritten so some things are still not working perfectly. In particular, there are issues with roll calls from the 113rd and 114th sessions of Congress. Therefore we recommend you omit any roll call ids from those sessions of Congress for now. See usage below.**

This is a package that enables you to query the Voteview database for roll calls and work with data frames or a `pscl` `rollcall` object.

To install this package, ensure you have `devtools` installed. If you do not run `install.packages("devtools")` before doing the following: `R   devtools::install_github("JeffreyBLewis/Rvoteview")`

Using Rvoteview
---------------

To use `Rvoteview`, you generally want to query the database to get a list of vote ids and then use those to return the individual votes. We query the database with a search term like so:

\`\`\`r library(Rvoteview)

res \<- voteview.search("Iraq") \#\> Query 'Iraq' returned 316 votes... names(res) \#\> [1] "descriptionShort" "description" "no"
 \#\> [4] "yea" "chamber" "session"
 \#\> [7] "rollnumber" "date" "id"

\#\# I will drop description since it is a very long field head(res[, -2]) \#\> descriptionShort no yea chamber \#\> 1 INVOKE CLOTURE ON OMNIBUS TRADE BILL KUWAITI TANKERS 41 53 Senate \#\> 2 SUPPORT A CEASEFIRE & SETTLEMENT IN IRAN-IRAQ WAR 0 96 Senate \#\> 3 CONDEMN IRAQ CHEM WEAPONS AGAINST IRAN 0 91 Senate \#\> 4 CHINA STOP SELLING MISSILES IRAN & IRAQ 0 97 Senate \#\> 5 IRAQ SANCTIONS B/C USE OF CHEM WEAPONS (PASS) 16 389 House \#\> 6 FOREIGN AID--US ASST CHINA MISSILE PROG ON CERT. C 237 23 House \#\> session rollnumber date id \#\> 1 100 189 1987-07-14 S1000189 \#\> 2 100 231 1987-08-07 S1000231 \#\> 3 100 621 1988-06-24 S1000621 \#\> 4 100 678 1988-07-27 S1000678 \#\> 5 100 828 1988-09-27 H1000828 \#\> 6 100 850 1988-09-30 H1000850 \`\``Using`res$id`we can get the JSON that contains the full set of votes and data for each roll call and then turn that JSON in to a`pscl`` `rollcall `` object.

\`\`\`r \#\# Get the JSON using the ids from the previous search \#\# Note I drop most recent sessions where errors are occurring json \<- voteview.download(res[res$session \< 113, ]$id)

\#\# Turn the JSON in to a rollcall object rc \<- voteview2rollcall(json)

\#\# Now this object can be used in many 'pscl' methods summary(rc) \#\> Source: Download from VoteView \#\> \#\> Number of Legislators: 1327 \#\> Number of Roll Call Votes: 313 \#\> \#\> \#\> Using the following codes to represent roll call votes: \#\> Yea: 1 2 3 \#\> Nay: 4 5 6 \#\> Abstentions: NA 7 8 9 \#\> Not In Legislature: 0 \#\> \#\> Party Composition: \#\> 100 200 328 <NA> \#\> 699 625 3 0 \#\> \#\> Vote Summary: \#\> Count Percent \#\> 0 (notInLegis) 332123 80.0 \#\> 1 (yea) 50986 12.3 \#\> 3 (yea) 7 0.0 \#\> 4 (nay) 5 0.0 \#\> 6 (nay) 28920 7.0 \#\> 7 (missing) 117 0.0 \#\> 8 (missing) 3 0.0 \#\> 9 (missing) 3190 0.8 \#\> \#\> Use summary(rc,verbose=TRUE) for more detailed information. \`\`\`

Please see the help files for each function after you install the package to see a little more about how they work.

<!--  THIS IS NOT WORKING RIGHT NOW BECAUSE OF SOME ERROR in voteview.download. It seems that having too many queries or something is causing it to bug out. If I breakdown res$id in to chunks it works....
## Simple Example

Search for roll calls with the keyword "University."
 
 ```r
 res <- voteview.search("University")
 print(res$id)
 json <- voteview.download(res[res$session < 113, ]$id)
 rc <- voteview2rollcall(json)
 summary(rc)
 ```
 
Search for roll calls with the keyword "University" since 2009. **For now, all searches that specify a start date will automatically drop roll calls from sessions 113 and 114.**

 
 ```r
 res <- voteview.search("University", startdate = "2009-01-01")
 json <- voteview.download(res$id)
 rc <- voteview2rollcall(json)
 summary(rc)
 ```-->
\`\`\`
