## This file contains helper functions for Rvoteview
## Functions:
##            baseurl
##            trim
##            Mode

# Trim white space off of strings
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Internal function to find mode of vector
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Internal function that returns the URL of the current server
baseurl <- function() {
  return("http://128.97.229.160")
}