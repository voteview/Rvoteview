## This file contains helper functions for Rvoteview
## Functions:
##            trim
##            Mode

# Trim white space off of strings
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Internal function to find mode of vector
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}