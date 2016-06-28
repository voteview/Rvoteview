## Author: Luke Sonnet
## Date: June 27, 2016
## Purpose: This file compiles the README and Vignette RMD files to README.MD 
## files so they are rendered on github properly.

## Run from voteview source directory or run in terminal after navigating here
## Requires rmarkdown and your system has to know where pandoc is
library(rmarkdown) ## will fail and throw error if it does not exist

## README.Rmd
rmarkdown::render("README.Rmd",
                  output_format = "md_document",
                  output_options = list(variant = "markdown_github"),
                  output_file = "README.md")

## vignettes/Rvoteview.Rmd
## To markdown
rmarkdown::render("vignettes/Rvoteview.Rmd",
                  output_format = "md_document",
                  output_options = list(variant = "markdown_github"),
                  output_file = "README.md")
