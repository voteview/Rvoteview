library(Rvoteview)
library(rjson)
context('Query voteview database with ids, download detailed data')

# Following is written to file to improve speed
# res <- voteview.search()
# save(res, file = "iraq_search.RData")

test_that('download function opens connection', {
  expect_that(voteview.download.json(ids = "H1110298"), not(throws_error("cannot open the connection")))
})

test_that('download converts to voteview', {
  jsonquery <- voteview.download.json(ids = "H1110298")
  expect_is(jsonquery, 'character')
  expect_that(dat <- fromJSON(jsonquery), not(throws_error()))
  expect_that(vv_small <- read.voteview.json(jsonquery), not(throws_error()))
  expect_is(vv_small, "voteview")
  expect_equal(nrow(vv_small$votematrix), 434)
})

test_that('query works for many ids', {
  res <- voteview.search("Iraq")
  expect_that(vv_big <- voteview.download(ids = res$id), not(throws_error()))
  expect_equal(nrow(vv_big$rollcalls), length(res$id))
})