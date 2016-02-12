library(Rvoteview)
library(rjson)
context('Query voteview database with ids, download detailed data')

# Following is written to file to improve speed
# res <- voteview.search()
# save(res, file = "iraq_search.RData")

test_that('download function opens connection', {
  expect_that(voteview_getvote(id = "H1110298"), not(throws_error("cannot open the connection")))
})

test_that('download converts to voteview', {
  vv <- voteview_download(ids = "H1110298")
  expect_is(vv, 'voteview')
  expect_equal(nrow(vv$votematrix), 434)
})

test_that('query works for many ids', {
  res <- voteview_search("Iraq")
  expect_that(vv_big <- voteview_download(ids = res$id[1:100]), not(throws_error()))
  expect_that(rc_big <- voteview2rollcall(vv_big), not(throws_error()))
  expect_equal(rc_big$m, 100)
})

