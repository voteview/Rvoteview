library(Rvoteview)
context('Query voteview database with ids, download detailed data')

# Following is written to file to improve speed
# res <- voteview.search()
# save(res, file = "iraq_search.RData")

test_that('download function opens connection', {
  expect_error(voteview_getvote(id = "RH1110298"), NA)
})

test_that('download converts to voteview', {
  rc <- voteview_download(ids = "RH1110298")
  expect_is(rc, 'rollcall')
  expect_equal(nrow(rc$votes), 434)
})

test_that('query works for many ids', {
  res <- voteview_search("Iraq")
  expect_error(rc_big <- voteview_download(ids = res$id[1:100]), NA)
  expect_equal(rc_big$m, 100)
})

