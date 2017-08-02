library(Rvoteview)
context('Query voteview database with ids, download detailed data')
Sys.unsetenv("R_TESTS")
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

test_that('download works with keep long = F', {
  expect_error(rc2 <- voteview_download(ids = "RS0010030", keeplong = F), NA)
})

# To save time, use above rc to check helper functions
test_that('melting rc works fine', {
  expect_error(melt_rollcall(rc), NA)
  expect_error(melt_rollcall(rc2), NA)
})

test_that('grapes operator works as expected', {
  expect_error(rc %+% rc2, 'keeplong')
  rc_nooverlap <- voteview_download(ids = 'RS0010030')
  rc_overlap <- voteview_download(ids = c('RH1110298', 'RH1110298'))
  rc2_overlap <- voteview_download(ids = c("RS0010030", "RS0010031"), keeplong = F)
  rc2_nooverlap <- voteview_download(ids = c("RS0010031"), keeplong = F)
  # with keep long
  expect_error(rc %+% rc_nooverlap, NA)
  expect_error(rc %+% rc_overlap, NA)
  # without keep long, warn about overlap in ICPSR being ambiguous
  expect_warning(rc2 %+% rc2_nooverlap, 'keeplong')
  expect_warning(rc2 %+% rc2_overlap, 'keeplong')
})