library(Rvoteview)
context('Query voteview database with ids, download detailed data')

# Following is written to file to improve speed
# res <- voteview.search()
# save(res, file = "iraq_search.RData")

rc <- voteview_download(ids = "RH1110298")
res <- voteview_search("Iraq")
rc2 <- voteview_download(ids = "RS0010030", keeplong = F)
rc_nooverlap <- voteview_download(ids = 'RS0010030')
rc_overlap <- voteview_download(ids = c('RH1110298', 'RH1110298'))
rc2_overlap <- voteview_download(ids = c("RS0010030", "RS0010031"), keeplong = F)
rc2_nooverlap <- voteview_download(ids = c("RS0010031"), keeplong = F)

test_that('download function opens connection', {
  expect_error(voteview_getvote(id = "RH1110298"), NA)
})

test_that('download converts to voteview', {
  expect_is(rc, 'rollcall')
  expect_equal(nrow(rc$votes), 434)
})

test_that('query works for many ids', {
  expect_error(rc_big <- voteview_download(ids = res$id[1:100]), NA)
  expect_equal(rc_big$m, 100)
})

test_that('download works with keep long = F', {
  expect_error(voteview_download(ids = "RS0010030", keeplong = F), NA)
})

# To save time, use above rc to check helper functions
test_that('melting rc works fine', {
  expect_error(melt_rollcall(rc), NA)
  expect_error(melt_rollcall(rc2), NA)
})

test_that('grapes operator works as expected', {
  expect_error(rc %+% rc2, 'keeplong')
  # with keep long
  expect_error(rc %+% rc_nooverlap, NA)
  expect_error(rc %+% rc_overlap, NA)
  # without keep long, warn about overlap in ICPSR being ambiguous
  expect_warning(rc2 %+% rc2_nooverlap, 'keeplong')
  expect_warning(rc2 %+% rc2_overlap, 'keeplong')
})

test_that('download metadata works', {
  d <- download_metadata("rollcalls", congress = 1, usetemp = FALSE)
  expect_true(!all(is.na(d$yea_count)))
  d <- download_metadata("members", congress = 110, chamber = "both", usetemp = FALSE)
  expect_true(
    length(setdiff(c("house", "senate"), tolower(d$chamber))) == 0
  )
  
  expect_error(
    download_metadata("rollcallf", usetemp = FALSE),
    "rollcalls"
  )
  
  expect_error(
    d <- download_metadata("parties", congress = 1, chamber = "senate", usetemp = FALSE),
    NA
  )
  expect_error(
    d <- download_metadata("parties", congress = "all", chamber = "senate", usetemp = FALSE),
    NA
  )
  expect_error(
    d <- download_metadata("parties", usetemp = FALSE),
    NA
  )
  
  expect_error(
    d <- download_metadata("members", congress = 1, chamber = "senate", usetemp = FALSE),
    NA
  )
  expect_error(
    d <- download_metadata("members", congress = "all", chamber = "senate", usetemp = FALSE),
    NA
  )
  
  expect_error(
    d <- download_metadata("rollcalls", congress = 30, chamber = "house", usetemp = FALSE),
    NA
  )
  
})