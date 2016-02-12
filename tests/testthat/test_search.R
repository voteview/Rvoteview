library(Rvoteview)
context('Query voteview database with string, download vote metadata')

test_that('search function hits database', {
  expect_that(voteview_search('test'), not(throws_error()))
})

test_that('search function options query leela correctly', {
  expect_that(voteview_search('Iraq', startdate = 2014), not(throws_error()))
  expect_that(voteview_search('Iraq', startdate = '2013-10-01'), not(throws_error()))
  expect_that(voteview_search('Iraq', enddate = '2014-10-01'), not(throws_error()))
  expect_that(voteview_search('Iraq', chamber = "House"), not(throws_error()))
  expect_that(voteview_search('Iraq', chamber = "House", session = 110:113), not(throws_error()))
})

test_that('search function returns example dataframe with correct number of votes', {
  df <- voteview_search('rhodesia')
  df_sub <- voteview_search('rhodesia', startdate = 1970)
  
  expect_is(df, 'data.frame')
  expect_is(df_sub, 'data.frame')
  expect_equal(nrow(df), 142)
  expect_equal(nrow(df_sub), 136)
  expect_is(df_sub$id, "character")
  expect_identical(length(unique(df_sub$id)), nrow(df_sub))
})