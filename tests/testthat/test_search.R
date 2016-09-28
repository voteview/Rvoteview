library(Rvoteview)
context('Query voteview database with string, download vote metadata')

test_that('search function hits database', {
  expect_error(voteview_search('test'), NA)
})

test_that('query returned correctly', {
  expect_equal(attr(voteview_search('rhodesia', enddate = '2000'),
                    'qstring'),
               '(rhodesia) AND (enddate:2000)')
  
  expect_equal(attr(voteview_search('rhodesia AND (enddate:2000 OR congress:110)'),
                    'qstring'),
               'rhodesia AND (enddate:2000 OR congress:110)')
})

test_that('search function options query leela correctly', {
  expect_error(voteview_search('Iraq', startdate = 2014), NA)
  expect_error(voteview_search('Iraq', startdate = '2013-10-01'), NA)
  expect_error(voteview_search('Iraq', enddate = '2014-10-01'), NA)
  expect_error(voteview_search('Iraq', chamber = "House"), NA)
  expect_error(voteview_search('Iraq', chamber = "House", congress = 110:113), NA)
})

test_that('search function returns example dataframe with correct number of votes', {
  df <- voteview_search('rhodesia', enddate = 1975)
  df_sub <- voteview_search('rhodesia', enddate = 1980)
  
  expect_is(df, 'data.frame')
  expect_is(df_sub, 'data.frame')
  expect_equal(nrow(df), 8)
  expect_equal(nrow(df_sub), 32)
  expect_is(df_sub$id, "character")
  expect_identical(length(unique(df_sub$id)), nrow(df_sub))
  
})

test_that('unicode searches work', {
  voteview_search('\U00B6')
})