# test make_filename
test_that("make_filename generates the file name", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})
