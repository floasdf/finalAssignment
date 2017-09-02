

test_that("makefilename",{
  year <- 2017
  tmpName <- make_filename(year)
  expect_that(tmpName , is_a("character"))
})