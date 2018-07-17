## Author: Ben Sabath
## Date: 7/17/2018
## Purpose: Tests for arepa package 
##

test_that("check unzipping works", {
  get_AQS_data_daily(year = 2012)
  expect_true(length(list.files("Data_AQS/daily_88101")) > 0)
  system("rm -r Data_AQS")
})

test_that("check annual file names", {
  get_AQS_data_annual(year = 2000:2002)
  x <- load_annual_average(year = 2000:2002)
  expect_is(x, "data.frame")
})

system("rm -r Data_AQS")