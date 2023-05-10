
library(testthat)
library(readr)

path <- system.file("extdata/earthquakes.txt", package = "NOAA")
earthquake_data <-  readr::read_delim(path, delim = "\t")
earthquake_clean <- eq_clean_data(earthquake_data)


test_that("output is a dataframe", {

  expect_s3_class(earthquake_clean, "data.frame")
})

test_that("column is a date" ,{
  columnDate <- earthquake_clean$date

  expect_s3_class(columnDate, 'POSIXt')
})


test_that("latitude and longitude columns are converted to class numeric or type double",{
  columnLat <- earthquake_clean$latitude
  columnLong <- earthquake_clean$longitude

  expect_type(columnLat, 'double')
  expect_type(columnLong, 'double')
})


