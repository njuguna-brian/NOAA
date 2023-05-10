
library(testthat)

path <- system.file("extdata/earthquakes.txt", package = "NOAA")
earthquake_data <-  readr::read_delim(path, delim = "\t")
earthquake_clean <- eq_clean_data(earthquake_data)

test_that('the output is a string (character) vector', {
  label = eq_create_label(earthquake_clean)

  expect_type(label, "character")
})

