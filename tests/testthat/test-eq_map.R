

library(testthat)


path <- system.file("extdata/earthquakes.txt", package = "NOAA")
earthquake_data <-  readr::read_delim(path, delim = "\t")
earthquake_clean <- eq_clean_data(earthquake_data)


map = earthquake_clean |>
  filter(country == "MEXICO" & lubridate::year(date) >= 2000) |>
  eq_map(annot_col = "date")

test_that("the output is a leaflet",{

  expect_s3_class(map, "leaflet")
})
