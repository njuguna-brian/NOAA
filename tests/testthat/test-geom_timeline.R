
library(testthat)
library(ggplot2)


path <- system.file("extdata/earthquakes.txt", package = "NOAA")
earthquake_data <-  readr::read_delim(path, delim = "\t")
earthquake_clean <- eq_clean_data(earthquake_data)


test_that('output is a ggplot',{
  g = ggplot(earthquake_clean,aes(x = date,
                             size = magnitude,
                             color = deaths,
                             fill = deaths))
  expect_s3_class(g, 'ggplot')
})
