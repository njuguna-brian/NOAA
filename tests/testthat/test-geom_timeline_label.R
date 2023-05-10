
library(testthat)
library(ggplot2)


path <- system.file("extdata/earthquakes.txt", package = "NOAA")
earthquake_data <-  readr::read_delim(path, delim = "\t")
earthquake_clean <- eq_clean_data(earthquake_data)


g <- ggplot(earthquake_clean)+
  geom_timeline_label(aes(x = c(date,location)))

test_that("The output is ggplot", {
  expect_s3_class(g, "ggplot")
})
