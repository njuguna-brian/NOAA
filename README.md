
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NOAA

<!-- badges: start -->
<!-- badges: end -->

The package NOAA provides convenient and efficient tools for accessing,
cleaning, and visualizing earthquake data from the National Oceanic and
Atmospheric Administration (NOAA) database. The package aims to simplify
the process of working with earthquake data, allowing users to quickly
explore and analyze patterns and trends in seismic activity.

## Installation

You can install the development version of NOAA from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("njuguna-brian/NOAA")
```

## Example

An example of its usage is as follows;

``` r
library(NOAA)
#> Loading required package: grid
path <- system.file("extdata/earthquakes.txt", package = "NOAA")
earthquake_data <- readr::read_delim(path, delim = "\t")
#> Rows: 6270 Columns: 39
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr  (2): Search Parameters, Location Name
#> dbl (37): Year, Mo, Dy, Hr, Mn, Sec, Tsu, Vol, Latitude, Longitude, Focal De...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
earthquake_clean <- eq_clean_data(earthquake_data)
head(earthquake_clean)
#> # A tibble: 6 × 10
#>   date                latitude longitude depth magnitude country    location    
#>   <dttm>                 <dbl>     <dbl> <dbl>     <dbl> <chr>      <chr>       
#> 1 1902-02-13 09:39:30     40.7      48.6    15       6.9 AZERBAIJAN Semacha; N …
#> 2 2019-02-12 16:05:07     40.8      72.3     9       6.4 UZBEKISTAN Andizhan    
#> 3 1905-06-02 05:39:42     34       132     100       7.8 JAPAN      Aki         
#> 4 1932-09-26 19:20:42     40.5      23.9    35       6.9 GREECE     Hierissos-S…
#> 5 1937-09-27 08:55:10     -9.4     110.     70       7.2 INDONESIA  Klumpit,Pra…
#> 6 1948-04-22 10:42:48     38.5      20.4    15       6.5 GREECE     Vasiliki (L…
#> # ℹ 3 more variables: house_damaged <dbl>, house_destroyed <dbl>, deaths <dbl>
```
