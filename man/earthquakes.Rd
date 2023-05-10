\name{earthquakes}
\alias{earthquakes}
\docType{data}
\title{Earthquakes dataset}
\description{
  The earthquakes dataset contains information about global earthquakes from 2150 BC to 2016 AD.
  It includes details such as the date and time of the earthquake, its location, magnitude,
  and various measures of the impact, such as the number of deaths, injuries, and damage to property.
}
\usage{data("earthquakes")}
\format{
  A data frame with 6270 observations on the following 39 variables.
  \describe{
    \item{\samp{Search Parameters}}{a character vector}
    \item{\code{Year}}{a numeric vector}
    \item{\code{Mo}}{a numeric vector}
    \item{\code{Dy}}{a numeric vector}
    \item{\code{Hr}}{a numeric vector}
    \item{\code{Mn}}{a numeric vector}
    \item{\code{Sec}}{a numeric vector}
    \item{\code{Tsu}}{a numeric vector}
    \item{\code{Vol}}{a numeric vector}
    \item{\samp{Location Name}}{a character vector}
    \item{\code{Latitude}}{a numeric vector}
    \item{\code{Longitude}}{a numeric vector}
    \item{\samp{Focal Depth (km)}}{a numeric vector}
    \item{\code{Mag}}{a numeric vector}
    \item{\samp{MMI Int}}{a numeric vector}
    \item{\code{Deaths}}{a numeric vector}
    \item{\samp{Death Description}}{a numeric vector}
    \item{\code{Missing}}{a numeric vector}
    \item{\samp{Missing Description}}{a numeric vector}
    \item{\code{Injuries}}{a numeric vector}
    \item{\samp{Injuries Description}}{a numeric vector}
    \item{\samp{Damage ($Mil)}}{a numeric vector}
    \item{\samp{Damage Description}}{a numeric vector}
    \item{\samp{Houses Destroyed}}{a numeric vector}
    \item{\samp{Houses Destroyed Description}}{a numeric vector}
    \item{\samp{Houses Damaged}}{a numeric vector}
    \item{\samp{Houses Damaged Description}}{a numeric vector}
    \item{\samp{Total Deaths}}{a numeric vector}
    \item{\samp{Total Death Description}}{a numeric vector}
    \item{\samp{Total Missing}}{a numeric vector}
    \item{\samp{Total Missing Description}}{a numeric vector}
    \item{\samp{Total Injuries}}{a numeric vector}
    \item{\samp{Total Injuries Description}}{a numeric vector}
    \item{\samp{Total Damage ($Mil)}}{a numeric vector}
    \item{\samp{Total Damage Description}}{a numeric vector}
    \item{\samp{Total Houses Destroyed}}{a numeric vector}
    \item{\samp{Total Houses Destroyed Description}}{a numeric vector}
    \item{\samp{Total Houses Damaged}}{a numeric vector}
    \item{\samp{Total Houses Damaged Description}}{a numeric vector}
  }
}
\source{
National Centers for Environmental Information (NCEI) of the National Oceanic and Atmospheric Administration (NOAA)}
\examples{
data(earthquakes)
str(earthquakes)
}
\keyword{datasets}
