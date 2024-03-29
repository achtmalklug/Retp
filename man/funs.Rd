% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{psychro}
\alias{psychro}
\alias{satpressslope}
\title{psychro
calculates the psychrometric constant (water in air)}
\usage{
psychro(p)

satpressslope(temp)
}
\value{
slope of the curve in kPa/°C
}
\description{
psychro
calculates the psychrometric constant (water in air)

satpressslope
calculates the slope of the water vapour saturation pressure for a given temperature
}
\examples{
psychro(3)
psychro(20)
}
