% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/serum_solubility_pk.R
\name{co2_plasma_solubility}
\alias{co2_plasma_solubility}
\title{Calculate the solubility coefficient of CO2 in plasma.}
\usage{
co2_plasma_solubility(temperature = 37)
}
\arguments{
\item{temperature}{Plasma Temperature in Celcius. Default 37c}
}
\value{
s The solubility coefficient of CO2 in plasma mmol/dL/kPa
}
\description{
\code{co2_plasma_solubility} calculates the solubility coefficient
of CO2 in plasma via the formula described by \insertCite{douglas_1988}{co2ntent}.
}
\references{
{
  \insertRef{douglas_1988}{co2ntent}
}
}
