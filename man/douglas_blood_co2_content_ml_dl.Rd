% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/douglas_co2_contents.R
\name{douglas_blood_co2_content_ml_dl}
\alias{douglas_blood_co2_content_ml_dl}
\title{Calculate CO2 content of whole blood via Douglas method}
\usage{
douglas_blood_co2_content_ml_dl(pco2, haemoglobin_g_dl, so2_fraction,
  ph = 7.4, temperature = 37, inputs_are_kpa = TRUE)
}
\arguments{
\item{pco2}{CO2 partial pressure}

\item{haemoglobin_g_dl}{Haemoglobin g/dL. No default}

\item{so2_fraction}{Haemoglobin saturation as a fraction e.g 0 < so2_fraction < 1.0}

\item{ph}{pH (hydrogen ion concentration). Default 7.40}

\item{temperature}{temperature in celcius. Default 37c}

\item{inputs_are_kpa}{If TRUE, input pCO2 is in kPa, if FALSE use mmHg}
}
\value{
The CO2 content of plasma in ml/dL
}
\description{
\code{douglas_blood_co2_content_ml_dl} calculates CO2 content of whole blood via
the method described by \insertCite{douglas_1988}{co2ntent}.
}
\details{
CO2 content of plasma is calculated via \code{co2ntent::douglas_plasma_co2_content_ml_dl}
which is then multiplied by the calculated CO2 content blood:plasma ratio calculated via
\code{co2ntent::douglas_co2_plasma_to_blood_ratio}.

This is therefore an all in one method, which calculates all the other required parameters.
}
\references{
{
  \insertRef{douglas_1988}{co2ntent}
}
}
