#' Calculate SO2 from pO2 of blood.
#'
#'\code{std_kelman_po2_to_so2} calculates haemoglobin oxygen saturation from partial pressure of oxygen in blood
#' viathe method described by \insertCite{kelman_1966}{co2ntent}. This form makes no correction for acid/base
#' disturbance, assuming pH=7.4, temperature=37c and pCO2=40mmHg
#'
#' @note \code{kelman_po2_to_so2} doesn't work for po2 < 0.5kpa as returned SO2 can become negative
#'
#' @references{
#'   \insertRef{kelman_1966}{co2ntent}
#' }
#'
#' @export
#'
#' @param po2 O2 partial pressure
#' @param inputs_are_kpa Input parameters are kPa, otherwise use mmHg
#' @return Haemoglobin saturation as fraction

std_kelman_po2_to_so2 <- function(po2, inputs_are_kpa=TRUE) {


  # error checking
  po2_param_check(po2, inputs_are_kpa=inputs_are_kpa)

  # function body
  a_1 <- -8.5322289e3
  a_2 <- 2.1214010e3
  a_3 <- -6.7073989e1
  a_4 <- 9.3596087e5
  a_5 <-	-3.1346258e4
  a_6 <-	2.3961674e3
  a_7 <-	-6.7104406e1

  if (inputs_are_kpa) {
    po2_mmhg <- kpa_to_mmhg(po2)
  } else {
    po2_mmhg <- po2
  }

  ret_val <- ((a_1 * po2_mmhg) + (a_2 * po2_mmhg^2) + (a_3 * po2_mmhg^3) + po2_mmhg^4) / (a_4 + (a_5 * po2_mmhg) + (a_6 * po2_mmhg^2) + (a_7 * po2_mmhg^3) + po2_mmhg^4)

  return(ret_val)
}

#' Calculate SO2 from pO2 of blood.
#'
#'\code{kelman_po2_to_so2} calculates haemoglobin oxygen saturation from partial pressure of oxygen in blood
#' via the method described by \insertCite{kelman_1966}{co2ntent}.
#'
#' @note \code{kelman_po2_to_so2} doesn't work for po2 < 0.5kpa as returned SO2 can become negative
#'
#' @references{
#'   \insertRef{kelman_1966}{co2ntent}
#' }
#'
#' @export
#'
#' @param po2 O2 partial pressure
#' @param temperature temperature in celcius. Default 37c
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @param pco2 CO2 partial pressure. Default 5.332895kPa (40mmHg)
#' @param inputs_are_kpa Input parameters are kPa, otherwise use mmHg
#' @return Haemoglobin saturation as fraction

kelman_po2_to_so2 <- function(po2, temperature=37, ph=7.40, pco2=5.332895, inputs_are_kpa=TRUE) {

  # error checking
  po2_param_check(po2, inputs_are_kpa=inputs_are_kpa)
  pco2_param_check(pco2, inputs_are_kpa=inputs_are_kpa)
  temperature_param_check(temperature)
  ph_param_check(ph)

  # function body

  if (inputs_are_kpa) {
    po2_mmhg <- kpa_to_mmhg(po2)
    pco2_mmhg <- kpa_to_mmhg(pco2)
  } else {
    po2_mmhg <- po2
    pco2_mmhg <- pco2
  }

  po2_mmHg_virtual <- po2_mmhg * 10^(0.024*(37-temperature) + 0.4*(ph - 7.40) + 0.06*(log10(40) - log10(pco2_mmhg)))

  return(std_kelman_po2_to_so2(po2_mmHg_virtual, inputs_are_kpa = FALSE))
}
