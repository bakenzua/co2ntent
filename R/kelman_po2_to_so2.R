#' Calculate SO2 from pO2 of blood.
#'
#' \code{kelman_std_po2_to_so2} calculates haemoglobin oxygen saturation
#' from partial pressure of oxygen in blood via the method described by
#' \insertCite{kelman_1966}{co2ntent}. This form makes no correction
#' for acid/base disturbance, assuming pH=7.4, temperature=37c and
#' pCO2=40mmHg
#'
#' @note \code{kelman_std_po2_to_so2} doesn't work for po2 < 0.5kpa as
#' returned SO2 can become negative
#'
#' @references{
#'   \insertRef{kelman_1966}{co2ntent}
#' }
#'
#' @keywords internal
#'
#' @param po2 O2 partial pressure
#' @param inputs_are_kpa Input parameters are kPa, otherwise use mmHg
#' @return Haemoglobin saturation as fraction

kelman_std_po2_to_so2 <- function(po2, po2_units = c("kPa", "mmHg")) {
 
  po2_units <- match.arg(po2_units)
  # error checking
  if (min(po2, na.rm = TRUE) < 0) {
    stop("kelman_std_po2_to_so2: po2 can not be negative")
  }

  # function body
  a_1 <- -8.5322289e3
  a_2 <- 2.1214010e3
  a_3 <- -6.7073989e1
  a_4 <- 9.3596087e5
  a_5 <- -3.1346258e4
  a_6 <- 2.3961674e3
  a_7 <- -6.7104406e1

  if (po2_units == "kPa") {
    po2_mmhg <- co2ntent::kpa_to_mmhg(po2)
  } else {
    po2_mmhg <- po2
  }

  ret_val <- ((a_1 * po2_mmhg) + (a_2 * po2_mmhg^2) + (a_3 * po2_mmhg^3) + po2_mmhg^4) / (a_4 + (a_5 * po2_mmhg) + (a_6 * po2_mmhg^2) + (a_7 * po2_mmhg^3) + po2_mmhg^4)

  return(ret_val)
}

#' Calculate SO2 from pO2 of blood.
#'
#' \code{kelman_po2_to_so2} calculates haemoglobin oxygen saturation from
#' partial pressure of oxygen in blood via the method described by
#' \insertCite{kelman_1966}{co2ntent}.
#'
#' @note \code{kelman_po2_to_so2} doesn't work for po2 < 0.5kpa as returned
#' SO2 can become negative
#'
#' @references{
#'   \insertRef{kelman_1966}{co2ntent}
#' }
#'
#'
#' @param po2 O2 partial pressure
#' @param temperature temperature in celsius. Default 37c
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @param pco2 CO2 partial pressure. Default 5.332895kPa (40mmHg)
#' @param pressure_units Input parameters are kPa, otherwise use mmHg
#' @return Haemoglobin saturation as fraction
kelman_po2_to_so2 <- function(po2, temperature = 37, ph = 7.40, pco2 = 5.332895, pressure_units = c("kPa", "mmHg")) {

  pressure_units <- match.arg(pressure_units)

  # error checking
  if (min(po2, na.rm = TRUE) < 0) {
    stop("kelman_po2_to_so2: po2 can not be negative")
  }
  if (min(temperature, na.rm = TRUE) < 0) {
    stop("kelman_po2_to_so2: temperature can not be negative")
  }
  if (min(ph, na.rm = TRUE) < 0) {
    stop("kelman_po2_to_so2: ph can not be negative")
  }
  if (min(pco2, na.rm = TRUE) < 0) {
    stop("kelman_po2_to_so2: pco2 can not be negative")
  }

  # function body

  if (pressure_units == "kPa") {
    po2_mmhg <- kpa_to_mmhg(po2)
    pco2_mmhg <- kpa_to_mmhg(pco2)
  } else {
    po2_mmhg <- po2
    pco2_mmhg <- pco2
  }

  # po2_mmHg_virtual <- po2_mmhg * 10^(0.024*(37-temperature) + 0.4*(ph - 7.40) + 0.06*(log10(40) - log10(pco2_mmhg)))
  po2_mmHg_virtual <- kelman_virtual_po2(po2 = po2_mmhg, pco2 = pco2_mmhg, temperature = temperature, ph = ph, pressure_units = "mmHg")

  ret_val <- kelman_std_po2_to_so2(po2_mmHg_virtual, po2_units = "mmHg")

  return(ret_val)
}

#' Calculate virtual pO2.
#'
#' \code{kelman_virtual_po2} calculates a 'virtual po2', a pO2 corrected for
#' ph pco2 and temperature, as per the method described by
#' \insertCite{kelman_1966}{co2ntent}.
#' 
#' Units for pO2 and pCO2 should be in the same units (mmHg or kPa) as specified by pressure_units.
#' The result is returned in same units
#'
#' @references{
#'   \insertRef{kelman_1966}{co2ntent}
#' }
#'
#' @keywords internal
#'
#' @param po2 O2 partial pressure
#' @param pco2 CO2 partial pressure. Default 5.332895kPa (40mmHg)
#' @param temperature temperature in celsius. Default 37c
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @param pressure_units Input parameters are kPa, otherwise use mmHg
#' @return Vector of virtual pO2
kelman_virtual_po2 <- function(po2, pco2, temperature = 37, ph = 7.4, pressure_units = c("kPa", "mmHg")) {

  pressure_units = match.arg(pressure_units)
  # error checking
  if (min(po2, na.rm = TRUE) < 0) {
    stop("kelman_virtual_po2: po2 can not be negative")
  }
  if (min(temperature, na.rm = TRUE) < 0) {
    stop("kelman_virtual_po2: temperature can not be negative")
  }
  if (min(ph, na.rm = TRUE) < 0) {
    stop("kelman_virtual_po2: ph can not be negative")
  }
  if (min(pco2, na.rm = TRUE) < 0) {
    stop("kelman_virtual_po2: pco2 can not be negative")
  }

  # function body

  if (pressure_units == "kPa") {
    po2_mmhg <- kpa_to_mmhg(po2)
    pco2_mmhg <- kpa_to_mmhg(pco2)
  } else {
    po2_mmhg <- po2
    pco2_mmhg <- pco2
  }

  # prevent log10(-Inf) being calculated and thus NA values returning in po2_mmHg_virtual
  pco2_mmhg[pco2_mmhg == 0] <- 0.000000000001

  po2_mmHg_virtual <- po2_mmhg * 10^(0.024 * (37 - temperature) + 0.4 * (ph - 7.40) + 0.06 * (log10(40) - log10(pco2_mmhg)))

  if (pressure_units == "kPa") {
    ret_po2s <- mmhg_to_kpa(po2_mmHg_virtual)
  } else {
    ret_po2s <- po2_mmHg_virtual
  }
  return(ret_po2s)
}
