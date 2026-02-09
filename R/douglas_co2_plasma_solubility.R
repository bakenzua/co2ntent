#' Calculate the solubility coefficient of CO2 in plasma.
#'
#' \code{douglas_co2_plasma_solubility} calculates the solubility coefficient
#' of CO2 in plasma via the formula described by \insertCite{douglas_1988}{co2ntent}.
#'
#' @references{
#'   \insertRef{douglas_1988}{co2ntent}
#' }
#'
#' @keywords internal
#'
#' @param temperature Plasma Temperature in Celsius. Default 37c
#' @return s The solubility coefficient of CO2 in plasma mmol/dL/kPa

douglas_co2_plasma_solubility <- function(temperature = 37, skip_range_check = FALSE) {
  # error checking
  if (min(temperature, na.rm = TRUE) < 0) {
    stop("douglas_co2_plasma_solubility: temperature can not be negative")
  }

  # function method
  t_var <- 37 - temperature
  ret_val <- 0.0307 + (0.00057 * t_var) + (0.00002 * t_var * t_var)
  return(ret_val)
}
