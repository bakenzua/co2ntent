#' Calculate the solubility coefficient of CO2 in plasma.
#'
#' \code{douglas_co2_plasma_solubility} calculates the solubility coefficient
#' of CO2 in plasma via the formula described by \insertCite{douglas_1988}{co2ntent}.
#'
#' @references{
#'   \insertRef{douglas_1988}{co2ntent}
#' }
#'
#' @export
#'
#' @param temperature Plasma Temperature in Celcius. Default 37c
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return s The solubility coefficient of CO2 in plasma mmol/dL/kPa

douglas_co2_plasma_solubility <- function(temperature=37, skip_range_check=FALSE) {


  # error checking
  temperature_param_check(temperature, skip_range_check=skip_range_check)

  # function method
  t_var <- 37 - temperature
  ret_val <- 0.0307 + (0.00057 * t_var) + (0.00002 * t_var * t_var)
  return(ret_val)
}
