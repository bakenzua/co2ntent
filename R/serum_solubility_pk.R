#' Calculate the solubility coefficient of CO2 in plasma.
#'
#' \code{co2_plasma_solubility} calculates the solubility coefficient
#' of CO2 in plasma via the formula described by \insertCite{douglas_1988}{co2ntent}.
#'
#' @references{
#'   \insertRef{douglas_1988}{co2ntent}
#' }
#'
#' @export
#'
#' @param temperature Plasma Temperature in Celcius. Default 37c
#' @return s The solubility coefficient of CO2 in plasma mmol/dL/kPa

co2_plasma_solubility <- function(temperature=37) {


  # error checking
  temperature_param_check(temperature)

  # function method
  t_var <- 37 - temperature
  ret_val <- 0.0307 + (0.00057 * t_var) + (0.00002 * t_var * t_var)
  return(ret_val)
}

#' Calculate the apparent pK' of the CO2-Bicarbonate system in blood.
#'
#' \code{apparent_pk_co2_hco3} calculates the apparent pK'of the
#' CO2-Bicarbonate in blood via the formula described by \insertCite{douglas_1988}{co2ntent}.
#'
#' @references{
#'   \insertRef{douglas_1988}{co2ntent}
#' }
#'
#' @export
#'
#' @param temperature Plasma Temperature in Celcius. Default 37c
#' @param ph Plasma pH. Default 7.40
#' @return The apparent pK'

apparent_pk_co2_hco3 <- function(temperature=37, ph=7.4) {


  # error checking
  temperature_param_check(temperature)
  ph_param_check(ph)

  # function body
  ph_var <- 7.4 - ph
  ret_val <- 6.086 + (0.042 * ph_var) + ((38 - temperature) * (0.00472 +  (0.00139 * ph_var)))
  return(ret_val)
}
