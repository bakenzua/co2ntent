#' Calculate the solubility coefficient of CO2 in plasma. Austin et al(1963)
#'
#' @param t Plasma Temperature.
#' @return The solubility coefficient of CO2 in plasma



co2_plasma_solubility <- function(t) {

  t_var <- 37 - t
  ret_val <- 0.0307 + (0.00057 * t_var) + (0.00002 * t_var * t_var)
  return(ret_val)
}

#' Calculate the apparent pK' of the CO2-bicarbonate system in blood.
#'
#' @param t Plasma Temperature.
#' @param ph Plasma pH.
#' @return The apparent pK'

apparent_pk <- function(t, ph) {

  ph_var <- 7.4 - ph
  ret_val <- 6.086 + (0.042 * ph_var) + ((38 - t) * (0.00472 +  (0.00139 * ph_var)))
  return(ret_val)
}
