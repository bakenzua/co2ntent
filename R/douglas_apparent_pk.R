#' Calculate the apparent pK' of the CO2-Bicarbonate system in blood.
#'
#' \code{douglas_apparent_pk_co2_hco3} calculates the apparent pK'of the
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
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return The apparent pK'

douglas_apparent_pk_co2_hco3 <- function(temperature=37, ph=7.4, skip_range_check=FALSE) {


  # error checking
  temperature_param_check(temperature, skip_range_check=skip_range_check)
  ph_param_check(ph, skip_range_check=skip_range_check)

  # function body
  ph_var <- 7.4 - ph
  ret_val <- 6.086 + (0.042 * ph_var) + ((38 - temperature) * (0.00472 +  (0.00139 * ph_var)))
  return(ret_val)
}
