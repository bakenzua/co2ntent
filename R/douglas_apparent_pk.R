#' Calculate the apparent pK' of the CO2-Bicarbonate system in blood.
#'
#' \code{douglas_apparent_pk_co2_hco3} calculates the apparent pK'of the
#' CO2-Bicarbonate in blood via the formula described by 
#' \insertCite{douglas_1988}{co2ntent}.
#'
#' @references{
#'   \insertRef{douglas_1988}{co2ntent}
#' }
#'
#' @keywords internal
#'
#' @param temperature Plasma Temperature in Celsius. Default 37c
#' @param ph Plasma pH. Default 7.40
#' @return The apparent pK'

douglas_apparent_pk_co2_hco3 <- function(temperature = 37, ph = 7.4) {
  # error checking
  if (min(temperature, na.rm = TRUE) < 0) {
    stop("douglas_apparent_pk_co2_hco3: temperature can not be negative")
  }
  if (min(ph, na.rm = TRUE) < 0) {
    stop("douglas_apparent_pk_co2_hco3: ph can not be negative")
  }

  # function body
  ph_var <- 7.4 - ph
  ret_val <- 6.086 + (0.042 * ph_var) + ((38 - temperature) * (0.00472 + (0.00139 * ph_var)))
  return(ret_val)
}
