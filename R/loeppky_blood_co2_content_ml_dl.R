#' Calculate CO2 content of blood via Loeppky method
#'
#' \code{loeppky_blood_co2_content_ml_dl} calculates the
#' CO2 content of blood via the method described by \insertCite{loeppky_1983}{co2ntent}.
#'
#' @references{
#'   \insertRef{loeppky_1983}{co2ntent}
#' }
#'
#'
#' @param pco2 CO2 partial pressure
#' @param pco2_units Unit for \code{pco2}; one of \code{"kPa"} or \code{"mmHg"}.
#' @return The CO2 content of plasma in ml/dL
#'
loeppky_blood_co2_content_ml_dl <- function(
  pco2,
  pco2_units = c("kPa", "mmHg")
) {
  pco2_units <- match.arg(pco2_units)

  # error checking
  if (min(pco2, na.rm = TRUE) < 0) {
    stop("loeppky_blood_co2_content_ml_dl: pco2 can not be negative")
  }

  # function body
  if (pco2_units == "kPa") {
    pco2_mmhg <- kpa_to_mmhg(pco2)
  } else {
    pco2_mmhg <- pco2
  }

  12.8105*pco2_mmhg^0.3692
}
