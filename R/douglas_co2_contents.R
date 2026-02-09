#' Calculate CO2 content of plasma via Douglas method
#'
#' \code{douglas_plasma_co2_content_ml_dl} calculates a hypothetical intermediary
#' CO2 content of plasma via the method described by \insertCite{douglas_1988}{co2ntent}.
#'
#' CO2 content of plasma is calculated from plasma temperature, pH, the calculated solubility
#' coefficient of CO2 in plasma and the apparent pK of the CO2-HCO3 equilibrium of plasma.
#' The latter two parameters are calculated by \code{co2ntent::douglas_co2_plasma_solubility} and
#' \code{co2ntent::douglas_apparent_pk_co2_hco3}.
#'
#' @references{
#'   \insertRef{douglas_1988}{co2ntent}
#' }
#'
#'
#' @param pco2 CO2 partial pressure
#' @param temperature temperature in celsius. Default 37c
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @param pco2_units Unit for \code{pco2}; one of \code{"kPa"} or \code{"mmHg"}.
#' @return The CO2 content of plasma in ml/dL
#'
#'
douglas_plasma_co2_content_ml_dl <- function(
  pco2,
  temperature = 37,
  ph = 7.4,
  pco2_units = c("kPa", "mmHg")
) {
  pco2_units <- match.arg(pco2_units)

  # error checking
  if (min(pco2, na.rm = TRUE) < 0) {
    stop("douglas_plasma_co2_content_ml_dl: ph can not be negative")
  }
  if (min(temperature, na.rm = TRUE) < 0) {
    stop("douglas_plasma_co2_content_ml_dl: temperature can not be negative")
  }
  if (min(ph, na.rm = TRUE) < 0) {
    stop("douglas_plasma_co2_content_ml_dl: ph can not be negative")
  }

  # function body
  if (pco2_units == "kPa") {
    pco2_mmhg <- kpa_to_mmhg(pco2)
  } else {
    pco2_mmhg <- pco2
  }

  ret_val <- .molar_volume_defaults()["co2"] *
    co2ntent:::douglas_co2_plasma_solubility(
      temperature
    ) *
    pco2_mmhg *
    (1 +
      10^(ph -
        co2ntent:::douglas_apparent_pk_co2_hco3(
          temperature,
          ph
        )))

  return(ret_val)
}

#' Calculate the plasma to blood CO2 content ratio via Douglas method
#'
#' \code{douglas_co2_plasma_to_blood_ratio} calculates the ratio between the CO2
#' content of plasma and the CO2 content of whole blood via the method described by
#' \insertCite{douglas_1988}{co2ntent}.
#'
#' The plasma/blood ratio is calculated via an equation of the form derived by
#' \insertCite{visser_1960}{co2ntent} and \insertCite{mchardy_1967}{co2ntent},
#' but with the coefficients derived and published by \insertCite{douglas_1988}{co2ntent}
#'
#' @references{
#'   \insertRef{douglas_1988}{co2ntent}
#'   \insertRef{mchardy_1967}{co2ntent}
#'   \insertRef{visser_1960}{co2ntent}
#' }
#'
#' @keywords internal
#'
#' @param haemoglobin_g_dl Haemoglobin g/dL. No default
#' @param so2_fraction Haemoglobin saturation as a fraction e.g 0 < so2_fraction < 1.0
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @return The CO2 content Plasma:Blood ratio
#'
#' @export
douglas_co2_plasma_to_blood_ratio <- function(
  haemoglobin_g_dl,
  so2_fraction,
  ph = 7.4
) {
  # error checking
  if (min(haemoglobin_g_dl, na.rm = TRUE) < 0) {
    stop("douglas_co2_plasma_to_blood_ratio: haemoglobin_g_dl can not be negative")
  }
  if (min(so2_fraction, na.rm = TRUE) < 0 | max(so2_fraction, na.rm = TRUE) > 1) {
    stop("douglas_co2_plasma_to_blood_ratio: haemoglobin_g_dl greater than 1")
  }
  if (min(ph, na.rm = TRUE) < 0) {
    stop("douglas_co2_plasma_to_blood_ratio: ph can not be negative")
  }

  # function body
  ret_val <- (1 -
    ((0.0289 * haemoglobin_g_dl) /
      ((3.352 - (0.456 * so2_fraction)) * (8.142 - ph))))
  return(ret_val)
}

#' Calculate CO2 content of whole blood via Douglas method
#'
#' \code{douglas_blood_co2_content_ml_dl} calculates CO2 content of whole blood via
#' the method described by \insertCite{douglas_1988}{co2ntent}.
#'
#' CO2 content of plasma is calculated via \code{co2ntent::douglas_plasma_co2_content_ml_dl}
#' which is then multiplied by the calculated CO2 content blood:plasma ratio calculated via
#' \code{co2ntent::douglas_co2_plasma_to_blood_ratio}.
#'
#' This is therefore an all in one method, which calculates all the other required parameters.
#'
#' @references{
#'   \insertRef{douglas_1988}{co2ntent}
#' }
#'
#'
#' @param pco2 CO2 partial pressure
#' @param haemoglobin_g_dl Haemoglobin g/dL. No default
#' @param so2_fraction Haemoglobin saturation as a fraction e.g 0 < so2_fraction < 1.0
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @param temperature temperature in celsius. Default 37c
#' @param pco2_units Unit for \code{pco2}; one of \code{"kPa"} or \code{"mmHg"}.
#' @return The CO2 content of plasma in ml/dL
#'
#'
douglas_blood_co2_content_ml_dl <- function(
  pco2,
  haemoglobin_g_dl,
  so2_fraction,
  ph = 7.4,
  temperature = 37,
  pco2_units = c("kPa", "mmHg")
) {
  pco2_units <- match.arg(pco2_units)

  # error checking
  if (min(pco2, na.rm = TRUE) < 0) {
    stop("douglas_blood_co2_content_ml_dl: pco2 can not be negative")
  }
  if (min(haemoglobin_g_dl, na.rm = TRUE) < 0) {
    stop("douglas_blood_co2_content_ml_dl: haemoglobin_g_dl can not be negative")
  }
  if (min(so2_fraction, na.rm = TRUE) < 0) {
    stop("douglas_blood_co2_content_ml_dl: so2_fraction can not be negative")
  }
  if (max(so2_fraction, na.rm = TRUE) > 1) {
    stop("douglas_blood_co2_content_ml_dl: so2_fraction can not be greater than 1")
  }
  if (min(ph, na.rm = TRUE) < 0) {
    stop("douglas_blood_co2_content_ml_dl: ph can not be negative")
  }
  if (min(temperature, na.rm = TRUE) < 0) {
    stop("douglas_blood_co2_content_ml_dl: temperature can not be negative")
  }

  # function body
  if (pco2_units == "kPa") {
    pco2_mmhg <- kpa_to_mmhg(pco2)
  } else {
    pco2_mmhg <- pco2
  }

  ret_val <- douglas_co2_plasma_to_blood_ratio(
    haemoglobin_g_dl,
    so2_fraction,
    ph = ph
  ) *
    douglas_plasma_co2_content_ml_dl(
      pco2 = pco2_mmhg,
      temperature = temperature,
      ph = ph,
      pco2_units = "mmHg"
    )

  return(ret_val)
}
