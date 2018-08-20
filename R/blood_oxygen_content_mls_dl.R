#' Calculate the O2 content of blood
#'
#' Calculates the oxygen content of blood in mls per 100ml.
#'
#' The majority of oxygen in blood is bound to haemoglobin, which is dependent upon
#' haemoglobin oxygen saturation and haemoglobin concentration. The total oxygen capacity of
#' haemoglobin is described by Hüfner’s constant, which is not constant but variable depending
#' on author/literature source or c. This function uses a value of 1.306 ml/g as per \insertCite{gregory_1974}{co2ntent}.
#' \insertCite{gorelov_2008}{co2ntent} delimits this variability.
#'
#' @export
#'
#' @references{
#'   \insertRef{gorelov_2008}{co2ntent}
#'   \insertRef{gregory_1974}{co2ntent}
#'   \insertRef{lumb_nunns_2010}{co2ntent}
#' }
#'
#' @param po2 O2 partial pressure
#' @param so2_fraction Haemoglobin saturation as a fraction e.g 0 < so2_fraction < 1.0
#' @param haemoglobin_g_dl Haemoglobin g/dL. No default
#' @param hufners_constant Oxygen capacity of human haemoglobin. Default 1.306 ml/g
#' @param inputs_are_kpa If TRUE, input pCO2 is in kPa, if FALSE use mmHg
#' @return The O2 content of blood in ml/dL
#'
blood_oxygen_content_mls_dl <- function(po2, so2_fraction, haemoglobin_g_dl, hufners_constant=1.306, inputs_are_kpa=TRUE) {

  # error checking
  po2_param_check(po2, inputs_are_kpa=inputs_are_kpa)
  so2_fraction_param_check(so2_fraction)
  haemoglobin_g_dl_param_check(haemoglobin_g_dl)

  # function body
  if (inputs_are_kpa) {
    dissolved_o2_ml_dl <- 0.0225 * po2
  } else {
    dissolved_o2_ml_dl <- 0.003 * po2
  }

 return((so2_fraction * hufners_constant * haemoglobin_g_dl) + dissolved_o2_ml_dl)
}
