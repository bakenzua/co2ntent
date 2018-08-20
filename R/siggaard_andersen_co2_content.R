#' Calculate plasma CO2 content as per Siggaard-Andersen method.
#'
#' \code{siggaard_andersen_plasma_co2_content_mmol_dl} calculates plasma CO2 content via the method described
#' by \insertCite{siggaard_1988}{co2ntent}.
#'
#' @references{
#'   \insertRef{siggaard_1988}{co2ntent}
#' }
#'
#' @export
#'
#' @param hco3_mmols_dl plasma bicarbonate concentration mmols/dL
#' @param pco2 CO2 partial pressure
#' @param inputs_are_kpa If TRUE, input pCO2 is in kPa, if FALSE use mmHg
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return The CO2 content of plasma in mmol/dL
#'
#'
siggaard_andersen_plasma_co2_content_mmol_dl <- function(hco3_mmols_dl,
                                                  pco2,
                                                  inputs_are_kpa=TRUE,
                                                  skip_range_check=FALSE

) {

  # error checking
  bicarbonate_mmol_dl_param_check(hco3_mmols_dl, skip_range_check=skip_range_check)
  pco2_param_check(pco2, inputs_are_kpa=inputs_are_kpa, skip_range_check=skip_range_check)

  # function body

  if (inputs_are_kpa) {
    pco2_mmhg <- kpa_to_mmhg(pco2)
  } else {
    pco2_mmhg <- pco2
  }

  ret_val <- hco3_mmols_dl + (0.0307 * pco2_mmhg)
  return(ret_val)
}

#' Calculate whole blood CO2 content as per Siggaard-Andersen method.
#'
#' \code{siggaard_andersen_blood_co2_content_mmol_dl} calculates whole blood
#' CO2 content via the method described by \insertCite{siggaard_1988}{co2ntent}.
#'
#' @references{
#'   \insertRef{siggaard_1988}{co2ntent}
#' }
#'
#' @export
#'
#' @param hco3_mmols_dl plasma bicarbonate concentration mmols/dL
#' @param pco2 CO2 partial pressure
#' @param haemoglobin_g_dl Haemoglobin g/dL. No default
#' @param so2_fraction Haemoglobin saturation as a fraction e.g 0 < so2_fraction < 1.0
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @param inputs_are_kpa If TRUE, input pCO2 is in kPa, if FALSE use mmHg
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return The CO2 content of blood in mmol/dL
#'
#'
siggaard_andersen_blood_co2_content_mmol_dl <- function(hco3_mmols_dl,
                                                  pco2,
                                                  haemoglobin_g_dl,
                                                  so2_fraction,
                                                  ph=7.4,
                                                  inputs_are_kpa=TRUE,
                                                  skip_range_check=FALSE
) {

  # error checking
  bicarbonate_mmol_dl_param_check(hco3_mmols_dl, skip_range_check=skip_range_check)
  pco2_param_check(pco2, inputs_are_kpa=inputs_are_kpa, skip_range_check=skip_range_check)
  ph_param_check(ph, skip_range_check=skip_range_check)
  so2_fraction_param_check(so2_fraction, skip_range_check=skip_range_check)
  haemoglobin_g_dl_param_check(haemoglobin_g_dl, skip_range_check=skip_range_check)

  # function body

  if (inputs_are_kpa) {
    pco2_mmhg <- kpa_to_mmhg(pco2)
  } else {
    pco2_mmhg <- pco2
  }

  ph_minus_pk <- siggaard_andersen_erythrocyte_ph(so2_fraction, ph) - siggaard_andersen_erythrocyte_p_k(so2_fraction, ph)

  ret_val <- (0.000768 * pco2_mmhg * haemoglobin_g_dl * (1 + 10^ph_minus_pk)) +
                  (siggaard_andersen_plasma_co2_content_mmol_dl(hco3_mmols_dl, pco2_mmhg, FALSE) * (1 - (haemoglobin_g_dl / 33.8)))
  return(ret_val)
}
