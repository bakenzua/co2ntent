#' Calculate CO2 content of plasma via Douglas method
#'
#' \code{douglas_plasma_co2_content_ml_dl} calculates a hypothetical intermediary
#' CO2 content of plasma via the method described by \insertCite{douglas_1988}{co2ntent}.
#'
#' CO2 content of plasma is calculated from plasma temperature, pH, the calculated solubilty
#' coefficient of CO2 in plasma and the apparent pK of the CO2-HCO3 equilibrium of plasma.
#' The latter two parameters are calculated by \code{co2ntent::co2_plasma_solubility} and
#' \code{co2ntent::apparent_pk_co2_hco3}.
#'
#' The formulae published by \insertCite{douglas_1988}{co2ntent} uses a factor
#' of 2.226 to convert plasma co2 content to ml/dL. This is an interesting choice of
#' molar volume, as it is inaccurate.
#'
#' @references{
#'   \insertRef{douglas_1988}{co2ntent}
#' }
#'
#' @export
#'
#' @param pco2 CO2 partial pressure
#' @param temperature temperature in celcius. Default 37c
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @param inputs_are_kpa If TRUE, input pCO2 is in kPa, if FALSE use mmHg
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return The CO2 content of plasma in ml/dL
#'
#'
douglas_plasma_co2_content_ml_dl <- function(pco2,
                                             temperature=37,
                                             ph=7.4,
                                             inputs_are_kpa=TRUE,
                                             skip_range_check=FALSE
                                             ) {

  # error checking
  pco2_param_check(pco2, inputs_are_kpa=inputs_are_kpa, skip_range_check=skip_range_check)
  temperature_param_check(temperature, skip_range_check=skip_range_check)
  ph_param_check(ph, skip_range_check=skip_range_check)

  # function body
  if (inputs_are_kpa) {
    pco2_mmhg <- kpa_to_mmhg(pco2)
  } else {
    pco2_mmhg <- pco2
  }

  ret_val <- 2.226 *
                co2ntent::co2_plasma_solubility(temperature) *
                pco2_mmhg *
                (1 + 10^(ph - co2ntent::apparent_pk_co2_hco3(temperature, ph)))

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
#' @export
#'
#' @param haemoglobin_g_dl Haemoglobin g/dL. No default
#' @param so2_fraction Haemoglobin saturation as a fraction e.g 0 < so2_fraction < 1.0
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return The CO2 content Plasma:Blood ratio
#'
#'
douglas_co2_plasma_to_blood_ratio <- function(haemoglobin_g_dl,
                                              so2_fraction,
                                              ph=7.4,
                                              skip_range_check=FALSE
                                                 ) {


  # error checking
  ph_param_check(ph, skip_range_check=skip_range_check)
  so2_fraction_param_check(so2_fraction, skip_range_check=skip_range_check)
  haemoglobin_g_dl_param_check(haemoglobin_g_dl, skip_range_check=skip_range_check)

  # function body
  ret_val <- (1 - ((0.0289 * haemoglobin_g_dl)/((3.352 - (0.456 * so2_fraction))*(8.142 - ph))))
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
#' @export
#'
#' @param pco2 CO2 partial pressure
#' @param haemoglobin_g_dl Haemoglobin g/dL. No default
#' @param so2_fraction Haemoglobin saturation as a fraction e.g 0 < so2_fraction < 1.0
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @param temperature temperature in celcius. Default 37c
#' @param inputs_are_kpa If TRUE, input pCO2 is in kPa, if FALSE use mmHg
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return The CO2 content of plasma in ml/dL
#'
#'
douglas_blood_co2_content_ml_dl <- function(pco2,
                                       haemoglobin_g_dl,
                                       so2_fraction,
                                       ph=7.4,
                                       temperature=37,
                                       inputs_are_kpa=TRUE,
                                       skip_range_check=FALSE
                                       ) {

  # error checking
  temperature_param_check(temperature, skip_range_check=skip_range_check)
  ph_param_check(ph, skip_range_check=skip_range_check)
  so2_fraction_param_check(so2_fraction, skip_range_check=skip_range_check)
  haemoglobin_g_dl_param_check(haemoglobin_g_dl, skip_range_check=skip_range_check)

  # function body
  if (inputs_are_kpa) {
    pco2_mmhg <- kpa_to_mmhg(pco2)
  } else {
    pco2_mmhg <- pco2
  }

  ret_val <- douglas_co2_plasma_to_blood_ratio(haemoglobin_g_dl,
                                                              so2_fraction,
                                                              ph=ph
             ) *
             douglas_plasma_co2_content_ml_dl(pco2=pco2_mmhg,
                                              temperature=temperature,
                                              ph=ph,
                                              inputs_are_kpa=FALSE
             )

  return(ret_val)
}
