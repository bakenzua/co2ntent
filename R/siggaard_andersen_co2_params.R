#' Calculate plasma bicarbonate concentration as per Henderson-Hasselbach equation.
#'
#' \code{plasma_bicarbonate_content} plasma bicarbonate concentration as per the
#' Henderson-Hasselbach equationv described by \insertCite{siggaard_1988}{co2ntent}.
#'
#' Calculation is a straightforward Henderson-Hasselbach rearrangement. This relies
#' on the solubility coeffiecient of CO2 in plasma. \insertCite{siggaard_1988}{co2ntent}
#' provide a constant of 0.230 mmol/L/kPa, or we can calculate with the method provided
#' by \insertCite{douglas_1988}{co2ntent} implemented as \code{\link{co2_plasma_solubility}}
#'
#' @references{
#'   \insertRef{douglas_1988}{co2ntent}
#'   \insertRef{siggaard_1988}{co2ntent}
#' }
#'
#' @export
#'
#' @param pco2 CO2 partial pressure
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @param temperature temperature in celcius. Default 37c
#' @param calculate_solubility_coefficient Default FALSE
#' @param inputs_are_kpa If TRUE, input pCO2 is in kPa, if FALSE use mmHg
#' @return The HCO3 concentration of plasma in mmol/dL
#'
#'
plasma_bicarbonate_content <- function(pco2,
                                       ph=7.4,
                                       temperature=37,
                                       calculate_solubility_coefficient=FALSE,
                                       inputs_are_kpa=TRUE
) {

  # error checking
  pco2_param_check(pco2, inputs_are_kpa=inputs_are_kpa)
  temperature_param_check(temperature)
  ph_param_check(ph)

  # function body
  if (inputs_are_kpa) {
    pco2_mmhg <- kpa_to_mmhg(pco2)
  } else {
    pco2_mmhg <- pco2
  }


  if (calculate_solubility_coefficient) {
    s <- co2_plasma_solubility(temperature = temperature)
  } else {
    s <- 0.023
  }

  ret_val <- s * pco2_mmhg * (10^(ph - 6.1))

  return(ret_val)
}


#' Calculate Erythrocyte pH as per Siggaard-Andersen method.
#'
#' \code{siggard_a_erythrocyte_ph} calculates erythrocyte pH via the method described
#' by \insertCite{siggaard_1988}{co2ntent}.
#'
#' @references{
#'   \insertRef{siggaard_1988}{co2ntent}
#' }
#'
#' @export
#'
#' @param so2_fraction Haemoglobin saturation as a fraction e.g 0 < so2_fraction < 1.0
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @return The CO2 content of plasma in ml/dL
#'
#'
siggaard_andersen_erythrocyte_ph <- function(so2_fraction,
                                             ph=7.4
) {

  # error checking
  ph_param_check(ph)
  so2_fraction_param_check(so2_fraction)

  # function body
  ret_val <- 7.19 + (0.77 * (ph - 7.4)) + (0.035 * (1 - so2_fraction))

  return(ret_val)
}

#' Calculate Erythrocyte pK as per Siggaard-Andersen method.
#'
#' \code{siggaard_andersen_erythrocyte_p_k} calculates erythrocyte pK via the method described
#' by \insertCite{siggaard_1988}{co2ntent}.
#'
#' @references{
#'   \insertRef{siggaard_1988}{co2ntent}
#' }
#'
#' @export
#'
#' @param so2_fraction Haemoglobin saturation as a fraction e.g 0 < so2_fraction < 1.0
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @return The CO2 content of plasma in ml/dL
#'
#'
siggaard_andersen_erythrocyte_p_k <- function(so2_fraction,
                                             ph=7.4
) {

  # error checking
  ph_param_check(ph)
  so2_fraction_param_check(so2_fraction)

  # function body
  ret_val <- 6.125 - log10(1 + 10^(siggaard_andersen_erythrocyte_ph(so2_fraction=so2_fraction, ph = ph) - 7.84 - (0.06 * so2_fraction)))

  return(ret_val)
}

