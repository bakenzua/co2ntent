#' Calculate plasma bicarbonate concentration as per Henderson-Hasselbach equation.
#'
#' \code{siggaard_andersen_plasma_bicarbonate_content_mmol_l} plasma bicarbonate concentration as per the
#' Henderson-Hasselbach equationv described by \insertCite{siggaard_1988}{co2ntent}.
#'
#' Calculation is a straightforward Henderson-Hasselbach rearrangement. This relies
#' on the solubility coeffiecient of CO2 in plasma. \insertCite{siggaard_1988}{co2ntent}
#' provide a constant of 0.230 mmol/L/kPa.
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
#' @param inputs_are_kpa If TRUE, input pCO2 is in kPa, if FALSE use mmHg
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return The HCO3 concentration of plasma in mmol/dL
#'
#'
siggaard_andersen_plasma_bicarbonate_content_mmol_l <- function(pco2,
                                       ph=7.4,
                                       temperature=37,
                                       inputs_are_kpa=TRUE,
                                       skip_range_check=FALSE
) {

  # error checking
  pco2_param_check(pco2, inputs_are_kpa=inputs_are_kpa, skip_range_check=skip_range_check)
  temperature_param_check(temperature, skip_range_check=skip_range_check)
  ph_param_check(ph, skip_range_check=skip_range_check)

  # function body
  if (inputs_are_kpa) {
    pco2_kpa <- pco2
  } else {
    pco2_kpa <- mmhg_to_kpa(pco2)
  }

  s <- 0.231  #  mmol / L / kPa
  # s <- mmhg_to_kpa(0.023) # 0.003066414 mmol/dl/mmhg

  pk_p <- 6.125 - log10(1 + 10^(ph - 8.7))

  ret_val <- s * pco2_kpa * (10^(ph - pk_p))

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
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return The erythrocyte pH
#'
#'
siggaard_andersen_erythrocyte_ph <- function(so2_fraction,
                                             ph=7.4,
                                             skip_range_check=FALSE
) {

  # error checking
  ph_param_check(ph, skip_range_check=skip_range_check)
  so2_fraction_param_check(so2_fraction, skip_range_check=skip_range_check)

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
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return The erythrocyte pK
#'
#'
siggaard_andersen_erythrocyte_p_k <- function(so2_fraction,
                                              ph=7.4,
                                              skip_range_check=FALSE
) {

  # error checking
  ph_param_check(ph, skip_range_check=skip_range_check)
  so2_fraction_param_check(so2_fraction, skip_range_check=skip_range_check)

  # function body
  ret_val <- 6.125 - log10(1 + 10^(siggaard_andersen_erythrocyte_ph(so2_fraction=so2_fraction, ph = ph, skip_range_check=skip_range_check) - 7.84 - (0.06 * so2_fraction)))

  return(ret_val)
}

