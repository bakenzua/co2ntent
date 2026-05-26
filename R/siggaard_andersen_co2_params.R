#' Calculate plasma bicarbonate concentration as per Henderson-Hasselbalch equation.
#'
#' \code{actual_bicarbonate_content_mmol_l} calculates plasma bicarbonate concentration as per the
#' Henderson-Hasselbalch equation.
#'
#' Calculation is a straightforward Henderson-Hasselbalch rearrangement. This relies
#' on the solubility coefficient of CO2 in plasma. \insertCite{siggaard_1988}{co2ntent}
#' provide a constant of 0.230 mmol/L/kPa.
#'
#' @references{
#'   \insertRef{douglas_1988}{co2ntent}
#'   \insertRef{siggaard_1988}{co2ntent}
#' }
#'
#' @keywords internal
#'
#' @param pco2 CO2 partial pressure
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @param pressure_units Unit for \code{pco2}; one of \code{"kPa"} or \code{"mmHg"}.
#' @return The HCO3 concentration of plasma in mmol/dL
#'
#' @export
actual_bicarbonate_content_mmol_l <- function(
  pco2,
  ph = 7.4,
  pco2_units = c("kPa", "mmHg")
) {
  pco2_units <- match.arg(pco2_units)

  # error checking
  if (min(pco2, na.rm = TRUE) < 0) {
    stop(
      "actual_bicarbonate_content_mmol_l: pco2 can not be negative"
    )
  }
  if (min(ph, na.rm = TRUE) < 0) {
    stop(
      "actual_bicarbonate_content_mmol_l: ph can not be negative"
    )
  }

  # function body
  if (pco2_units == "kPa") {
    pco2_kpa <- pco2
  } else {
    pco2_kpa <- mmhg_to_kpa(pco2)
  }

  s <- 0.230 #  mmol / L / kPa
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
#' @keywords internal
#'
#' @param so2_fraction Haemoglobin saturation as a fraction e.g 0 < so2_fraction < 1.0
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @return The erythrocyte pH
#'
#'
siggaard_andersen_erythrocyte_ph <- function(so2_fraction, ph = 7.4) {
  # error checking
  if (min(ph, na.rm = TRUE) < 0) {
    stop("siggaard_andersen_erythrocyte_ph: ph can not be negative")
  }
  if (min(so2_fraction, na.rm = TRUE) < 0) {
    stop("siggaard_andersen_erythrocyte_ph: so2_fraction can not be negative")
  }
  if (max(so2_fraction, na.rm = TRUE) > 1) {
    stop("siggaard_andersen_erythrocyte_ph: so2_fraction can not be greater than 1")
  }

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
#' @keywords internal
#'
#' @param so2_fraction Haemoglobin saturation as a fraction e.g 0 < so2_fraction < 1.0
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @return The erythrocyte pK
#'
#'
siggaard_andersen_erythrocyte_p_k <- function(
  so2_fraction,
  ph = 7.4
) {
  # error checking
  if (min(ph, na.rm = TRUE) < 0) {
    stop("siggaard_andersen_erythrocyte_p_k: ph can not be negative")
  }
  if (min(so2_fraction, na.rm = TRUE) < 0) {
    stop("siggaard_andersen_erythrocyte_p_k: so2_fraction can not be negative")
  }
  if (max(so2_fraction, na.rm = TRUE) > 1) {
    stop("siggaard_andersen_erythrocyte_p_k: so2_fraction can not be greater than 1")
  }

  # function body
  ret_val <- 6.125 -
    log10(
      1 +
        10^(siggaard_andersen_erythrocyte_ph(
          so2_fraction = so2_fraction,
          ph = ph
        ) -
          7.84 -
          (0.06 * so2_fraction))
    )

  return(ret_val)
}
