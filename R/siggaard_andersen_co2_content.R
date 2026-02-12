#' Calculate plasma CO2 content as per Siggaard-Andersen method.
#'
#' \code{siggaard_andersen_plasma_co2_content_mmol_l} calculates plasma CO2 content via the method described
#' by \insertCite{siggaard_1988}{co2ntent}.
#'
#' @references{
#'   \insertRef{siggaard_1988}{co2ntent}
#' }
#'
#'
#' @param hco3_mmols_l plasma bicarbonate concentration mmols/dL
#' @param pco2 CO2 partial pressure
#' @param pco2_units Unit for \code{pco2}; one of \code{"kPa"} or \code{"mmHg"}
#' @return The CO2 content of plasma in mmol/L
#'
#'
siggaard_andersen_plasma_co2_content_mmol_l <- function(
  hco3_mmols_l,
  pco2,
  pco2_units = c("kPa", "mmHg")
) {
  pco2_units <- match.arg(pco2_units)

  # error checking
  if (min(hco3_mmols_l, na.rm = TRUE) < 0) {
    stop(
      "siggaard_andersen_plasma_co2_content_mmol_l: hco3_mmols_l can not be negative"
    )
  }
  if (min(pco2, na.rm = TRUE) < 0) {
    stop(
      "siggaard_andersen_plasma_co2_content_mmol_l: pco2 can not be negative"
    )
  }

  # function body
  if (pco2_units == "kPa") {
    pco2_mmhg <- kpa_to_mmhg(pco2)
  } else {
    pco2_mmhg <- pco2
  }

  # solubility_coeff_co2_plasma <- 0.231 # mmol / L / kPa
  solubility_coeff_co2_plasma <- 0.0308 # mmol / L / mmHg

  return(hco3_mmols_l + (solubility_coeff_co2_plasma * pco2_mmhg))
}

#' Calculate whole blood CO2 content as per Siggaard-Andersen method.
#'
#' \code{siggaard_andersen_blood_co2_content_mmol_l} calculates whole blood
#' CO2 content via the method described by \insertCite{siggaard_1988}{co2ntent}.
#'
#' @references{
#'   \insertRef{siggaard_1988}{co2ntent}
#' }
#'
#'
#' @param hco3_mmols_l plasma bicarbonate concentration mmols/L
#' @param pco2 CO2 partial pressure
#' @param haemoglobin_g_dl Haemoglobin g/dL. No default
#' @param so2_fraction Haemoglobin saturation as a fraction e.g 0 < so2_fraction < 1.0
#' @param ph pH (hydrogen ion concentration). Default 7.40
#' @param pco2_units Unit for \code{pco2}; one of \code{"kPa"} or \code{"mmHg"}.
#' @return The CO2 content of blood in mmol/L
#'
#'
siggaard_andersen_blood_co2_content_mmol_l <- function(
  hco3_mmols_l,
  pco2,
  haemoglobin_g_dl,
  so2_fraction,
  ph = 7.4,
  pco2_units = c("kPa", "mmHg")
) {
  pco2_units <- match.arg(pco2_units)

  # error checking
  if (min(hco3_mmols_l, na.rm = TRUE) < 0) {
    stop(
      "siggaard_andersen_blood_co2_content_mmol_l: hco3_mmols_l can not be negative"
    )
  }
  if (min(pco2, na.rm = TRUE) < 0) {
    stop("siggaard_andersen_blood_co2_content_mmol_l: pco2 can not be negative")
  }
  if (min(haemoglobin_g_dl, na.rm = TRUE) < 0) {
    stop(
      "siggaard_andersen_blood_co2_content_mmol_l: haemoglobin_g_dl can not be negative"
    )
  }
  if (min(ph, na.rm = TRUE) < 0) {
    stop("siggaard_andersen_blood_co2_content_mmol_l: ph can not be negative")
  }
  if (min(so2_fraction, na.rm = TRUE) < 0) {
    stop("douglas_blood_co2_content_ml_dl: so2_fraction can not be negative")
  }
  if (max(so2_fraction, na.rm = TRUE) > 1) {
    stop(
      "douglas_blood_co2_content_ml_dl: so2_fraction can not be greater than 1"
    )
  }

  # function body
  if (pco2_units == "kPa") {
    pco2_mmhg <- kpa_to_mmhg(pco2)
  } else {
    pco2_mmhg <- pco2
  }

  vol_fraction_erythrocyte <- haemoglobin_g_dl / 33.83822

  ph_minus_pk <- siggaard_andersen_erythrocyte_ph(
    so2_fraction = so2_fraction,
    ph = ph
  ) -
    siggaard_andersen_erythrocyte_p_k(
      so2_fraction = so2_fraction,
      ph = ph
    )
  # solubility_coeff_co2_erythrocyte <- 0.195 # mmol / dL / kPa
  solubility_coeff_co2_erythrocyte <- 0.026 # mmol / dL / mmHg

  erythrocyte_partition_content <- solubility_coeff_co2_erythrocyte *
    pco2_mmhg *
    (1 + 10^ph_minus_pk)

  plasma_partition_content <- siggaard_andersen_plasma_co2_content_mmol_l(
    hco3_mmols_l = hco3_mmols_l,
    pco2 = pco2_mmhg,
    pco2_units = "mmHg"
  )

  ret_val <- (vol_fraction_erythrocyte * erythrocyte_partition_content) +
    ((1 - vol_fraction_erythrocyte) * plasma_partition_content)
  # ret_val <- (0.000768 * pco2_mmhg * haemoglobin_g_dl * (1 + 10^ph_minus_pk)) +
  #                 (siggaard_andersen_plasma_co2_content_mmol_dl(hco3_mmols_dl=hco3_mmols_dl, pco2=pco2_mmhg, inputs_are_kpa=FALSE, skip_range_check=skip_range_check) * (1 - (haemoglobin_g_dl / 3.383822)))
  return(ret_val)
}
