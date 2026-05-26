#' High-level interface for O2 content of blood
#'
#' Provides a unified entry point to calculate the oxygen content of blood in
#' mL of O2 per dL of blood.
#'
#' @param po2 O2 partial pressure.
#' @param so2_fraction Haemoglobin saturation as a fraction
#'   (e.g. \eqn{0 < so2\_fraction < 1.0}).
#' @param haemoglobin_g_dl Haemoglobin concentration in g/dL.
#' @param hufners_constant Oxygen capacity of human haemoglobin in mL/g.
#'   Default is \code{1.306} as per Gregory (1974).
#' @param pressure_units Unit for \code{po2}; one of \code{"kPa"} or \code{"mmHg"}.
#' @param content_units Unit for result; one of \code{"mmol/L"} or \code{"ml/dL"}.
#' @return Numeric vector of O2 content in mL/dL.
#'
#' @examples
#' o2_content(
#'   po2            = 10,
#'   so2_fraction   = 0.95,
#'   haemoglobin_g_dl = 15,
#'   pressure_units = "kPa"
#' )
#'
#' @export
o2_content <- function(
  po2,
  so2_fraction,
  haemoglobin_g_dl,
  hufners_constant = 1.306,
  pressure_units = c("kPa", "mmHg"),
  content_units = c("ml/dL", "mmol/L")
) {
  pressure_units <- match.arg(pressure_units)
  content_units <- match.arg(content_units)

  if (content_units == "ml/dL") {
    blood_oxygen_content_mls_dl(
      po2 = po2,
      so2_fraction = so2_fraction,
      haemoglobin_g_dl = haemoglobin_g_dl,
      hufners_constant = hufners_constant,
      po2_units = pressure_units
    )
  } else if (content_units == "mmol/L") {
    blood_oxygen_content_mls_dl(
      po2 = po2,
      so2_fraction = so2_fraction,
      haemoglobin_g_dl = haemoglobin_g_dl,
      hufners_constant = hufners_constant,
      po2_units = pressure_units
    ) |>
      co2ntent::mls_dl_to_mmols_l(gas = "o2")
  } else {
    stop(
      "Only units = 'ml/dL' or 'mmol/L', are currently supported in o2_content().",
      call. = FALSE
    )
  }
}
