#' High-level interface for pO2 to SO2
#'
#' Calculates haemoglobin oxygen saturation from partial pressure of oxygen in
#' blood via a chosen method (currently Kelman only).
#'
#' @param po2 O2 partial pressure.
#' @param temperature Temperature in Celsius. Default 37.
#' @param ph pH (hydrogen ion concentration). Default 7.40.
#' @param pco2 CO2 partial pressure. Default 5.332895 kPa (40 mmHg).
#' @param pressure_units Units for \code{po2} and \code{pco2}; one of \code{"kPa"} or \code{"mmHg"}.
#' @param method Character string; currently only \code{"kelman"} is supported.
#'
#' @return Numeric vector of haemoglobin saturation as a fraction.
#'
#' @examples
#' po2_to_so2(
#'   po2  = 10,
#'   temperature = 37,
#'   ph   = 7.4,
#'   pco2 = 5.3329,
#'   pressure_units = "kPa"
#' )
#'
#' @export
po2_to_so2 <- function(
  po2,
  temperature = 37,
  ph = 7.40,
  pco2 = 5.332895,
  pressure_units = c("kPa", "mmHg"),
  method = c("kelman")
) {
  pressure_units <- match.arg(pressure_units)
  method <- match.arg(method)

  if (method == "kelman") {
    return(
      kelman_po2_to_so2(
        po2 = po2,
        temperature = temperature,
        ph = ph,
        pco2 = pco2,
        pressure_units = pressure_units
      )
    )
  }

  stop(
    "Unsupported method in po2_to_so2(). Currently only method = 'kelman' is supported.",
    call. = FALSE
  )
}
