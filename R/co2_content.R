#' High-level interface for CO2 content calculations
#'
#' Provides a unified entry point to calculate CO2 content in plasma or whole
#' blood using supported physiological models.
#'
#' @param phase Character string; one of \code{"plasma"} or \code{"blood"}.
#'   Selects whether to calculate plasma or whole-blood CO2 content.
#' @param method Character string; one of \code{"douglas"} or
#'   \code{"siggaard_andersen"}. Selects the underlying physiological model.
#' @param content_units Character string; currently \code{"ml/dL"} for Douglas-based
#'   calculations and \code{"mmol/L"} for Siggaard-Andersen-based calculations.
#' @param ... Additional arguments passed on to the underlying implementation
#'   function. See the documentation for the corresponding lower-level
#'   functions for full details.
#'
#' @return Numeric vector of CO2 content in the requested units.
#'
#' @examples
#' # Whole blood CO2 content using Douglas method (ml/dL)
#' co2_content(
#'   phase  = "blood",
#'   method = "douglas",
#'   content_units  = "ml/dL",
#'   pco2   = 5,
#'   haemoglobin_g_dl = 10,
#'   so2_fraction     = 0.9
#' )
#'
#' # Whole blood CO2 content using Siggaard-Andersen method (mmol/L)
#' co2_content(
#'   phase  = "blood",
#'   method = "siggaard_andersen",
#'   content_units  = "mmol/L",
#'   hco3_mmols_l    = 17,
#'   pco2            = 5,
#'   haemoglobin_g_dl = 10,
#'   so2_fraction     = 0.9,
#'   pco2_units = "kPa"
#' )
#'
#' @export
co2_content <- function(
  phase  = c("blood", "plasma"),
  method = c("douglas", "siggaard_andersen"),
  content_units  = c("ml/dL", "mmol/L"),
  pco2_units = c("kPa", "mmHg"),
  ...
) {
  phase  <- match.arg(phase)
  method <- match.arg(method)
  content_units  <- match.arg(content_units)
  pco2_units <- match.arg(pco2_units)

  # Whole blood content ------------------------------------------------------
  if (phase == "blood" && method == "douglas" && content_units == "ml/dL") {
    return(douglas_blood_co2_content_ml_dl(...))
  }
  if (phase == "blood" && method == "douglas" && content_units == "mmol/L") {
    return(douglas_blood_co2_content_ml_dl(...) |> mls_dl_to_mmols_l())
  }

  if (phase == "blood" && method == "siggaard_andersen" && content_units == "mmol/L") {
    return(siggaard_andersen_blood_co2_content_mmol_l(...))
  }
  if (phase == "blood" && method == "siggaard_andersen" && content_units == "ml/dL") {
    return(siggaard_andersen_blood_co2_content_mmol_l(...) |> mmols_l_to_mls_dl())
  }

  # Plasma content -----------------------------------------------------------
  if (phase == "plasma" && method == "douglas" && content_units == "ml/dL") {
    return(douglas_plasma_co2_content_ml_dl(...))
  }  
  if (phase == "plasma" && method == "douglas" && content_units == "mmol/L") {
    return(douglas_plasma_co2_content_ml_dl(...) |> mls_dl_to_mmols_l())
  }

  if (phase == "plasma" && method == "siggaard_andersen" && content_units == "mmol/L") {
    return(siggaard_andersen_plasma_co2_content_mmol_l(...))
  }
  if (phase == "plasma" && method == "siggaard_andersen" && content_units == "ml/dL") {
    return(siggaard_andersen_plasma_co2_content_mmol_l(...) |> mmols_l_to_mls_dl())
  }

  stop(
    "Unsupported combination of phase, method and content_units in co2_content(). ",
    call. = FALSE
  )
}