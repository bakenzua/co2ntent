#' Convert mmol/L ↔ mL/dL for CO2, O2 or ideal gas
#'
#' Convert between mmol per litre and mL per decilitre for gases.
#'
#' The conversion uses the molar volume in dL per mmol. With $V_m$ in dL/mmol:
#' $$ mL/dL = mmol/L \times (10 \times V_m). $$
#'
#' @param x Numeric vector: values to convert (mmol/L for mmols_l_to_mls_dl, mL/dL for mls_dl_to_mmols_l).
#' @param gas Character; one of "co2", "o2", "ideal". Chooses the molar volume default.
#' @param molar_volume Optional numeric scalar to override the default molar volume (units: dL/mmol).
#' @return Numeric vector of same length and (when appropriate) names as `x`.
#' @examples
#' mmols_l_to_mls_dl(c(1, 2.5), gas = "co2")
#' mls_dl_to_mmols_l(c(2.2263, 5.56575), gas = "co2")
#' 
#' @export
#' 
mmols_l_to_mls_dl <- function(x, gas = c("co2", "o2", "ideal"), molar_volume = NULL) {
  gas <- match.arg(gas)
  # defaults <- .molar_volume_defaults() # dL / mmol
  if (!is.null(molar_volume)) {
    if (!is.numeric(molar_volume) || length(molar_volume) != 1 || is.na(molar_volume)) {
      stop("`molar_volume` must be a single numeric value (dL/mmol) when provided.")
    }
    vm <- as.numeric(molar_volume)
  } else {
    vm <- unname(.molar_volume_defaults()[gas])
  }

  if (!is.numeric(x)) stop("`x` must be numeric.")
  if (length(x) == 0) return(numeric(0))
  out <- x * vm
  nm <- names(x)
  if (!is.null(nm) && length(nm) == length(out)) names(out) <- nm
  out
}

#' @rdname mmols_l_to_mls_dl
#' 
#' @export
#' 
mls_dl_to_mmols_l <- function(x, gas = c("co2", "o2", "ideal"), molar_volume = NULL) {
  gas <- match.arg(gas)
  # defaults <- .molar_volume_defaults() # dL / mmol
  if (!is.null(molar_volume)) {
    if (!is.numeric(molar_volume) || length(molar_volume) != 1 || is.na(molar_volume)) {
      stop("`molar_volume` must be a single numeric value (dL/mmol) when provided.")
    }
    vm <- as.numeric(molar_volume)
  } else {
    vm <- unname(.molar_volume_defaults()[gas])
  }
  if (!is.numeric(x)) stop("`x` must be numeric.")
  if (length(x) == 0) return(numeric(0))
  out <- x / vm
  nm <- names(x)
  if (!is.null(nm) && length(nm) == length(out)) names(out) <- nm
  out
}


#' Default molar volumes for gases (internal)
#'
#' Named numeric vector of default molar volumes in decilitres per millimole (dL/mmol).
#' Values are chosen for common physiological gases:
#' - co2: carbon dioxide
#' - o2: oxygen
#' - ideal: ideal gas approximation
#'
#' These defaults are used by mmols_l_to_mls_dl() and mls_dl_to_mmols_l()
#' when `molar_volume` is not supplied.
#'
#' @return Named numeric vector (dL/mmol).
#' @keywords internal
#' @seealso mmols_l_to_mls_dl, mls_dl_to_mmols_l
#' @examples
#' .molar_volume_defaults()["o2"]
.molar_volume_defaults <- function() {
  c(co2 = 2.2263, o2 = 2.2393, ideal = 2.2414) # dL / mmol
} 
