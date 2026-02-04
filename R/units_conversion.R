#' Convert mmHg to kPa
#'
#' Convert pressure from millimetres of mercury (mmHg) to kilopascals (kPa).
#'
#' @param p Numeric vector of pressures in mmHg. NA values are preserved.
#' @return Numeric vector of pressures in kPa (same length and names as `p`).
#' @examples
#' mmhg_to_kpa(c(760, 380, NA))
#' @seealso kpa_to_mmhg
#' @note 1 atm = 760 mmHg = 101.325 kPa.
#' @export
mmhg_to_kpa <- function(p) {
  kpa_per_mmhg <- 101.325 / 760
  if (!is.numeric(p)) {
    stop("`p` must be numeric.")
  }
  if (length(p) == 0) {
    numeric(0)
  } else {
    out <- p * kpa_per_mmhg
    if (!is.null(names(p))) names(out) <- names(p)
    out
  }
}

#' Convert kPa to mmHg
#'
#' Convert pressure from kilopascals (kPa) to millimetres of mercury (mmHg).
#'
#' @param p Numeric vector of pressures in kPa. NA values are preserved.
#' @return Numeric vector of pressures in mmHg (same length and names as `p`).
#' @examples
#' kpa_to_mmhg(c(101.325, 50.6625, NA))
#' @seealso mmhg_to_kpa
#' @export
kpa_to_mmhg <- function(p) {
  mmhg_per_kpa <- 760 / 101.325
  if (!is.numeric(p)) {
    stop("`p` must be numeric.")
  }
  if (length(p) == 0) {
    numeric(0)
  } else {
    out <- p * mmhg_per_kpa
    if (!is.null(names(p))) names(out) <- names(p)
    out
  }
}

