#' Convert mmHg to kPa
#'
#' @export
#'
#' @param p Pressure in mmHg
#' @return Pressure in kPa

mmhg_to_kpa <- function(p) {

  return(p / (760000/101325))
}

#' Convert kPa to mmHg
#'
#' @export
#'
#' @param p Pressure in kPa
#' @return Pressure in mmHg

kpa_to_mmhg <- function(p) {

  return(p * (760000/101325))
}

#' Convert mmols/L to mls/dL
#'
#' @export
#'
#' @param mmols_l Content in mmols/L
#' @return Content in mls/dL

mmols_l_to_mls_dl <- function(mmols_l) {

  # return(mmols_dl * 2.4789598)
  return(mmols_l * 2.2710980)
}

#' Convert mls/dL to mmols/L
#'
#' @export
#'
#' @param mls_dl Content in mls/dL
#' @return Content in mmols/L

mls_dl_to_mmols_l <- function(mls_dl) {

  # return(mls_dl / 2.4789598)
  return(mls_dl / 2.2710980)
}
