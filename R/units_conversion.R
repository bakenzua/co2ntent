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

#' Convert mmols/dL to mls/dL
#'
#' @export
#'
#' @param mmols_dl Content in mmols/dL
#' @return Content in mls/dL

mmols_dl_to_mls_dl <- function(mmols_dl) {

  return(mmols_dl * 2.4789598)
}

#' Convert mls/dL to mmols/dL
#'
#' @export
#'
#' @param mls_dl Content in mls/dL
#' @return Content in mmols/dL

mls_dl_to_mmols_dl <- function(mls_dl) {

  return(mls_dl / 2.4789598)
}
