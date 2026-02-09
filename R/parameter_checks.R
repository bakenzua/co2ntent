#' Check vector of pCO2 values.
#'
#' \code{pco2_param_check} checks a vector of pCO2 values for abnormal or missing values.
#' Abnormal values are designed to catch values entered in error rather than representing a 'normal'
#' physiological range. If abnormal values are seen, a warning is raised. 
#' Abnormal values are defined as pCO2 < 1kPa or pCO2 > 20kPa
#'
#' @param pco2 CO2 partial pressure
#' @param pco2_units Unit for \code{pco2}; one of \code{"kPa"} or \code{"mmHg"}.
#' @return No return value.
#'
#'
pco2_param_check <- function(
  pco2,
  pco2_units   = c("kPa", "mmHg")
) {
  pco2_units <- match.arg(pco2_units)
  if (pco2_units == "kPa") {
    max_pco2 <- 30
    min_pco2 <- 1
  } else {
    max_pco2 <- kpa_to_mmhg(30)
    min_pco2 <- kpa_to_mmhg(1)
  }

  # error checking
    if (max(pco2) > max_pco2) {
      warning(paste0("pco2 parameter contains high values: ", max(pco2)))
    }

    if (min(pco2) < min_pco2) {
      warning(paste0("pco2 parameter contains low values: ", min(pco2)))
    }
  }

#' Check vector of pO2 values.
#'
#' \code{po2_param_check} checks a vector of pO2 values for abnormal or missing values.
#' Abnormal values are designed to catch values entered in error rather than representing a 'normal'
#' physiological range. If abnormal values are seen, a warning is raised.
#' Abnormal values are defined as pO2 < 1kPa or pO2 > 100kPa
#'
#' @param po2 O2 partial pressure
#' @param po2_units Unit for \code{po2}; one of \code{"kPa"} or \code{"mmHg"}.
#' @return No return value.
#'
#'
po2_param_check <- function(
  po2,
  po2_units   = c("kPa", "mmHg")
) {
  po2_units <- match.arg(po2_units)

  if (po2_units == "kPa") {
    max_po2 <- 100
    min_po2 <- 1
  } else {
    max_po2 <- kpa_to_mmhg(100)
    min_po2 <- kpa_to_mmhg(1)
  }

  # error checking
    if (max(po2) > max_po2) {
      warning(paste0("po2 parameter contains high values: ", max(po2)))
    }

    if (min(po2) < min_po2) {
      warning(paste0("po2 parameter contains low values: ", min(po2)))
    }
}

#' Check vector of temperature values.
#'
#' \code{temperature_param_check} checks a vector of temperature values for abnormal or missing values.
#' Abnormal values are designed to catch values entered in error rather than representing a 'normal'
#' physiological range. If abnormal values are seen, a warning is raised. If missing values are
#' found an error is raised. Abnormal values are defined as T < 28c  or T > 44c
#'
#' @param temperature temperature in celsius.
#' @return No return value.
#'
#' @export
temperature_param_check <- function(temperature) {
  # error checking temperature
    if (max(temperature) > 44) {
      warning(paste0("temperature parameter contains high values: ", max(temperature)))
    }

    if (min(temperature) < 28) {
      warning(paste0("temperature parameter contains low values: ", min(temperature)))
    }

}

#' Check vector of ph values.
#'
#' \code{ph_param_check} checks a vector of pH values for abnormal or missing values.
#' Abnormal values are designed to catch values entered in error rather than representing a 'normal'
#' physiological range. If abnormal values are seen, a warning is raised. 
#' Abnormal values are defined as pH < 6.8  or pH > 7.7
#'
#' @param ph pH (hydrogen ion concentration)
#' @return No return value.
#'
#'  @export
ph_param_check <- function(ph) {
    if (max(ph) > 7.8) {
      warning(paste0("ph parameter contains high values: ", max(ph)))
    }

    if (min(ph) < 6.5) {
      warning(paste0("ph parameter contains low values: ", min(ph)))
    }
}


#' Check vector of fractional SO2 values.
#'
#' \code{so2_fraction_param_check} checks a vector of fractional SO2 values for
#' abnormal values. If abnormal values are seen, a warning is
#' raised. 
#' Abnormal values are defined as so2_fraction < 0  or so2_fraction > 1
#'
#' @param so2_fraction Haemoglobin saturation as a fraction
#' @return No return value.
#'
#' @export
so2_fraction_param_check <- function(so2_fraction) {
  
  if (max(so2_fraction, na.rm = TRUE) > 1) {
    warning(
      paste0(
        "so2_fraction parameter contains values more than 1: ",
        max(so2_fraction)
      )
    )
  }

  if (min(so2_fraction, na.rm = TRUE) < 0) {
    warning(paste0("so2_fraction parameter contains values less than 0: ", min(so2_fraction)))
  }

}


#' Check vector of Haemoglobin values.
#'
#' \code{haemoglobin_g_dl_param_check} checks a vector of Haemoglobin values for abnormal or missing values.
#' Abnormal values are designed to catch values entered in error rather than representing a 'normal'
#' physiological range. If abnormal values are seen, a warning is raised. If missing values are
#' found an error is raised. Abnormal values are defined as Hb < 2g/dL  or Hb > 20 g/dL
#'
#' @param haemoglobin_g_dl Haemoglobin g/dL.
#' @return No return value.
#'
#' @export
haemoglobin_g_dl_param_check <- function(haemoglobin_g_dl) {

  if (max(haemoglobin_g_dl) > 20) {
    warning(paste0("haemoglobin_g_dl parameter contains high values: ", max(haemoglobin_g_dl, na.rm = TRUE)))
  }

  if (min(haemoglobin_g_dl) < 2) {
    warning(paste0("haemoglobin_g_dl parameter contains low values: ", min(haemoglobin_g_dl, na.rm = TRUE)))
  }
}


#' Check vector of HCO3 values.
#'
#' \code{bicarbonate_mmol_dl_param_check} checks a vector of Bicarbonate values for abnormal or missing values.
#' Abnormal values are designed to catch values entered in error rather than representing a 'normal'
#' physiological range. If abnormal values are seen, a warning is raised. 
#' Abnormal values are defined as HCO3 < 2 mmol/dL  or HCO3 > 75 mmol/dL
#'
#' @param hco3_mmols_l Bicarbonate mmol/L.
#' @return No return value.
#'
#' @export
bicarbonate_mmol_l_param_check <- function(hco3_mmols_l) {

    if (max(hco3_mmols_l) > 75) {
      warning(paste0("hco3_mmols_dl parameter contains high values: ", max(hco3_mmols_l)))
    }

    if (min(hco3_mmols_l) < 2) {
      warning(paste0("hco3_mmols_dl parameter contains low values: ", min(hco3_mmols_l)))
    }
}
