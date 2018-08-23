#' Check vector of pCO2 values.
#'
#' \code{pco2_param_check} checks a vector of pCO2 values for abnormal or missing values.
#' Abnormal values are designed to catch values entered in error rather than representing a 'normal'
#' physiological range. If abnormal values are seen, a warning is raised. If missing values are
#' found an error is raised. Abnormal values are defined as pCO2 < 1kPa or pCO2 > 20kPa
#'
#' @param pco2 CO2 partial pressure
#' @param inputs_are_kpa If TRUE, input pCO2 is in kPa, if FALSE use mmHg
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return No return value.
#'
#'
pco2_param_check <- function(pco2, inputs_are_kpa=TRUE, skip_range_check=FALSE) {

  if(inputs_are_kpa) {
    max_pco2 <- 30
    min_pco2 <- 1
  } else {
    max_pco2 <- kpa_to_mmhg(30)
    min_pco2 <- kpa_to_mmhg(1)
  }

  # error checking
  if (!skip_range_check) {
    if (max(pco2) > max_pco2) {
      warning(paste0("pco2 parameter contains high values: ", max(pco2)))
    }

    if (min(pco2) < min_pco2) {
      warning(paste0("pco2 parameter contains low values: ", min(pco2)))
    }
  }

  if (sum(is.na(pco2)) > 0) {
    stop("pco2 parameter contains missing values")
  }
}

#' Check vector of pO2 values.
#'
#' \code{po2_param_check} checks a vector of pO2 values for abnormal or missing values.
#' Abnormal values are designed to catch values entered in error rather than representing a 'normal'
#' physiological range. If abnormal values are seen, a warning is raised. If missing values are
#' found an error is raised. Abnormal values are defined as pO2 < 1kPa or pO2 > 100kPa
#'
#' @param po2 O2 partial pressure
#' @param inputs_are_kpa If TRUE, input pO2 is in kPa, if FALSE use mmHg
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return No return value.
#'
#'
po2_param_check <- function(po2, inputs_are_kpa=TRUE, skip_range_check=FALSE) {

  if(inputs_are_kpa) {
    max_po2 <- 100
    min_po2 <- 1
  } else {
    max_po2 <- kpa_to_mmhg(100)
    min_po2 <- kpa_to_mmhg(1)
  }

  # error checking
  if (!skip_range_check) {
    if (max(po2) > max_po2) {
      warning(paste0("po2 parameter contains high values: ", max(po2)))
    }

    if (min(po2) < min_po2) {
      warning(paste0("po2 parameter contains low values: ", min(po2)))
    }
  }

  if (sum(is.na(po2)) > 0) {
    stop("po2 parameter contains missing values")
  }
}

#' Check vector of temperature values.
#'
#' \code{temperature_param_check} checks a vector of temperature values for abnormal or missing values.
#' Abnormal values are designed to catch values entered in error rather than representing a 'normal'
#' physiological range. If abnormal values are seen, a warning is raised. If missing values are
#' found an error is raised. Abnormal values are defined as T < 28c  or T > 44c
#'
#' @param temperature temperature in celcius.
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return No return value.
#'
#'
temperature_param_check <- function(temperature, skip_range_check=FALSE) {
  # error checking temperature
  if (!skip_range_check) {
    if (max(temperature) > 44) {
      warning(paste0("temperature parameter contains high values: ", max(temperature)))
    }

    if (min(temperature) < 28) {
      warning(paste0("temperature parameter contains low values: ", min(temperature)))
    }
  }

  if (sum(is.na(temperature)) > 0) {
    stop("temperature parameter contains missing values")
  }
}

#' Check vector of ph values.
#'
#' \code{ph_param_check} checks a vector of pH values for abnormal or missing values.
#' Abnormal values are designed to catch values entered in error rather than representing a 'normal'
#' physiological range. If abnormal values are seen, a warning is raised. If missing values are
#' found an error is raised. Abnormal values are defined as pH < 6.8  or pH > 7.7
#'
#' @param ph pH (hydrogen ion concentration)
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return No return value.
#'
#'
ph_param_check <- function(ph, skip_range_check=FALSE) {
  if (!skip_range_check) {
    if (max(ph) > 7.8) {
      warning(paste0("ph parameter contains high values: ", max(ph)))
    }

    if (min(ph) < 6.5) {
      warning(paste0("ph parameter contains low values: ", min(ph)))

    }
  }

  if (sum(is.na(ph)) > 0) {
    stop("ph parameter contains missing values")
  }
}


#' Check vector of fractional SO2 values.
#'
#' \code{so2_fraction_param_check} checks a vector of fractional SO2 values for abnormal or missing values.
#' If abnormal values are seen, a warning is raised. If missing values are
#' found an error is raised. Abnormal values are defined as so2_fraction < 0  or so2_fraction > 1
#'
#' @param so2_fraction Haemoglobin saturation as a fraction
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return No return value.
#'
#'
so2_fraction_param_check <- function(so2_fraction, skip_range_check=FALSE) {
  if (!skip_range_check) {
    if (max(so2_fraction) > 1) {
      warning(paste0("so2_fraction parameter contains high values: ", max(so2_fraction)))
    }

    if (min(so2_fraction) < 0) {
      warning(paste0("so2_fraction parameter contains low values: ", min(so2_fraction)))
    }
  }

  if (sum(is.na(so2_fraction)) > 0) {
    stop("so2_fraction parameter contains missing values")
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
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return No return value.
#'
#'
haemoglobin_g_dl_param_check <- function(haemoglobin_g_dl, skip_range_check=FALSE) {
  if (!skip_range_check) {
    if (max(haemoglobin_g_dl) > 20) {
      warning(paste0("haemoglobin_g_dl parameter contains high values: ", max(haemoglobin_g_dl)))
    }

    if (min(haemoglobin_g_dl) < 2) {
      warning(paste0("haemoglobin_g_dl parameter contains low values: ", min(haemoglobin_g_dl)))
    }
  }

  if (sum(is.na(haemoglobin_g_dl)) > 0) {
    stop("haemoglobin_g_dl parameter contains missing values")
  }
}


#' Check vector of HCO3 values.
#'
#' \code{bicarbonate_mmol_dl_param_check} checks a vector of Bicarbonate values for abnormal or missing values.
#' Abnormal values are designed to catch values entered in error rather than representing a 'normal'
#' physiological range. If abnormal values are seen, a warning is raised. If missing values are
#' found an error is raised. Abnormal values are defined as HCO3 < 2 mmol/dL  or HCO3 > 75 mmol/dL
#'
#' @param hco3_mmols_l Bicarbonate mmol/L.
#' @param skip_range_check If TRUE skip checking of parameter ranges. Default: FALSE
#' @return No return value.
#'
#'
bicarbonate_mmol_l_param_check <- function(hco3_mmols_l, skip_range_check=FALSE) {
  if (!skip_range_check) {
    if (max(hco3_mmols_l) > 75) {
      warning(paste0("hco3_mmols_dl parameter contains high values: ", max(hco3_mmols_l)))
    }

    if (min(hco3_mmols_l) < 2) {
      warning(paste0("hco3_mmols_dl parameter contains low values: ", min(hco3_mmols_l)))
    }
  }

  if (sum(is.na(hco3_mmols_l)) > 0) {
    stop("hco3_mmols_dl parameter contains missing values")
  }
}

