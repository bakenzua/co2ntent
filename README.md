co2ntent
-------

An R package for calculating carbon dioxide and oxygen content of blood.

The co2ntent package provides several canonical formulae from the
literature, for the calculation of carbon dioxide and oxygen content
of the blood.

Some data sets presented in the referenced literature are also included.

Several helper methods for units conversion and other minor tasks are
provided.

Carbon dioxide content calculation models derived by the author are
also provided.

Installing the development version
-------

Download the source code from git, unzip it if necessary, and then type `R CMD INSTALL pROC`. Alternatively, you can use the [devtools](https://github.com/hadley/devtools/wiki) package by [Hadley Wickham](http://had.co.nz/) to automate the process (make sure you follow [the full instructions to get started](http://www.rstudio.com/projects/devtools/)):

``` r
if (! requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("bakenzua/co2ntent")
```

Example
-------

This is an example demonstrating how `con2tent` can be used with tidyvverse functions on the inbuilt dataset.

``` r
library(tidyverse)
library(co2ntent)

names(co2ntent::douglas_table_3)
# [1] "subject"                  "sample_type"              "ph"                      
# [4] "haemoglobin_g_dl"         "so2_fraction"             "blood_co2_content_ml_dl" 
# [7] "pco2_torr"                "plasma_co2_content_ml_dl"

co2ntent::douglas_table_3 %>%
  # calculate douglas plasma content
  mutate(douglas_calculated_content_plasma = pmap_dbl(
    list(
      pco2=pco2_torr,
      ph=ph
    ),
    douglas_plasma_co2_content_ml_dl,
    inputs_are_kpa=FALSE
    
  )) %>%
  # calculate douglas blood content
  mutate(douglas_calculated_content_blood = pmap_dbl(
    list(
      pco2=pco2_torr, 
      haemoglobin_g_dl=haemoglobin_g_dl, 
      so2_fraction=so2_fraction, 
      ph=ph
    ),
    douglas_blood_co2_content_ml_dl,
    inputs_are_kpa=FALSE
    
  )) %>%
  # calculate hco3
  mutate(plasma_hco3 = map2_dbl(pco2_torr, ph, plasma_bicarbonate_content, inputs_are_kpa=FALSE)) %>%
  # calculate siggaard plasma content
  mutate(siggaard_calculated_content_plasma_mmols = map2_dbl(plasma_hco3, pco2_torr, siggaard_andersen_plasma_co2_content_mmol_dl, inputs_are_kpa=FALSE)) %>%
  # calculate siggaard blood content
  mutate(siggaard_calculated_content_blood_mmols = pmap_dbl(
    list(
      hco3_mmols_dl=plasma_hco3,
      pco2=pco2_torr, 
      haemoglobin_g_dl=haemoglobin_g_dl, 
      so2_fraction=so2_fraction, 
      ph=ph
    ),
    siggaard_andersen_blood_co2_content_mmol_dl,
    inputs_are_kpa=FALSE
    
  )) %>%
  # units conversion
  mutate(siggaard_calculated_content_plasma_mls = map_dbl(siggaard_calculated_content_plasma_mmols, mmols_dl_to_mls_dl)) %>%
  mutate(siggaard_calculated_content_blood_mls = map_dbl(siggaard_calculated_content_blood_mmols, mmols_dl_to_mls_dl)) -> df

```
