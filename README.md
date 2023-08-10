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

You can use the [devtools](https://github.com/hadley/devtools/wiki) package by [Hadley Wickham](http://had.co.nz/) to automate the process (make sure you follow [the full instructions to get started](http://www.rstudio.com/projects/devtools/)):

``` r
if (! requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("bakenzua/co2ntent")
```

Example
-------

This is an example demonstrating how `co2tent` can be used with tidyvverse functions on the inbuilt dataset.

``` r
library(tidyverse)
library(co2ntent)

names(co2ntent::douglas_table_3)
# [1] "subject"                  "sample_type"              "ph"                      
# [4] "haemoglobin_g_dl"         "so2_fraction"             "blood_co2_content_ml_dl" 
# [7] "pco2_torr"                "plasma_co2_content_ml_dl"

co2ntent::douglas_table_3 |>
  mutate(
    # calculate douglas plasma content
    douglas_calculated_content_plasma_ml_dl = pmap_dbl(
      list(
        pco2=pco2_torr,
        ph=ph
      ),
      douglas_plasma_co2_content_ml_dl,
      inputs_are_kpa=FALSE

    ),
    # calculate douglas blood content
    douglas_calculated_content_blood_ml_dl = pmap_dbl(
      list(
        pco2=pco2_torr,
        haemoglobin_g_dl=haemoglobin_g_dl,
        so2_fraction=so2_fraction,
        ph=ph
      ),
      douglas_blood_co2_content_ml_dl,
      inputs_are_kpa=FALSE

    ),
    # calculate hco3
    plasma_hco3_mmol_l = map2_dbl(
      pco2_torr, ph, 
      siggaard_andersen_plasma_bicarbonate_content_mmol_l, 
      inputs_are_kpa=FALSE
    ),

    # calculate siggaard plasma content
    siggaard_calculated_content_plasma_mmol_l = map2_dbl(
      plasma_hco3_mmol_l, 
      pco2_torr, 
      siggaard_andersen_plasma_co2_content_mmol_l, 
      inputs_are_kpa=FALSE
    ),
    # calculate siggaard blood content
    siggaard_calculated_content_blood_mmol_l = pmap_dbl(
      list(
        hco3_mmols_l=plasma_hco3_mmol_l,
        pco2=pco2_torr,
        haemoglobin_g_dl=haemoglobin_g_dl,
        so2_fraction=so2_fraction,
        ph=ph
      ),
      siggaard_andersen_blood_co2_content_mmol_l,
      inputs_are_kpa=FALSE
    ),
    # units conversion
    siggaard_calculated_content_plasma_ml_dl = map_dbl(
      siggaard_calculated_content_plasma_mmol_l, 
      mmols_l_to_mls_dl
    ),
    siggaard_calculated_content_blood_ml_dl = map_dbl(
      siggaard_calculated_content_blood_mmol_l, 
      mmols_l_to_mls_dl
    )
  ) |>
  select(
    blood_co2_content_ml_dl, douglas_calculated_content_blood_ml_dl, siggaard_calculated_content_blood_ml_dl
  ) |>
  pivot_longer(
    c(douglas_calculated_content_blood_ml_dl, siggaard_calculated_content_blood_ml_dl),
    names_to = "calculated_method",
    values_to = "calculated_content"
  ) |>
  ggplot(aes(blood_co2_content_ml_dl, calculated_content, colour=calculated_method)) + 
    geom_point() +
    geom_smooth(method='lm') +
    geom_abline(slope=1, intercept=0) + 
    theme_classic() +
    labs(
      title="Comparison of methods of CO2 content calculation vs Actual Content",
      subtitle = "Original data from Loeppky, Luft, and Fletcher (1983)",
      x = "Actual CO2 content ml/dl",
      y = "Calculated CO2 content ml/dl",
      colour = "Calculation Method"
    ) +
    theme(
      legend.position = "bottom"
    )

```
![co2ntent_readme_plot](https://github.com/bakenzua/co2ntent/assets/17120936/16413e7c-2b5e-4776-9020-979757e1bc0a)