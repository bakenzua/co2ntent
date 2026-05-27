# Calculate whole blood CO2 content as per Siggaard-Andersen method.

`siggaard_andersen_blood_co2_content_mmol_l` calculates whole blood CO2
content via the method described by (Siggaard-Andersen et al. 1988) .

## Usage

``` r
siggaard_andersen_blood_co2_content_mmol_l(
  hco3_mmols_l,
  pco2,
  haemoglobin_g_dl,
  so2_fraction,
  ph = 7.4,
  pco2_units = c("kPa", "mmHg")
)
```

## Arguments

- hco3_mmols_l:

  plasma bicarbonate concentration mmols/L

- pco2:

  CO2 partial pressure

- haemoglobin_g_dl:

  Haemoglobin g/dL. No default

- so2_fraction:

  Haemoglobin saturation as a fraction e.g 0 \< so2_fraction \< 1.0

- ph:

  pH (hydrogen ion concentration). Default 7.40

- pco2_units:

  Unit for `pco2`; one of `"kPa"` or `"mmHg"`.

## Value

The CO2 content of blood in mmol/L

## References

Siggaard-Andersen O, Wimberley PD, Fogh-Andersen N, Gøthgen IH (1988).
“Measured and derived quantities with modern pH and blood gas equipment:
Calculation algorithms with 54 equations.” *Scandinavian Journal of
Clinical and Laboratory Investigation*, **48**(sup189), 7-15.
[doi:10.1080/00365518809168181](https://doi.org/10.1080/00365518809168181)
. <https://doi.org/10.1080/00365518809168181>.
