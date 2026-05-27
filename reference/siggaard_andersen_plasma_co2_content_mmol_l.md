# Calculate plasma CO2 content as per Siggaard-Andersen method.

`siggaard_andersen_plasma_co2_content_mmol_l` calculates plasma CO2
content via the method described by (Siggaard-Andersen et al. 1988) .

## Usage

``` r
siggaard_andersen_plasma_co2_content_mmol_l(
  hco3_mmols_l,
  pco2,
  pco2_units = c("kPa", "mmHg")
)
```

## Arguments

- hco3_mmols_l:

  plasma bicarbonate concentration mmols/dL

- pco2:

  CO2 partial pressure

- pco2_units:

  Unit for `pco2`; one of `"kPa"` or `"mmHg"`

## Value

The CO2 content of plasma in mmol/L

## References

Siggaard-Andersen O, Wimberley PD, Fogh-Andersen N, Gøthgen IH (1988).
“Measured and derived quantities with modern pH and blood gas equipment:
Calculation algorithms with 54 equations.” *Scandinavian Journal of
Clinical and Laboratory Investigation*, **48**(sup189), 7-15.
[doi:10.1080/00365518809168181](https://doi.org/10.1080/00365518809168181)
. <https://doi.org/10.1080/00365518809168181>.
