# Calculate plasma bicarbonate concentration as per Henderson-Hasselbalch equation.

`actual_bicarbonate_content_mmol_l` calculates plasma bicarbonate
concentration as per the Henderson-Hasselbalch equation.

## Usage

``` r
actual_bicarbonate_content_mmol_l(
  pco2,
  ph = 7.4,
  pco2_units = c("kPa", "mmHg")
)
```

## Arguments

- pco2:

  CO2 partial pressure

- ph:

  pH (hydrogen ion concentration). Default 7.40

- pressure_units:

  Unit for `pco2`; one of `"kPa"` or `"mmHg"`.

## Value

The HCO3 concentration of plasma in mmol/dL

## Details

Calculation is a straightforward Henderson-Hasselbalch rearrangement.
This relies on the solubility coefficient of CO2 in plasma.
(Siggaard-Andersen et al. 1988) provide a constant of 0.230 mmol/L/kPa.

## References

Douglas AR, Jones NL, Reed JW (1988). “Calculation of whole blood CO2
content.” *J. Appl. Physiol.*, **65**(1), 473–477. Siggaard-Andersen O,
Wimberley PD, Fogh-Andersen N, Gøthgen IH (1988). “Measured and derived
quantities with modern pH and blood gas equipment: Calculation
algorithms with 54 equations.” *Scandinavian Journal of Clinical and
Laboratory Investigation*, **48**(sup189), 7-15.
[doi:10.1080/00365518809168181](https://doi.org/10.1080/00365518809168181)
. <https://doi.org/10.1080/00365518809168181>.
