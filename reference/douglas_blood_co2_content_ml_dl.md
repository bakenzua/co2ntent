# Calculate CO2 content of whole blood via Douglas method

`douglas_blood_co2_content_ml_dl` calculates CO2 content of whole blood
via the method described by (Douglas et al. 1988) .

## Usage

``` r
douglas_blood_co2_content_ml_dl(
  pco2,
  haemoglobin_g_dl,
  so2_fraction,
  ph = 7.4,
  temperature = 37,
  pco2_units = c("kPa", "mmHg")
)
```

## Arguments

- pco2:

  CO2 partial pressure

- haemoglobin_g_dl:

  Haemoglobin g/dL. No default

- so2_fraction:

  Haemoglobin saturation as a fraction e.g 0 \< so2_fraction \< 1.0

- ph:

  pH (hydrogen ion concentration). Default 7.40

- temperature:

  temperature in celsius. Default 37c

- pco2_units:

  Units for `pco2`; one of `"kPa"` or `"mmHg"`.

## Value

The CO2 content of plasma in ml/dL

## Details

CO2 content of plasma is calculated via
[`co2ntent::douglas_plasma_co2_content_ml_dl`](https://bakenzua.github.io/co2ntent/reference/douglas_plasma_co2_content_ml_dl.md)
which is then multiplied by the calculated CO2 content blood:plasma
ratio calculated via
[`co2ntent::douglas_co2_plasma_to_blood_ratio`](https://bakenzua.github.io/co2ntent/reference/douglas_co2_plasma_to_blood_ratio.md).

This is therefore an all in one method, which calculates all the other
required parameters.

## References

Douglas AR, Jones NL, Reed JW (1988). “Calculation of whole blood CO2
content.” *J. Appl. Physiol.*, **65**(1), 473–477.
