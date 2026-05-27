# Calculate CO2 content of plasma via Douglas method

`douglas_plasma_co2_content_ml_dl` calculates a hypothetical
intermediary CO2 content of plasma via the method described by (Douglas
et al. 1988) .

## Usage

``` r
douglas_plasma_co2_content_ml_dl(
  pco2,
  temperature = 37,
  ph = 7.4,
  pco2_units = c("kPa", "mmHg")
)
```

## Arguments

- pco2:

  CO2 partial pressure

- temperature:

  temperature in celsius. Default 37c

- ph:

  pH (hydrogen ion concentration). Default 7.40

- pco2_units:

  Unit for `pco2`; one of `"kPa"` or `"mmHg"`.

## Value

The CO2 content of plasma in ml/dL

## Details

CO2 content of plasma is calculated from plasma temperature, pH, the
calculated solubility coefficient of CO2 in plasma and the apparent pK
of the CO2-HCO3 equilibrium of plasma. The latter two parameters are
calculated by
[`co2ntent::douglas_co2_plasma_solubility`](https://bakenzua.github.io/co2ntent/reference/douglas_co2_plasma_solubility.md)
and
[`co2ntent::douglas_apparent_pk_co2_hco3`](https://bakenzua.github.io/co2ntent/reference/douglas_apparent_pk_co2_hco3.md).

## References

Douglas AR, Jones NL, Reed JW (1988). “Calculation of whole blood CO2
content.” *J. Appl. Physiol.*, **65**(1), 473–477.
