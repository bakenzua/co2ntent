# Calculate the solubility coefficient of CO2 in plasma.

`douglas_co2_plasma_solubility` calculates the solubility coefficient of
CO2 in plasma via the formula described by (Douglas et al. 1988) .

## Usage

``` r
douglas_co2_plasma_solubility(temperature = 37, skip_range_check = FALSE)
```

## Arguments

- temperature:

  Plasma Temperature in Celsius. Default 37c

## Value

s The solubility coefficient of CO2 in plasma mmol/dL/kPa

## References

Douglas AR, Jones NL, Reed JW (1988). “Calculation of whole blood CO2
content.” *J. Appl. Physiol.*, **65**(1), 473–477.
