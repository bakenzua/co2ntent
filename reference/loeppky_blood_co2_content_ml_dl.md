# Calculate CO2 content of blood via Loeppky method

`loeppky_blood_co2_content_ml_dl` calculates the CO2 content of blood
via the method described by (Loeppky et al. 1983) .

## Usage

``` r
loeppky_blood_co2_content_ml_dl(pco2, pco2_units = c("kPa", "mmHg"))
```

## Arguments

- pco2:

  CO2 partial pressure

- pco2_units:

  Unit for `pco2`; one of `"kPa"` or `"mmHg"`.

## Value

The CO2 content of plasma in ml/dL

## References

Loeppky JA, Luft UC, Fletcher ER (1983). “Quantitative description of
whole blood CO2 dissociation curve and Haldane effect.” *Respir
Physiol*, **51**(2), 167–181.
