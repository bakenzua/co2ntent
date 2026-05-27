# Calculate Erythrocyte pH as per Siggaard-Andersen method.

`siggard_a_erythrocyte_ph` calculates erythrocyte pH via the method
described by (Siggaard-Andersen et al. 1988) .

## Usage

``` r
siggaard_andersen_erythrocyte_ph(so2_fraction, ph = 7.4)
```

## Arguments

- so2_fraction:

  Haemoglobin saturation as a fraction e.g 0 \< so2_fraction \< 1.0

- ph:

  pH (hydrogen ion concentration). Default 7.40

## Value

The erythrocyte pH

## References

Siggaard-Andersen O, Wimberley PD, Fogh-Andersen N, Gøthgen IH (1988).
“Measured and derived quantities with modern pH and blood gas equipment:
Calculation algorithms with 54 equations.” *Scandinavian Journal of
Clinical and Laboratory Investigation*, **48**(sup189), 7-15.
[doi:10.1080/00365518809168181](https://doi.org/10.1080/00365518809168181)
. <https://doi.org/10.1080/00365518809168181>.
