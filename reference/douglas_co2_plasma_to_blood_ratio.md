# Calculate the plasma to blood CO2 content ratio via Douglas method

`douglas_co2_plasma_to_blood_ratio` calculates the ratio between the CO2
content of plasma and the CO2 content of whole blood via the method
described by (Douglas et al. 1988) .

## Usage

``` r
douglas_co2_plasma_to_blood_ratio(haemoglobin_g_dl, so2_fraction, ph = 7.4)
```

## Arguments

- haemoglobin_g_dl:

  Haemoglobin g/dL. No default

- so2_fraction:

  Haemoglobin saturation as a fraction e.g 0 \< so2_fraction \< 1.0

- ph:

  pH (hydrogen ion concentration). Default 7.40

## Value

The CO2 content Plasma:Blood ratio

## Details

The plasma/blood ratio is calculated via an equation of the form derived
by (Visser 1960) and (McHardy 1967) , but with the coefficients derived
and published by (Douglas et al. 1988)

## References

Douglas AR, Jones NL, Reed JW (1988). “Calculation of whole blood CO2
content.” *J. Appl. Physiol.*, **65**(1), 473–477. McHardy GJ (1967).
“The relationship between the differences in pressure and content of
carbon dioxide in arterial and venous blood.” *Clin Sci*, **32**(2),
299–309. Visser BF (1960). “Pulmonary diffusion of carbon dioxide.”
*Phys Med Biol*, **5**, 155–166.
