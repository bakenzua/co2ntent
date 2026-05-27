# Convert kPa to mmHg

Convert pressure from kilopascals (kPa) to millimetres of mercury
(mmHg).

## Usage

``` r
kpa_to_mmhg(p)
```

## Arguments

- p:

  Numeric vector of pressures in kPa. NA values are preserved.

## Value

Numeric vector of pressures in mmHg (same length and names as \`p\`).

## See also

mmhg_to_kpa

## Examples

``` r
kpa_to_mmhg(c(101.325, 50.6625, NA))
#> [1] 760 380  NA
```
