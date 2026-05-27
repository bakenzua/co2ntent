# Convert mmHg to kPa

Convert pressure from millimetres of mercury (mmHg) to kilopascals
(kPa).

## Usage

``` r
mmhg_to_kpa(p)
```

## Arguments

- p:

  Numeric vector of pressures in mmHg. NA values are preserved.

## Value

Numeric vector of pressures in kPa (same length and names as \`p\`).

## Note

1 atm = 760 mmHg = 101.325 kPa.

## See also

kpa_to_mmhg

## Examples

``` r
mmhg_to_kpa(c(760, 380, NA))
#> [1] 101.3250  50.6625       NA
```
