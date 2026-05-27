# Convert mmol/L ↔ mL/dL for CO2, O2 or ideal gas

Convert between mmol per litre and mL per decilitre for gases.

## Usage

``` r
mmols_l_to_mls_dl(x, gas = c("ideal", "co2", "o2"), molar_volume = NULL)

mls_dl_to_mmols_l(x, gas = c("ideal", "co2", "o2"), molar_volume = NULL)
```

## Arguments

- x:

  Numeric vector: values to convert (mmol/L for mmols_l_to_mls_dl, mL/dL
  for mls_dl_to_mmols_l).

- gas:

  Character; one of "co2", "o2", "ideal". Chooses the molar volume
  default.

- molar_volume:

  Optional numeric scalar to override the default molar volume (units:
  dL/mmol).

## Value

Numeric vector of same length and (when appropriate) names as \`x\`.

## Details

The conversion uses the molar volume in dL per mmol. With \$V_m\$ in
dL/mmol: \$\$ mL/dL = mmol/L (10 V_m). \$\$

## Examples

``` r
mmols_l_to_mls_dl(c(1, 2.5), gas = "co2")
#> [1] 2.22630 5.56575
mls_dl_to_mmols_l(c(2.2263, 5.56575), gas = "co2")
#> [1] 1.0 2.5
```
