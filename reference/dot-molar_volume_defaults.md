# Default molar volumes for gases (internal)

Named numeric vector of default molar volumes in decilitres per
millimole (dL/mmol). Values are chosen for common physiological gases: -
co2: carbon dioxide - o2: oxygen - ideal: ideal gas approximation

## Usage

``` r
.molar_volume_defaults()
```

## Value

Named numeric vector (dL/mmol).

## Details

These defaults are used by mmols_l_to_mls_dl() and mls_dl_to_mmols_l()
when \`molar_volume\` is not supplied.

## See also

mmols_l_to_mls_dl, mls_dl_to_mmols_l

## Examples

``` r
.molar_volume_defaults()["o2"]
#> Error in .molar_volume_defaults(): could not find function ".molar_volume_defaults"
```
