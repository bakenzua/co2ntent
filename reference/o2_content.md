# High-level interface for O2 content of blood

Provides a unified entry point to calculate the oxygen content of blood
in mL of O2 per dL of blood.

## Usage

``` r
o2_content(
  po2,
  so2_fraction,
  haemoglobin_g_dl,
  hufners_constant = 1.306,
  pressure_units = c("kPa", "mmHg"),
  content_units = c("ml/dL", "mmol/L")
)
```

## Arguments

- po2:

  O2 partial pressure.

- so2_fraction:

  Haemoglobin saturation as a fraction (e.g. \\0 \< so2\\fraction \<
  1.0\\).

- haemoglobin_g_dl:

  Haemoglobin concentration in g/dL.

- hufners_constant:

  Oxygen capacity of human haemoglobin in mL/g. Default is `1.306` as
  per Gregory (1974).

- pressure_units:

  Unit for `po2`; one of `"kPa"` or `"mmHg"`.

- content_units:

  Unit for result; one of `"mmol/L"` or `"ml/dL"`.

## Value

Numeric vector of O2 content in mL/dL.

## Examples

``` r
o2_content(
  po2            = 10,
  so2_fraction   = 0.95,
  haemoglobin_g_dl = 15,
  pressure_units = "kPa"
)
#> [1] 18.8355
```
