# High-level interface for CO2 content calculations

Provides a unified entry point to calculate CO2 content in plasma or
whole blood using supported physiological models.

## Usage

``` r
co2_content(
  phase = c("blood", "plasma"),
  method = c("douglas", "siggaard_andersen", "loeppky"),
  content_units = c("ml/dL", "mmol/L"),
  pressure_units = c("kPa", "mmHg"),
  ...
)
```

## Arguments

- phase:

  Character string; one of `"plasma"` or `"blood"`. Selects whether to
  calculate plasma or whole-blood CO2 content.

- method:

  Character string; one of `"douglas"`, `"siggaard_andersen"`, or
  `"loeppky"`. Selects the underlying physiological model.

- content_units:

  Character string; currently `"ml/dL"` for Douglas-based calculations
  and `"mmol/L"` for Siggaard-Andersen-based calculations.

- pressure_units:

  Units for pressure parameters; one of `"kPa"` or `"mmHg"`.

- ...:

  Additional arguments passed on to the underlying implementation
  function. See the documentation for the corresponding lower-level
  functions for full details.

## Value

Numeric vector of CO2 content in the requested units.

## Examples

``` r
# Whole blood CO2 content using Douglas method (ml/dL)
co2_content(
  phase  = "blood",
  method = "douglas",
  content_units  = "ml/dL",
  pco2   = 5,
  haemoglobin_g_dl = 10,
  so2_fraction     = 0.9
)
#>      co2 
#> 47.55379 

# Whole blood CO2 content using Siggaard-Andersen method (mmol/L)
co2_content(
  phase  = "blood",
  method = "siggaard_andersen",
  content_units  = "mmol/L",
  hco3_mmols_l    = 17,
  pco2            = 5,
  haemoglobin_g_dl = 10,
  so2_fraction     = 0.9,
  pressure_units = "kPa"
)
#> [1] 17.1243
```
