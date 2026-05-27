# High-level interface for pO2 to SO2

Calculates haemoglobin oxygen saturation from partial pressure of oxygen
in blood via a chosen method (currently Kelman only).

## Usage

``` r
po2_to_so2(
  po2,
  temperature = 37,
  ph = 7.4,
  pco2 = 5.332895,
  pressure_units = c("kPa", "mmHg"),
  method = c("kelman")
)
```

## Arguments

- po2:

  O2 partial pressure.

- temperature:

  Temperature in Celsius. Default 37.

- ph:

  pH (hydrogen ion concentration). Default 7.40.

- pco2:

  CO2 partial pressure. Default 5.332895 kPa (40 mmHg).

- pressure_units:

  Units for `po2` and `pco2`; one of `"kPa"` or `"mmHg"`.

- method:

  Character string; currently only `"kelman"` is supported.

## Value

Numeric vector of haemoglobin saturation as a fraction.

## Examples

``` r
po2_to_so2(
  po2  = 10,
  temperature = 37,
  ph   = 7.4,
  pco2 = 5.3329,
  pressure_units = "kPa"
)
#> [1] 0.9507951
```
