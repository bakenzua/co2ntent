# Calculate SO2 from pO2 of blood.

`kelman_po2_to_so2` calculates haemoglobin oxygen saturation from
partial pressure of oxygen in blood via the method described by (Kelman
1966) .

## Usage

``` r
kelman_po2_to_so2(
  po2,
  temperature = 37,
  ph = 7.4,
  pco2 = 5.332895,
  pressure_units = c("kPa", "mmHg")
)
```

## Arguments

- po2:

  O2 partial pressure

- temperature:

  temperature in celsius. Default 37c

- ph:

  pH (hydrogen ion concentration). Default 7.40

- pco2:

  CO2 partial pressure. Default 5.332895kPa (40mmHg)

- pressure_units:

  Input parameters are kPa, otherwise use mmHg

## Value

Haemoglobin saturation as fraction

## Note

`kelman_po2_to_so2` doesn't work for po2 \< 0.5kpa as returned SO2 can
become negative

## References

Kelman GR (1966). “Digital computer subroutine for the conversion of
oxygen tension into saturation.” *Journal of Applied Physiology*,
**21**(4), 1375-1376.
[doi:10.1152/jappl.1966.21.4.1375](https://doi.org/10.1152/jappl.1966.21.4.1375)
. PMID: 5916678.
