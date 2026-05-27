# Calculate virtual pO2.

`kelman_virtual_po2` calculates a 'virtual po2', a pO2 corrected for ph
pco2 and temperature, as per the method described by (Kelman 1966) .

## Usage

``` r
kelman_virtual_po2(
  po2,
  pco2,
  temperature = 37,
  ph = 7.4,
  pressure_units = c("kPa", "mmHg")
)
```

## Arguments

- po2:

  O2 partial pressure

- pco2:

  CO2 partial pressure. Default 5.332895kPa (40mmHg)

- temperature:

  temperature in celsius. Default 37c

- ph:

  pH (hydrogen ion concentration). Default 7.40

- pressure_units:

  Input parameters are kPa, otherwise use mmHg

## Value

Vector of virtual pO2

## Details

Units for pO2 and pCO2 should be in the same units (mmHg or kPa) as
specified by pressure_units. The result is returned in same units

## References

Kelman GR (1966). “Digital computer subroutine for the conversion of
oxygen tension into saturation.” *Journal of Applied Physiology*,
**21**(4), 1375-1376.
[doi:10.1152/jappl.1966.21.4.1375](https://doi.org/10.1152/jappl.1966.21.4.1375)
. PMID: 5916678.
