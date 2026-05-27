# Calculate SO2 from pO2 of blood.

`kelman_std_po2_to_so2` calculates haemoglobin oxygen saturation from
partial pressure of oxygen in blood via the method described by (Kelman
1966) . This form makes no correction for acid/base disturbance,
assuming pH=7.4, temperature=37c and pCO2=40mmHg

## Usage

``` r
kelman_std_po2_to_so2(po2, po2_units = c("kPa", "mmHg"))
```

## Arguments

- po2:

  O2 partial pressure

- inputs_are_kpa:

  Input parameters are kPa, otherwise use mmHg

## Value

Haemoglobin saturation as fraction

## Note

`kelman_std_po2_to_so2` doesn't work for po2 \< 0.5kpa as returned SO2
can become negative

## References

Kelman GR (1966). “Digital computer subroutine for the conversion of
oxygen tension into saturation.” *Journal of Applied Physiology*,
**21**(4), 1375-1376.
[doi:10.1152/jappl.1966.21.4.1375](https://doi.org/10.1152/jappl.1966.21.4.1375)
. PMID: 5916678.
