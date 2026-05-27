# Calculate the O2 content of blood

Calculates the oxygen content of blood in mls per 100ml.

## Usage

``` r
blood_oxygen_content_mls_dl(
  po2,
  so2_fraction,
  haemoglobin_g_dl,
  hufners_constant = 1.306,
  po2_units = c("kPa", "mmHg")
)
```

## Arguments

- po2:

  O2 partial pressure

- so2_fraction:

  Haemoglobin saturation as a fraction e.g 0 \< so2_fraction \< 1.0

- haemoglobin_g_dl:

  Haemoglobin g/dL. No default

- hufners_constant:

  Oxygen capacity of human haemoglobin. Default 1.306 ml/g

- po2_units:

  Unit for `po2`; one of `"kPa"` or `"mmHg"`. Default is "kPa".

## Value

The O2 content of blood in ml/dL

## Details

The majority of oxygen in blood is bound to haemoglobin, which is
dependent upon haemoglobin oxygen saturation and haemoglobin
concentration. The total oxygen capacity of haemoglobin is described by
Hüfner’s constant, which is not "constant" but variable depending on
author/literature source or c. This function uses a value of 1.306 ml/g
as per (Gregory 1974) . (Gorelov "2008") delimits this variability.

## References

Gorelov V ("2008"). “Theoretical value of Hüfner's constant.”
*Anaesthesia*, **59**(1), 97-97.
[doi:10.1111/j.1365-2044.2004.03598.x](https://doi.org/10.1111/j.1365-2044.2004.03598.x)
.
<https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1365-2044.2004.03598.x>.
Gregory IC (1974). “The oxygen and carbon monoxide capacities of fetal
and adult blood.” *J. Physiol. (Lond.)*, **236**(3), 625–634. Lumb AB,
Lumb AB (2010). *Nunn's Applied Respiratory Physiology*, 7th ed edition.
Elsevier. Book, Whole,
<http://www.clinicalkey.com/dura/browse/bookChapter/3-s2.0-C2009055355X>.
