# Calculating O2 Content of Blood

The oxygen content, or capacity, of blood is expressed as volume of
oxygen gas per volume of blood, typically milliliters of oxygen per 100
milliliters of blood. Oxygen carriage is primarily dependent on
haemoglobin, where haemoglobin molecules can bind with up to four oxygen
molecules. The fraction of available binding sites bound to oxygen
molecules is termed the haemoglobin oxygen saturation. Oxygen carriage
more precisely defined as being dependent upon haemoglobin concentration
in the blood and the saturation of that haemoglobin. Additionally, a
small amount of oxygen is carried in solution.

The total oxygen capacity of haemoglobin is described by Hüfner’s
constant, $`K_H`$, found experimentally to be a value of 1.306 ml/g
Gregory (1974)

$`K_s`$ is defined as the solubility coeffficient of oxygen in blood,
found experimentally to be 0.0225ml/dL/kPa

Formally:

``` math
\begin{equation}
  C_{O_2} = k_{H} \cdot S_{O_2} \cdot Hb  + k_s \cdot p_{O_2}
\end{equation}
```

where:

``` math
\begin{aligned}
k_H &=  1.301\; ml/g\\
k_s &=  0.0225\; ml/dL/kPa\\
\end{aligned}
```

## Implementation

### blood_oxygen_content_mls_dl

[`co2ntent::o2_content()`](https://bakenzua.github.io/co2ntent/reference/o2_content.md)
calculates $`C_{O_2}`$ from $`S_{O_2}`$, $`p_{O_2}`$ and $`Hb`$
parameters. Optionally an alternative value of Hüfner’s constant can be
provided, 1.36 being a popular value in the literature.

#### Usage

- `o2_content(po2, so2_fraction, haemoglobin_g_dl, hufners_constant, pressure_units="kPa")`

#### Arguments

- po2 - O2 partial pressure
- so2_fraction - Haemoglobin saturation as a fraction e.g 0 \<
  so2_fraction \< 1.0
- haemoglobin_g_dl - Haemoglobin g/dL. No default
- hufners_constant - Oxygen capacity of human haemoglobin. Default 1.306
  ml/g
- pressure_units - Input parameters are kPa, otherwise use mmHg

## Examples

### blood_oxygen_content_mls_dl

    > library(co2ntent)
    >
    > o2_content(po2=8, so2_fraction = 0.93, haemoglobin_g_dl = 15)
    [1] 18.3987
    > o2_content(po2=8, so2_fraction = 0.93, haemoglobin_g_dl = 15, hufners_constant = 1.36)
    [1] 19.152
    >

## References

Gregory, I. C. 1974. “The oxygen and carbon monoxide capacities of fetal
and adult blood.” *J. Physiol. (Lond.)* 236 (3): 625–34.
