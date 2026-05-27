# co2ntent — Project Context

An R package for calculating blood CO2 and O2 content using canonical
physiological models. Three CO2 methods: Douglas (1988),
Siggaard-Andersen (1988), Loeppky (1983). One O2 method: Gregory (1974).
pO2→SO2 via Kelman (1966). Two built-in datasets from Loeppky (1983).

## Architecture: Facade Pattern

Three public facades dispatch to lower-level implementations by
method/units:

| Facade | File | Methods |
|----|----|----|
| [`co2_content()`](https://bakenzua.github.io/co2ntent/reference/co2_content.md) | `R/co2_content.R` | douglas, siggaard_andersen, loeppky |
| [`o2_content()`](https://bakenzua.github.io/co2ntent/reference/o2_content.md) | `R/o2_content.R` | (single impl) |
| [`po2_to_so2()`](https://bakenzua.github.io/co2ntent/reference/po2_to_so2.md) | `R/po2_to_so2.R` | kelman |

All facades use the unified `pressure_units` parameter (kPa/mmHg). They
pass it by the appropriate lower-level name (`pco2_units` for CO2
functions, `po2_units` for O2 functions) to the underlying
implementations. Both levels of functions are exported for direct use.

## Unit Parameter Naming Convention (critical — easy to get wrong)

| Gas  | Facade uses      | Lower-level functions use |
|------|------------------|---------------------------|
| CO2  | `pressure_units` | `pco2_units`              |
| O2   | `pressure_units` | `po2_units`               |
| Both | —                | `pressure_units`          |

[`kelman_po2_to_so2()`](https://bakenzua.github.io/co2ntent/reference/kelman_po2_to_so2.md)
and
[`kelman_virtual_po2()`](https://bakenzua.github.io/co2ntent/reference/kelman_virtual_po2.md)
take `pressure_units` (kPa/mmHg) since they handle both pO2 and pCO2.
[`kelman_std_po2_to_so2()`](https://bakenzua.github.io/co2ntent/reference/kelman_std_po2_to_so2.md)
takes `po2_units`.

## Exported Functions (15)

`actual_bicarbonate_content_mmol_l`, `blood_oxygen_content_mls_dl`,
`co2_content`, `douglas_co2_plasma_to_blood_ratio`, `kelman_po2_to_so2`,
`kelman_std_po2_to_so2`, `kelman_virtual_po2`, `kpa_to_mmhg`,
`mls_dl_to_mmols_l`, `mmhg_to_kpa`, `mmols_l_to_mls_dl`, `o2_content`,
`po2_to_so2`.

## Build & Test

``` bash
R CMD build .                                    # build the package tarball
R CMD check co2ntent_*.tar.gz                    # full check (includes vignettes)
Rscript -e 'devtools::test()'                    # run all tests
Rscript -e 'devtools::document()'                # regenerate NAMESPACE and Rd files
```

## Vignettes

Source: `vignettes/*.Rmd`. Built output: `doc/*.html`. pkgdown site:
`docs/`. CI deploys pkgdown to `gh-pages` on push to `main`/`master`.

Files: - `vignettes/co2_content.Rmd` — CO2 content calculations and
Bland-Altman plots - `vignettes/o2-content.Rmd` — O2 content theory and
examples - `vignettes/po2-to-so2.Rmd` — O2 dissociation curve and Kelman
method

## Common Pitfalls

1.  **Vignettes break `R CMD build`**: Even a single brace mismatch
    blocks the entire build. Test with `R CMD build .` after vignette
    changes.
2.  **Error messages must name the correct function**: Every
    [`stop()`](https://rdrr.io/r/base/stop.html) call should include the
    calling function name as a prefix (e.g.,
    `"siggaard_andersen_blood_co2_content_mmol_l: ..."`). Copy-pasting
    messages between functions causes misleading errors.
3.  **Parameter naming triage**: When adding a new method to a facade,
    remember to wire it in all content_units combinations (ml/dL and
    mmol/L).
4.  **NAMESPACE is auto-generated**: Edit roxygen2 `@export` tags in R
    source, then run `devtools::document()`. Do not edit NAMESPACE by
    hand.
5.  **Unit conversion**: Lower-level functions work in native units;
    facades handle unit conversion via
    [`mmols_l_to_mls_dl()`](https://bakenzua.github.io/co2ntent/reference/mmols_l_to_mls_dl.md)
    /
    [`mls_dl_to_mmols_l()`](https://bakenzua.github.io/co2ntent/reference/mmols_l_to_mls_dl.md).
    Default gas order is `c("ideal", "co2", "o2")`.

## Git

- `develop` is the active branch. `master` tracks releases.
- `gh-pages` is auto-deployed by CI.
