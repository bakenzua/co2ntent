# co2ntent — Project Context

An R package for calculating blood CO2 and O2 content using canonical
physiological models. Three CO2 methods: Douglas (1988), Siggaard-Andersen
(1988), Loeppky (1983). One O2 method: Gregory (1974). pO2→SO2 via Kelman
(1966). Two built-in datasets from Loeppky (1983).

## Architecture: Facade Pattern

Three public facades dispatch to lower-level implementations by method/units:

| Facade             | File            | Methods                        |
|--------------------|-----------------|--------------------------------|
| `co2_content()`    | `R/co2_content.R` | douglas, siggaard_andersen, loeppky |
| `o2_content()`     | `R/o2_content.R`  | (single impl)                  |
| `po2_to_so2()`     | `R/po2_to_so2.R`  | kelman                         |

Facades accept `phase`, `method`, `content_units`, `pco2_units`/`po2_units`/`pressure_units`
and pass through via `...` to the underlying function. Underlying functions
are also exported for direct use.

## Unit Parameter Naming Convention (critical — easy to get wrong)

| Gas  | Facade uses      | Kelman/PO2 functions use |
|------|------------------|--------------------------|
| CO2  | `pco2_units`     | —                        |
| O2   | `po2_units`      | —                        |
| Both | —                | `pressure_units`         |

`kelman_po2_to_so2()` and `kelman_virtual_po2()` take `pressure_units` (kPa/mmHg),
NOT `po2_units`. `kelman_std_po2_to_so2()` takes `po2_units`.

## Exported Functions (15)

`actual_bicarbonate_content_mmol_l`, `blood_oxygen_content_mls_dl`,
`co2_content`, `douglas_co2_plasma_to_blood_ratio`, `kelman_po2_to_so2`,
`kelman_std_po2_to_so2`, `kelman_virtual_po2`, `kpa_to_mmhg`,
`mls_dl_to_mmols_l`, `mmhg_to_kpa`, `mmols_l_to_mls_dl`, `o2_content`,
`po2_to_so2`.

## Build & Test

```bash
R CMD build .                                    # build the package tarball
R CMD check co2ntent_*.tar.gz                    # full check (includes vignettes)
Rscript -e 'devtools::test()'                    # run all tests
Rscript -e 'devtools::document()'                # regenerate NAMESPACE and Rd files
```

## Vignettes

Source: `vignettes/*.Rmd`. Built output: `doc/*.html`. pkgdown site: `docs/`.
CI deploys pkgdown to `gh-pages` on push to `main`/`master`.

Files:
- `vignettes/co2_content.Rmd` — CO2 content calculations and Bland-Altman plots
- `vignettes/o2-content.Rmd` — O2 content theory and examples
- `vignettes/po2-to-so2.Rmd` — O2 dissociation curve and Kelman method

## Common Pitfalls

1. **Vignettes break `R CMD build`**: Even a single brace mismatch blocks the
   entire build. Test with `R CMD build .` after vignette changes.
2. **Error messages must name the correct function**: Every `stop()` call
   should include the calling function name as a prefix
   (e.g., `"siggaard_andersen_blood_co2_content_mmol_l: ..."`). Copy-pasting
   messages between functions causes misleading errors.
3. **Parameter naming triage**: When adding a new method to a facade, remember
   to wire it in all content_units combinations (ml/dL and mmol/L).
4. **NAMESPACE is auto-generated**: Edit roxygen2 `@export` tags in R source,
   then run `devtools::document()`. Do not edit NAMESPACE by hand.
5. **Unit conversion**: Lower-level functions work in native units; facades
   handle unit conversion via `mmols_l_to_mls_dl()` / `mls_dl_to_mmols_l()`.
   Default gas order is `c("ideal", "co2", "o2")`.

## Git

- `develop` is the active branch. `master` tracks releases.
- `gh-pages` is auto-deployed by CI.
