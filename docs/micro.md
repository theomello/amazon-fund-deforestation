# Standard errors clustered by microregion

Code: `R/micro.R` (`micro_check()`). Results: `results/micro_clustering.csv` (fixest models) and `results/micro_staggered.csv` (staggered DiD). Test: `tests/testthat/test-micro.R`.

## What was checked and why

The headline specifications cluster by municipality (`cd_mun`). `docs/design.md` listed microregion clustering as skipped ("state x year is used instead"), and the planned robustness was two-way clustering by municipality and microregion x year. This check re-estimates the same models with the standard errors clustered at four levels, side by side:

1. municipality (what the pipeline reports), 623 clusters
2. state x year, 171 clusters (matches the state x year fixed effects)
3. IBGE microregion, 99 clusters in the sample
4. state, 9 clusters

Point estimates do not change, only the standard errors. Models: the dose-response (`money_lag1`), the restart event study (all contracts, and only contracts signed before August 2024), the freeze event study, Callaway and Sant'Anna (overall ATT) and Sun and Abraham (overall ATT).

## Data

Microregions come from IBGE's 2017 territorial division table (`DTB_2017.zip` on geoftp.ibge.gov.br, cached as `data/raw/ibge/DTB_2017.zip`, mapping saved to `data/derived/micro_microregions.csv`). IBGE retired microregions in 2017 in favour of "immediate regions", so 2017 is the last edition that has them. The localidades API named in the task did not answer from this machine (timeouts), hence the file download. All 623 sample municipalities are matched. Microregions nest within states, so they are a coarser spatial grouping than municipalities but finer than states.

## Results

Coefficients are log points per R$ 1 million (dose-response) or per R$ 1 million contracted (restart, freeze). p-values are analytic (cluster-robust t or normal). "Wild p" is the wild cluster restricted bootstrap p-value (999 draws; Webb six-point weights for the 9 states, Rademacher for microregions).

| Specification | Estimate | Municipality SE (p) | State x year SE (p) | Microregion SE (p) [wild p] | State SE (p) [wild p] |
|---|---|---|---|---|---|
| Dose-response, money lag 1 | +0.049 | 0.034 (0.16) | 0.042 (0.25) | 0.032 (0.14) [0.12] | 0.033 (0.17) [0.30] |
| Restart, all contracts, 2024 | -0.016 | 0.009 (0.09) | 0.026 (0.53) | 0.012 (0.17) [0.26] | 0.014 (0.28) [0.19] |
| Restart, all contracts, 2025 | -0.053 | 0.013 (<0.001) | 0.032 (0.10) | 0.013 (<0.001) [0.009] | 0.015 (0.007) [0.07] |
| Restart, all contracts, 2026 | -0.067 | 0.015 (<0.001) | 0.044 (0.13) | 0.016 (<0.001) [0.008] | 0.018 (0.006) [0.04] |
| Restart, contracts before Aug 2024, 2025 | +0.076 | 0.098 (0.44) | 0.119 (0.52) | 0.114 (0.50) [0.54] | 0.119 (0.54) [0.86] |
| Restart, contracts before Aug 2024, 2026 | +0.084 | 0.161 (0.60) | 0.084 (0.32) | 0.177 (0.64) [0.65] | 0.071 (0.27) [0.10] |
| Staggered DiD, overall ATT (C&S) | +0.352 | 0.103 (<0.001) | not available | 0.135 (0.009) | 0.184 (0.055) |
| Staggered DiD, overall ATT (Sun and Abraham) | +0.395 | 0.086 (<0.001) | 0.095 (<0.001) | 0.095 (<0.001) | 0.121 (0.011) |

Freeze exposure (PRODES 2019 to 2022 against 2018): no coefficient is significant under any clustering (smallest p is 0.10, 2019 with microregion clustering; wild p 0.10). Full numbers are in the CSV.

## Reading

- **Microregion clustering barely changes anything compared with municipality clustering.** SEs move by at most about 10 to 30 percent in either direction (the C&S ATT SE grows from 0.103 to 0.135; the restart 2025 and 2026 SEs are nearly identical). Spatial correlation across neighbouring municipalities is not the main threat to inference here. The dose-response stays insignificant, the restart effect for all contracts stays highly significant, the before-August-2024 restart and the freeze stay null under these two clusterings (state clustering is different, see below), and the first-project ATT stays positive and significant.
- **State x year clustering is the exception for the restart headline.** The SEs for 2025 and 2026 are 2.5 to 3 times larger (0.032 and 0.044) and the p-values become 0.10 and 0.13. This is the clustering that matches the level of the fixed effects, but it ignores serial correlation within a municipality over the three post years, so it is not a more conservative version of the same test in any principled way. It does show that the restart result leans on treating municipalities as independent draws within a state-year. Anyone who insists on state x year clustering should read the headline as suggestive rather than significant.
- **State clustering is borderline and not uniformly null elsewhere.** The before-August-2024 restart, null under every other clustering, gets a state wild p of 0.052 for 2024 and 0.10 for 2026 (the 2024 coefficient is -0.062, an unreliable estimate with a municipality SE of 0.097; 9 clusters make these small SEs fragile, so I would not read them as evidence).  With 9 clusters the analytic p-values (0.007 and 0.006) are unreliable; the wild bootstrap gives 0.07 for 2025 and 0.04 for 2026. The estimates survive at the 10 percent level but not comfortably.
- **First-project ATT**: significant at 1 percent with microregion clustering and borderline at state level (p = 0.055 analytic; Sun and Abraham p = 0.011).

## Caveats

- 9 state clusters is below any usual threshold. The wild bootstrap helps but with 9 clusters the Webb weights still give a coarse, somewhat conservative distribution. State results should be read as a floor on credibility, not a precise p-value.
- 99 microregions are enough for analytic inference, but cluster sizes are uneven (a few microregions hold dozens of municipalities, many hold a handful), so the wild p-values are the safer numbers.
- The wild bootstrap is my own implementation (the `fwildclusterboot` package is not installed): it imposes the null, re-demeans the reweighted residuals by the municipality and state x year effects on every draw, and uses the cluster-robust t-statistic. It is covered by a test that compares it with the analytic p-value on simulated data, but it is not validated against `fwildclusterboot`. Treat the third decimal as noise (999 draws).
- Callaway and Sant'Anna with a coarser cluster uses the multiplier bootstrap clustered at that level (a random draw, seed fixed at 2026). State x year is not possible there because the clustering variable has to be constant within a unit; that cell is empty on purpose. Two cohorts (2013, 2017) are very small, as already noted in the README.
- Microregions are the 2017 IBGE definition; municipalities created after 2017 would be missing, but none in the sample are.
- The headline pipeline clusters by municipality, not by state x year as `docs/design.md` line 248 says; that note is out of date, and this check supplies both.
