# MapBiomas robustness check

## Question

Do the PRODES results hold with an independent deforestation measure? MapBiomas maps land cover from Landsat each year with its own classifier, so it shares the satellite but not the method or the mapping team.

## Data

MapBiomas Collection 11, "Deforestation (native vegetation loss), annual, by municipality" (version of September 2026, xlsx, free):
`https://brasil.mapbiomas.org/wp-content/uploads/sites/3/2026/10/MAPBIOMAS-BRAZIL-DEFORESTATION-ANNUAL-MUNICIPALITY-COLLECTION_11.xlsx`
The link comes from the statistics page `https://brasil.mapbiomas.org/estatisticas/` (section on deforestation). Cached in `data/raw/mapbiomas/`; the Legal Amazon part since 2008 is saved as `data/derived/mapbiomas.csv` (km2 per municipality and calendar year, 2008 to 2025). All 772 panel municipalities are present. The 2026 map does not exist yet, so the check covers 2008 to 2025 and cannot test PRODES 2026.

Dead ends (so nobody repeats them): guessed Google Cloud Storage paths under `storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9|10/lulc/statistics/...` return 403 AccessDenied, and the bucket cannot be listed anonymously. The old Collection 9/10 file names are gone from the statistics page, which now only offers Collection 11. A plain `curl` with a browser user agent works for the brasil.mapbiomas.org file.

## Method

Three outcomes, all in km2 and transformed as log(km2 + 0.01), like the PRODES outcome:

- **MapBiomas primary forest loss** (main): loss of primary forest formation and floodable forest. Closest to what PRODES maps (clear-cutting of primary forest). Fixed before looking at results.
- Primary vegetation loss of any natural class (adds savanna, grassland, wetland).
- Primary and secondary native vegetation loss of any class.

Timing: PRODES year t runs from August of t-1 to July of t. It is paired with MapBiomas year t (about 7 months of overlap). A variant pairs it with year t-1.

Specifications are the headline ones, unchanged except for the outcome: the dose-response (`money_lag1`, municipality and state x year fixed effects, blacklist and crop price controls, clustered by municipality), and the restart event study (contracts since 2023 per municipality, year by year relative to 2022, with the same controls as `est_exposure`). PRODES is re-run on exactly the same municipality-years (2008 to 2025) so the comparison is fair. Code: `R/mapb.R`.

## Results

Level agreement is high where it should be: the correlation of PRODES and MapBiomas primary forest loss is 0.95 in km2 and 0.72 in logs, but only 0.32 once municipality and state x year effects are removed (that is the variation the regressions use). MapBiomas reports more clearing in total: 183,000 km2 of primary forest loss against 140,000 km2 in PRODES over 2008 to 2025 in the estimation sample.

**Dose-response** (coefficient per R$ 1 million of last year's money, log points; `results/mapb_dose_response.csv`):

| Outcome | Estimate | s.e. |
|---|---|---|
| PRODES, full headline sample 2008-2026 | 0.0496 | 0.0343 |
| PRODES, 2008-2025 | 0.0726 | 0.0326 |
| MapBiomas primary forest loss | 0.0354 | 0.0217 |
| MapBiomas primary vegetation loss, any class | 0.0442 | 0.0220 |
| MapBiomas all native vegetation loss | 0.0428 | 0.0196 |
| MapBiomas primary forest loss, asinh | 0.0414 | 0.0212 |
| MapBiomas forest loss, year t-1 | 0.0286 | 0.0215 |
| MapBiomas forest loss, two-year treatment lag | 0.0709 | 0.0249 |

The sign matches PRODES in every variant: more local money follows more deforestation, the pattern the README reads as money following deforestation. Magnitudes are roughly half of PRODES on the same years, except the two-year treatment lag (0.071, the same as PRODES). Three of the seven MapBiomas rows are significant at 5 % (any class, all native, two-year lag); the main primary forest row is not (p = 0.10).

**Restart** (all contracts since 2023, effect per R$ 1 million relative to 2022; `results/mapb_restart_event_study.csv`, figure `figures/mapb_restart_event_study.png`):

| Outcome | 2024 | 2025 |
|---|---|---|
| PRODES | -0.0159 (0.0095), -1.6 % | -0.0521 (0.0124), -5.1 % |
| MapBiomas primary forest loss | -0.0154 (0.0068), -1.5 % | -0.0185 (0.0081), -1.8 % |
| MapBiomas primary vegetation loss, any class | -0.0255 (0.0065), -2.5 % | -0.0267 (0.0086), -2.6 % |

The direction of the restart result replicates, but the MapBiomas event study does not have a clean pre-period: relative to 2022, the 2008 to 2018 coefficients for primary forest loss are all negative (about -0.04 to -0.07, t between -3 and -8) and jump to about zero in 2019 to 2021, while the 2023 to 2025 coefficients (-0.01 to -0.02) are small next to that gap. PRODES has no such pattern (pre-2022 coefficients all within about 2 s.e. of zero). So the MapBiomas restart coefficients are not cleanly identified: the parallel-trends assumption is rejected for that outcome, and the replication of the sign is weak support, not confirmation. (The freeze event study has no significant pre-period coefficients, `results/mapb_freeze_event_study.csv`.) The 2025 size is only about a third of PRODES for primary forest loss, so the headline "5 % less" is partly a PRODES feature. Using only contracts signed before August 2024, the effect is again absent in 2025 (PRODES +8 %, s.e. 10 %; MapBiomas +16 %, s.e. 8 %, p = 0.09), the same timing caveat as in the README. In that variant both measures also show a drop in 2023 (PRODES -14 %, p = 0.03; MapBiomas -17 %, p < 0.001) before most of the money was disbursed, which is consistent with a timing artifact rather than a Fund effect, though this check cannot rule out either.

## Caveats

- The two products do not measure the same thing: MapBiomas calendar years versus PRODES August to July, different classifiers, and MapBiomas counts loss in the Cerrado part of municipalities too. Differences can come from the measurement, so the result "holds in sign, shrinks in size" is the honest summary, not "confirmed".
- No 2026 data, so MapBiomas cannot confirm the 2026 restart coefficient.
- The 2025 MapBiomas map is the newest and will be revised in the next collection; recent years are the least stable in both datasets.
- The dose-response estimate in the 2008-2025 PRODES sample (0.073) differs from the headline (0.049) because 2026 is dropped, so compare MapBiomas to the row beside it, not to the headline.
- fixest drops the crop price control as collinear in the event-study regressions, as in the existing restart code.
