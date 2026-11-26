# PRODES cloud and unobserved area

Code: `R/cloud.R`. Results: `results/cloud_dose_response.csv`, `results/cloud_restart_event_study.csv`, `results/cloud_description.csv`. Cached data: `data/raw/prodes/prodes_residual_by_municipality.csv`.

## Question

PRODES cannot see forest under cloud. Municipalities in the wetter parts of the Amazon have more unobserved area, so measured deforestation there is noisier and partly shifted to later years. If cloudiness also correlates with Fund money, it could bias the headline results. The two reference papers control for cloud cover and unobserved area.

## Is cloud cover published per municipality? No (checked 5 Nov 2026, after the PRODES 2026 release of 16 Oct)

The README statement that PRODES does not publish cloud cover per municipality is still true. What I tried:

- **Dashboard JSON** (`terrabrasilis.dpi.inpe.br/app/prodes/dashboard/deforestation/files/data/prodes_legal_amazon.json`, the file `R/prodes.R` uses). All 19 periods contain one area type only (type 1, deforestation). Guessed files `..._nuvem.json` and `..._cloud.json` return 404.
- **TerraBrasilis GeoServer WFS** (`.../geoserver/ows?service=WFS`, GetCapabilities). The Legal Amazon workspace `prodes-legal-amz` has these data layers: `yearly_deforestation`, `accumulated_deforestation_2007`, `no_forest`, `residual`, plus boundaries (municipalities, states, indigenous lands, conservation units, hydrography). The biome workspace `prodes-amazon-nb` has the same set. No layer is named or described as cloud, nuvem or unobserved; requesting `cloud`, `nuvem`, `clouds`, `nuvens`, `unobserved`, `area_observada`, `cloud_cover` and `not_observed` in both workspaces returns HTTP 400 (unknown type).
- **Class values.** `yearly_deforestation` has only the classes `desmatamento` and `DESMATAMENTO`; `no_forest` has `nao_floresta`. (The server caps an answer at 50,000 features, so I looked at the first 50,000 rows of each.)
- **WMS/WCS.** The only raster layer in the Legal Amazon workspace is `temporal_mosaic_legal_amazon`, a Landsat image mosaic by year, not a cloud mask. Using it would mean downloading and classifying imagery, which is out of scope.
- **Bulk download paths** (`.../download/dataset/legal-amz-prodes/vector/` and `/raster/`) answer 403, and the `/downloads/` page is rendered by JavaScript, so I could not list files from it. I did not try the INPE FTP (`ftp.dpi.inpe.br`) or the geonetwork metadata. A per-scene cloud mask may exist there. If someone at INPE can send it, `cloud_panel()` takes any table of `cd_mun, year, residual_km2`-style values.

## What is available instead: the residual layer

`prodes-legal-amz:residual` has 58,941 polygons of forest that was cleared earlier but only seen later, because it was under cloud or otherwise unobserved when first imaged (field `def_cloud` gives the earlier year). It carries the year it was detected (2010 to 2025). Its area in a municipality is a one-sided proxy for earlier unobserved area: it measures the part of the cloud that later turned out to hide clearing, not cloud itself.

Construction: download the polygons (about 80 MB, 50 s), assign each to the IBGE municipality containing a point inside it, sum km² by municipality and year (4,593 municipality-years; total 5,419 km², against 143,403 km² of PRODES increments in the estimation sample). Controls and restrictions use the residual as a percentage of municipal area, so a municipality-year with no residual has 0. The year 2015 (1,619 km²) and 2025 (1,017 km²) are large because INPE processed backlogs of previously unobserved areas in those releases.

## Specifications

Same two headline specifications as the main analysis (sample, fixed effects and standard errors unchanged), plus:

1. control: residual share in the same year; in the previous year; municipality mean residual share interacted with year;
2. restriction: drop municipality-years above the 95th or 75th percentile of the positive residual shares (the 95th percentile is 0.080 % of municipal area); drop the 10 % of municipalities with the highest mean residual share.

## Results

Dose-response (log deforestation on last year's Fund money, R$ million):

| Specification | Estimate | s.e. | p | N |
|---|---|---|---|---|
| Baseline | 0.0487 | 0.0342 | 0.156 | 11,837 |
| + residual share, same year | 0.0488 | 0.0345 | 0.157 | 11,837 |
| + residual share, previous year | 0.0490 | 0.0342 | 0.153 | 11,837 |
| + mean residual share x year | 0.0491 | 0.0343 | 0.153 | 11,837 |
| Drop years above 95th pct | 0.0449 | 0.0349 | 0.198 | 11,608 |
| Drop years above 75th pct | 0.0666 | 0.0378 | 0.079 | 10,692 |
| Drop 10 % highest-residual municipalities | 0.0612 | 0.0372 | 0.100 | 10,716 |

Restart event study, money contracted since 2023 (coefficient on exposure x year, reference 2022; 2025 and 2026):

| Specification | 2025 | 2026 |
|---|---|---|
| Baseline | -0.0526 (0.0126) | -0.0668 (0.0147) |
| + residual share, same year | -0.0516 (0.0124) | -0.0668 (0.0147) |
| + mean residual x year | -0.0528 (0.0126) | -0.0671 (0.0147) |
| Drop years above 95th pct | -0.0523 (0.0127) | -0.0668 (0.0148) |
| Drop years above 75th pct | -0.0596 (0.0151) | -0.0665 (0.0147) |
| Drop 10 % highest-residual municipalities | -0.0554 (0.0139) | -0.0661 (0.0153) |

Counting only contracts signed before August 2024 the effect stays absent: baseline +0.076 (0.098) in 2025 and +0.084 (0.161) in 2026; with the residual control +0.079 and +0.086.

## Reading

Nothing moves. Adding the cloud proxy changes no coefficient by more than 0.001, and dropping the most cloud-affected observations changes the restart coefficients by at most 0.007 and the dose-response coefficient by at most 0.018 (all inside one standard error). Within a municipality and state-year, the residual share is only weakly related to deforestation (correlation 0.07).

## Caveats

- This is a proxy for earlier unobserved area, not the share of the municipality that was cloud-covered in a year. It is zero in 61 % of sample municipality-years, so it cannot rule out cloud effects in places where no clearing was later found under the cloud.
- Residual polygons are dated by when they were detected, which is later than the cloud that hid them. The lagged and municipality-mean versions try to catch persistent cloudiness, but cannot recover year-specific cloud.
- The proxy is a lower bound for the true unobserved area. A genuine cloud mask could behave differently, especially in the wet western and northern municipalities.
- I did not check whether the dashboard's municipal increments already include residual clearing in its detection year; if they do, part of the proxy mechanically enters the outcome.
- The residual layer ends in 2025, so 2026 is coded as zero residual for every municipality (the panel runs to 2026). The same-year control and the year restrictions therefore carry no information for 2026, the headline year; only the lagged and municipality-mean versions do. The 2026 restart coefficients barely moving is thus weak evidence.
- The same-year residual is detected in the same PRODES year as the outcome and may already be part of the dashboard increment, so it is not a clean pre-determined control; the municipality mean (2022 and earlier only, no look-ahead) is the cleanest version.
- Municipality assignment uses one interior point per polygon, so a polygon crossing a boundary goes wholly to one municipality.
- The robustness conclusion is therefore "no sign that cloud-related measurement matters", not a proof that it does not.
