# The Amazon Fund and Deforestation

**Did Amazon Fund money reduce deforestation in the municipalities where it was spent?**

The [Amazon Fund](https://www.amazonfund.gov.br/en/home/) (Fundo Amazônia) is one of the largest results-based forest finance programmes. It has disbursed about R$ 2.9 billion since 2010, mostly donated by Norway and Germany and managed by Brazil's development bank BNDES. It was frozen in 2019, when the federal government dissolved its governance committees, and restarted in 2023. That makes the Fund a rare policy with both a long track record and two sharp, nationally imposed breaks.

This started as my seminar paper in the second semester of my economics bachelor at LMU Munich (summer 2021). In 2026 I rebuilt it from scratch, with new data through 2026 and a design that uses the freeze and the restart.

## Main finding

![Restart event study](figures/04_restart_event_study.png)

**After the 2023 restart, deforestation fell faster in municipalities that contracted more new Fund money.** Each R$ 1 million contracted from 2023 on is associated with about **2 % less deforestation in 2024, 5 % in 2025 and 6 % in 2026**, relative to 2022. All three estimates are statistically significant. The comparison is within the same state and year, so it holds fixed everything that changed for the whole Amazon or a whole state at the same time.

**Read this as suggestive, not as proof.** The restart coincided with the return of federal environmental enforcement under the new government, and new projects partly went to the municipalities where deforestation had peaked in 2019–2022. Some coefficients before 2018 are negative too, which means exposed municipalities were on a rising path before the restart. Part of the decline could therefore be a return to normal levels or enforcement targeted at the same places. Two checks are next (see *Next steps*).

### The rest of the picture

| Question | Design | Result |
|---|---|---|
| Does more money in a year mean less deforestation the next year? | Fixed effects, municipality + state × year | No. +5 % per R$ 1 million (p = 0.15); none of 21 robustness variants is significantly negative ([figure](figures/07_dose_response_robustness.png)) |
| What happens after a municipality gets its first local project? | Staggered DiD (Callaway & Sant'Anna) | Deforestation keeps rising relative to not-yet-treated municipalities (+0.35 log points on average; [figure](figures/05_first_project_event_study.png)). This most likely reflects *targeting*: projects go to active deforestation frontiers |
| Did deforestation rise where Fund money dried up in the 2019 freeze? | Event study on each municipality's money shortfall | No detectable rise ([figure](figures/06_freeze_event_study.png)). Payments on existing contracts continued through 2019–2022; only new contracts stopped |
| Does local Fund money bring more enforcement? | IBAMA flora fines as outcome | No effect |

Taken together: local Fund money was **not** allocated at random. It followed deforestation, which is why naive comparisons (including my own in 2021) find a positive association. The restart provides the cleanest variation so far, and it points in the hoped-for direction.

## Where the money went

![Fund disbursements per year](figures/01_fund_disbursements.png)

![Deforestation in the Legal Amazon](figures/02_deforestation.png)

Only about **23 % of Fund money** went to projects that name the municipalities or indigenous lands where they work. The estimates above concern that local money. State programmes (30 %) and biome-wide or federal programmes (46 %, e.g. IBAMA enforcement and satellite monitoring) can't be tied to municipalities. They're absorbed by the state × year fixed effects.

![Map of local Fund money](figures/03_map_money.png)

## Then and now

| | 2021 seminar paper | 2026 rebuild |
|---|---|---|
| Data | Spreadsheets copied by hand from the Fund's website, PRODES export 2008–2020 | Everything downloaded by script: all 156 project pages with 644 dated disbursements (they sum exactly to the Fund's published R$ 2,926 million), PRODES through 2026 |
| Treatment | "Has a project": binary, no intensity | R$ per municipality per year (2025 prices), plus freeze and restart exposure |
| Method | Two-way fixed effects (`plm`), plain staggered DiD | `fixest` with state × year fixed effects, Callaway–Sant'Anna for staggered adoption, event studies around the freeze and restart |
| Code | One 1,800-line exploratory script | Small tested functions (86 unit tests), one entry point, about 40 seconds end to end |

The 2021 version is preserved under the tag [`v2021-seminar`](../../tree/v2021-seminar) and as a PDF in [`archive/2021/`](archive/2021/The_Amazon_Fund_2021.pdf).

## Data

| Source | What | How |
|---|---|---|
| [INPE PRODES / TerraBrasilis](https://terrabrasilis.dpi.inpe.br) | Annual deforestation per municipality, 2008–2026; indigenous-land polygons | JSON files behind the official dashboard (updated 2026-10-16) |
| [Fundo Amazônia](https://www.fundoamazonia.gov.br/pt/projetos-apoiados/) | 156 projects: values, dates, territorial coverage, every disbursement | Scraped project pages (cached in `data/raw/`) |
| [BNDES open data](https://dadosabertos.bndes.gov.br/dataset/operacoes-financiamento) | The Fund's 163 contracts | Cross-check of the scraped list |
| [Seixas (2023), Zenodo](https://doi.org/10.5281/zenodo.8412144) | Priority-municipality blacklist 2008–2022, with the MMA portaria per entry | Download |
| [IBAMA open data](https://dadosabertos.ibama.gov.br/dataset/fiscalizacao-auto-de-infracao) | Environmental infractions (flora) per municipality | Download (aggregated; the raw file is not committed) |
| IBGE (SIDRA tables 1612, 3939; `geobr`) | Crop areas and prices, cattle herds, municipal boundaries | API |
| BCB, FRED | IPCA inflation, USD/BRL, global beef price | API |

Which municipalities a project covers is matched automatically from the project's text. A small, cited corrections file ([`data/manual/coverage_overrides.csv`](data/manual/coverage_overrides.csv)) fixes the rest; the review is documented in [`docs/manual_review.md`](docs/manual_review.md).

## Running it

Requires R ≥ 4.5 with `tidyverse`, `fixest`, `did`, `sf`, `geobr`, `httr2`, `rvest`, `jsonlite` and `testthat`.

- **RStudio:** open `amazon-fund-deforestation.Rproj` and source `run_all.R`.
- **Terminal:** `Rscript run_all.R` from the repo root.

Raw downloads are cached in `data/raw/`, so reruns work offline; set `REFRESH <- TRUE` in `run_all.R` to fetch fresh data. Run the tests with `Rscript tests/testthat.R`.

| Folder | Contents |
|---|---|
| `R/` | One file per step: `prodes.R`, `fund_scrape.R`, `fund_coverage.R`, `priority.R`, `controls.R`, `panel.R`, `estimate.R`, `report.R` |
| `data/raw/`, `data/manual/` | Cached downloads and the two hand-maintained inputs |
| `figures/`, `results/` | Figures and coefficient tables produced by `run_all.R` |
| `docs/` | Design, implementation plan and manual-review notes |

## What this can and cannot say

- **It covers local projects only** (about 23 % of the money). The effect of the Fund as a whole, including federal enforcement it financed, is absorbed by the year and state × year effects and can't be estimated this way.
- **Missing controls.** There are no weather controls: free sources would have needed far more downloading than the state × year effects justify. PRODES doesn't publish cloud cover per municipality. The cattle price index starts in 2010, so it enters only as a robustness check.
- **The blacklist after 2022 is carried forward** from the last list with a free, machine-readable source. The 2023 and 2024 lists (Portarias MMA 834/2023 and 1.202/2024) are what the restart result most needs to control for.
- Two staggered-adoption cohorts (2013, 2017) are small.
- **Provisional:** the manual coverage review awaits a spot-check.

## Next steps

1. Add the official 2023/2024 priority-municipality lists and re-estimate the restart, the check most likely to change the headline.
2. Deeper identification for the restart: matching on pre-2019 trends or synthetic control.
3. Possibly a new paper reporting the results.
