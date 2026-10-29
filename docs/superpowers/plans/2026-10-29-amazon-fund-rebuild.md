# Amazon Fund Rebuild Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the 2021 exploratory script with a short, scripted R pipeline. It downloads PRODES deforestation (2008–2026) and every Amazon Fund disbursement, maps projects to municipalities, builds a municipality × year panel, and estimates the dose–response, staggered-DiD and freeze/restart designs.

**Architecture:** Pure functions live in `R/` (one file per responsibility) and are unit-tested with `testthat` against small fixtures. `run_all.R` sources the functions and runs the steps in order: download (cached in `data/raw/`) → parse → map coverage → controls → panel (`data/derived/`) → estimates → figures/tables (`output/`). Two hand-maintained, cited CSVs live in `data/manual/`.

**Tech Stack:** R ≥ 4.5; tidyverse (dplyr, tidyr, readr, stringr, purrr), rvest, httr2, jsonlite, sf, geobr, fixest, did, testthat.

**Spec:** `docs/superpowers/specs/2026-10-29-amazon-fund-rebuild-design.md` (read it before any task; section numbers below refer to it).

## Global Constraints

- Branch `rebuild-2026`; tag `v2021-seminar` already exists. Never push to `main`. Merge only after the owner has seen the results.
- R only. The Python files in `docs/exploration/` are references to port, not code to ship.
- Scraping fundoamazonia.gov.br: at most 1 request per second; every page is cached. `refresh = FALSE` (the default) never touches the network when a cached file exists.
- PRODES year *t* = 1 Aug *t−1* to 31 Jul *t*.
- Money in 2025 reais, deflated with the IPCA.
- Never allocate money by past deforestation (§4.4).
- Main specification (§6.1): `log(defor_km2 + 0.01) ~ money_lag1 + controls | cd_mun + uf^year`, clustered by `cd_mun`.
- Total R code (excluding tests) well under 1,000 lines; no dead code; comments explain why.
- Owner preference: if something looks wrong or a source turns out to be hard to get (more than a quick fix), **stop and ask the owner** before investing effort. Document the decision in the spec's §11.
- Commit messages end with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.

## Review Focus

1. **Municipality name collisions and spelling variants** (same name in two states, e.g. "Presidente Médici" in MA and RO; apostrophes like "Pau D'Arco"; accents). Every PRODES municipality and every matched project municipality must resolve to exactly one IBGE code. Tests in Tasks 2 and 4.
2. **Negative disbursements** (refunds such as `-R$ 267,95`) must be kept as negative amounts and summed. Test in Task 3.
3. **Projects with no disbursement table** (approved or contracted but not yet paid) and cancelled projects must parse without error and contribute zero money. Tests in Tasks 3 and 6.
4. **Offline rerun:** with caches present and `refresh = FALSE`, the whole pipeline runs without network. Test in Task 1 (`cache_download`).
5. **Encodings:** the BNDES CSV is Windows-1252, and the Fund pages and PRODES names are UTF-8 with accents. Parsed names keep their accents, and normalized keys strip them. Tests in Tasks 1 and 3.

---

### Task 1: Scaffolding, archive the 2021 version, shared helpers

**Files:**
- Move: `report/The_Amazon_Fund.pdf` → `archive/2021/The_Amazon_Fund_2021.pdf`
- Delete: `code/`, `data/*.csv|xlsx|xls|dbf` (top level of `data/` only; keep `data/raw/`), `figures/`, `report/` (all of these are preserved in tag `v2021-seminar`)
- Create: `run_all.R`, `R/utils.R`, `tests/testthat.R`, `tests/testthat/test-utils.R`
- Modify: `.gitignore` (add `data/derived/`, `output/`, `.Rproj.user/`, `data/raw/**/*.zip` and any raw file > 50 MB)

**Interfaces:**
- Produces (in `R/utils.R`):
  - `cache_download(url, path, refresh = FALSE, sleep = 0) -> path`. Downloads only if the file is missing or `refresh`; creates parent directories; sleeps `sleep` seconds after a real download; errors on a non-200 status.
  - `prodes_year(date) -> integer`. Returns `year(date) + (month(date) >= 8)`.
  - `parse_brl(x) -> numeric`. Parses "R$ 1.800.104,08", "-R$ 267,95" and "R$ -267,95"; returns NA for anything else.
  - `norm_key(x) -> character`. Uppercase, accents stripped, apostrophes and hyphens become spaces, whitespace squashed.
  - `deflate_to_2025(value, year, ipca) -> numeric`, where `ipca` is a tibble with columns `year` and `index` (annual average).
- `run_all.R`: `source()` every file in `R/`, sets `REFRESH <- FALSE`, then calls the step functions defined in later tasks in order. Each later task appends its call(s).

- [ ] **Step 1:** Install the missing packages: `Rscript -e 'install.packages(c("testthat","fixest","did"), repos="https://cloud.r-project.org")'`. Expected: all three load.
- [ ] **Step 2: Write the failing tests** in `tests/testthat/test-utils.R`:

```r
test_that("prodes_year follows Aug-Jul", {
  expect_equal(prodes_year(as.Date(c("2019-07-31", "2019-08-01", "2020-06-26"))), c(2019L, 2020L, 2020L))
})
test_that("parse_brl handles thousands, decimals and negatives", {
  expect_equal(parse_brl(c("R$ 1.800.104,08", "-R$ 267,95", "R$ -267,95", "n/a")), c(1800104.08, -267.95, -267.95, NA))
})
test_that("norm_key strips accents and punctuation", {
  expect_equal(norm_key(c("São Félix do Xingu", "Pau D'Arco", " Xapurí ")), c("SAO FELIX DO XINGU", "PAU D ARCO", "XAPURI"))
})
test_that("cache_download does not hit the network when cached", {
  f <- tempfile(); writeLines("x", f)
  expect_equal(cache_download("http://invalid.invalid/x", f), f)
})
test_that("deflate_to_2025 scales by index ratio", {
  ipca <- tibble::tibble(year = c(2020L, 2025L), index = c(100, 150))
  expect_equal(deflate_to_2025(10, 2020L, ipca), 15)
})
```

- [ ] **Step 3:** Run `Rscript -e 'source("tests/testthat.R")'` (it sources `R/*.R`, then `testthat::test_dir("tests/testthat")`). Expected: FAIL, functions not found.
- [ ] **Step 4:** Implement the five helpers in `R/utils.R`, plus `tests/testthat.R` and the `run_all.R` skeleton. Do the moves and deletions listed under Files.
- [ ] **Step 5:** Run the tests. Expected: PASS.
- [ ] **Step 6:** Commit: `git add -A && git commit -m "Restructure repo for 2026 rebuild; add shared helpers"`. Leave `data/raw/` out of this commit; it's committed in Task 3.

### Task 2: PRODES municipal deforestation linked to IBGE codes

**Files:**
- Create: `R/prodes.R`, `tests/testthat/test-prodes.R`, `tests/testthat/fixtures/prodes_small.json`. The fixture is a hand-cut subset of `data/raw/prodes/prodes_legal_amazon_data.json`: two periods (2019–2020, 2007–2008) and three municipalities, including one name duplicated across states.

**Interfaces:**
- Consumes: `cache_download`, `norm_key`.
- Produces:
  - `download_prodes(refresh = FALSE) -> list(data = path, names = path, mun = path)`. Uses the three URLs in spec §4.1 and saves to `data/raw/prodes/` under the existing filenames.
  - `read_prodes(paths) -> tibble(gid int, prodes_name chr, year int, defor_km2 dbl)`. Sums `areas[].area` over features with `loi == 2` (municipality), keyed by `loiname` gid; `year` = `endDate.year`. The period whose start year is 1500 becomes `year = 2007L` and must be returned in a **separate** tibble.
  - `read_prodes_stock(paths) -> tibble(gid, stock_2007_km2)`.
  - `link_prodes_ibge(prodes_geo_path, ibge_mun) -> tibble(gid, cd_mun chr(7))`. `ibge_mun` is `geobr::read_municipality(year = 2022)` filtered to Legal Amazon states (AC AM AP MA MT PA RO RR TO). Match on `norm_key(name)`; names that are ambiguous or missing get matched by joining the PRODES polygon's `st_point_on_surface` into the IBGE polygons.
  - `build_deforestation(refresh = FALSE) -> tibble(cd_mun, year, defor_km2)`, plus `stock_2007_km2` as a separate tibble. Writes `data/derived/deforestation.csv` and `data/derived/stock_2007.csv`.

- [ ] **Step 1: Write failing tests:**
  - `read_prodes` on the fixture returns one row per gid × year with the fixture's hand-summed areas, and no row for the 1500 period.
  - `link_prodes_ibge` on the real cached files gives every gid exactly one `cd_mun`, with no duplicated `cd_mun` (`expect_false(anyDuplicated(...))`).
  - Integration (skip if the cache is missing): the 2020 Legal Amazon total from `build_deforestation()` is within ±1 km² of 10,496, and 2010 within ±1 of 6,279.
- [ ] **Step 2:** Run. Expected: FAIL.
- [ ] **Step 3:** Implement the functions above.
- [ ] **Step 4:** Run. Expected: PASS. Print the number of linked municipalities (expect about 770–805 gids → about 770 codes). If more than 5 gids can't be linked, stop and report.
- [ ] **Step 5:** Append `build_deforestation(REFRESH)` to `run_all.R`. Commit.

### Task 3: Scrape and parse Amazon Fund projects and disbursements

**Files:**
- Create: `R/fund_scrape.R`, `tests/testthat/test-fund.R`, `tests/testthat/fixtures/APL-Babacu.html`, `tests/testthat/fixtures/Sementes-do-Portal.html` (copied from `data/raw/fundo_amazonia/pages/`)
- Commit: `data/raw/fundo_amazonia/`, `data/raw/prodes/`, `data/raw/bndes/`, `data/raw/DOWNLOADED.txt`

**Interfaces:**
- Consumes: `cache_download`, `parse_brl`.
- Produces:
  - `scrape_fund(refresh = FALSE) -> chr` (paths of project pages). Paginates `https://www.fundoamazonia.gov.br/pt/projetos-apoiados/index.html?reloaded&page=N` until a page has no `/pt/projeto/` links. Also scrapes `/pt/projetos-apoiados/projetos-cancelados` and its pages into `data/raw/fundo_amazonia/cancelled/`. Uses `sleep = 1`.
  - `parse_project(path) -> list(project = 1-row tibble, disbursements = tibble(slug, n int, date Date, value_brl dbl))`. Project columns: `slug, valor_total, valor_apoio, natureza, temas, local, eixos, status, date_approval, date_contract, date_completion, abrangencia, cancelled lgl`. Port the label-following logic of `docs/exploration/parse_fa.py`: text lines after "Valor Total do Projeto", "Valor do apoio do Fundo Amazônia", "Natureza do Responsável", "Temas", "Local", "Eixos", "Data da aprovação", "Data da contratação", "Data da conclusão"; *Abrangência territorial* runs until the next heading ("Descrição", "Beneficiários" or "Objetivos"); disbursement rows are `^\d+º desembolso$` followed by a date line and a value line.
  - `build_fund(refresh = FALSE) -> list(projects, disbursements)`. Writes `data/derived/fund_projects.csv` and `data/derived/fund_disbursements.csv`, and logs a cross-check against `read_bndes_fund()`.
  - `read_bndes_fund(path = "data/raw/bndes/operacoes-nao-automaticas.csv") -> tibble`. Read with `locale(encoding = "windows-1252")` and `delim = ";"`; filter `fonte_de_recurso_desembolsos == "RECURSOS VINCULADOS - FUNDO AMAZÔNIA"`.

- [ ] **Step 1: Write failing tests:**
  - `APL-Babacu`: `valor_apoio == 4897085.37`; `date_contract == as.Date("2014-10-29")`; `abrangencia == "Estado do Maranhão, nos municípios de Lago do Junco, Lago da Pedra e Bacabal"`; 4 disbursements, with the last one `value_brl == -267.95` on `2020-06-26`.
  - `Sementes-do-Portal`: 7 disbursements summing to 5397778.87 (tolerance 0.01); `date_contract == as.Date("2010-03-25")`; abrangencia contains "Apiacás" (accent kept).
  - A synthetic page (string written to a tempfile) with no "Desembolsos" section returns a 0-row disbursements tibble and no error.
  - Integration over the cached pages: 156 non-cancelled projects, 644 disbursements, total `sum(value_brl)` within ±0.5 million of 2,926 million.
  - `read_bndes_fund()` returns 163 rows and the client names contain "FUNDAÇÃO" or "FUNDACAO" correctly decoded (no `�`).
- [ ] **Step 2:** Run. Expected: FAIL.
- [ ] **Step 3:** Implement.
- [ ] **Step 4:** Run. Expected: PASS. If the cancelled-projects listing isn't scrapeable within a quick try, record it and continue; cancelled projects only add zero money.
- [ ] **Step 5:** Append `build_fund(REFRESH)` to `run_all.R`. Commit the code and the raw caches.

### Task 4: Coverage classification and municipality matching

**Files:**
- Create: `R/fund_coverage.R`, `tests/testthat/test-coverage.R`, `data/manual/coverage_overrides.csv` (header only for now: `slug,action,cd_mun,coverage_type,note,source_url`)

**Interfaces:**
- Consumes: `norm_key`, `fund_projects` (Task 3), `ibge_mun` (Task 2), FUNAI indigenous-land polygons.
- Produces:
  - `classify_coverage(projects, ibge_mun) -> projects + coverage_type`. One of `"municipalities"`, `"indigenous_lands"`, `"state"`, `"biome_national"`, `"outside"`, with the same rules as `docs/exploration/match_munis.py`. State names, river and region names ("Juruá", "Amapá", "Tocantins", "Acre", "Pará", ...) never count as municipality matches. Projects whose state is outside the Legal Amazon (e.g. Bahia, Espírito Santo) are `"outside"`.
  - `match_municipalities(projects, ibge_mun) -> tibble(slug, cd_mun, method = "name")`. Word-boundary match of `norm_key` names, restricted to the project's states; a shorter name is dropped when it's contained in a longer matched name (Aripuanã vs Novo Aripuanã).
  - `download_funai_tis(refresh = FALSE) -> path`. The FUNAI indigenous-land polygons (`tis_poligonais`) from FUNAI's geoserver or downloads page. **Verify the URL; if no stable download exists, stop and ask the owner.**
  - `match_indigenous_lands(projects, tis, ibge_mun) -> tibble(slug, cd_mun, method = "ti_overlay", weight)`. TI names from `abrangencia` are matched to `terrai_nom` by `norm_key`; `weight` = the TI's area share in each municipality.
  - `apply_overrides(links, projects, overrides) -> list(links, projects)`. Applies actions `add`, `remove` and `set_type`.
  - `build_coverage() -> list(projects, links)`. Writes `data/derived/fund_coverage.csv` (slug, coverage_type) and `data/derived/fund_links.csv` (slug, cd_mun, method, weight).

- [ ] **Step 1: Write failing tests** (with a small `ibge_mun` tibble built in the test):
  - "Estado do Maranhão, nos municípios de Lago do Junco, Lago da Pedra e Bacabal" → exactly those 3 codes, `coverage_type == "municipalities"`.
  - "Região do Alto Juruá, no estado do Acre" → 0 municipality links, `coverage_type == "state"`.
  - "Estado da Bahia" → `"outside"`.
  - "Amazônia Legal" → `"biome_national"`.
  - Text naming "Novo Aripuanã" matches only Novo Aripuanã, not Aripuanã.
  - Duplicate name across states: "Presidente Médici" with project state RO links only the RO code.
  - `apply_overrides` with one `remove` and one `add` row changes the link set accordingly.
  - Integration: the share of Fund support by coverage type over all projects matches spec §4.3 within ±3 percentage points before overrides.
- [ ] **Step 2:** Run. Expected: FAIL.
- [ ] **Step 3:** Implement.
- [ ] **Step 4:** Run. Expected: PASS.
- [ ] **Step 5:** Append `build_coverage()` to `run_all.R`. Commit.

### Task 5: Manual review: coverage overrides and priority municipalities (owner spot-check gate)

**Files:**
- Modify: `data/manual/coverage_overrides.csv`
- Create: `data/manual/priority_municipalities.csv` (`cd_mun, name, uf, year_in, year_out, decree, source_url`), `docs/manual_review.md` (what was checked, decisions, open doubts)
- Create: `tests/testthat/test-manual.R`

**Interfaces:**
- Produces: `read_priority(path = "data/manual/priority_municipalities.csv", years = 2008:2026) -> tibble(cd_mun, year, priority = 0/1)`, in `R/fund_coverage.R` or a new `R/manual.R`.

- [ ] **Step 1: Write failing tests:**
  - Every `cd_mun` in both manual files exists in `ibge_mun`.
  - Every row has a non-empty `source_url`.
  - `read_priority` gives the 36 municipalities of MMA Portaria 28/2008 `priority == 1` in 2008.
  - No `coverage_overrides` slug is missing from `fund_projects`.
- [ ] **Step 2:** Review every project with `coverage_type` in `municipalities` or `indigenous_lands`, plus all projects whose text contains "municípios" without names. For each, compare `abrangencia` and the project page with the automatic links. Write add/remove/set_type rows with a short note. Time-box this: if a project's coverage can't be pinned down from its page in a few minutes, set it to `state` with a note rather than guessing.
- [ ] **Step 3:** Transcribe the priority lists (MMA Portaria 28/2008 and the later portarias adding and removing municipalities, up to the most recent list). If the decrees for some years can't be found quickly, stop and ask the owner.
- [ ] **Step 4:** Run the tests. Expected: PASS. Rerun `build_coverage()` and record the new coverage table in `docs/manual_review.md`.
- [ ] **Step 5:** Commit. **Gate:** tell the owner the files are ready for their spot-check (link `docs/manual_review.md`). Tasks 6–8 may proceed, but results stay labelled provisional until the owner confirms.

### Task 6: Controls and the municipality × year panel with treatment variables

**Files:**
- Create: `R/controls.R`, `R/panel.R`, `tests/testthat/test-panel.R`

**Interfaces:**
- Consumes: outputs of Tasks 2–5.
- Produces (`R/controls.R`; each writes `data/derived/<name>.csv` and caches raw data):
  - `get_ipca(refresh) -> tibble(year, index)`: BCB SGS series 433 (monthly IPCA %) via `https://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados?formato=csv`, chained into an annual-average index.
  - `get_weather(ibge_mun, refresh) -> tibble(cd_mun, year, precip_mm, temp_c)`: Open-Meteo historical API (`archive-api.open-meteo.com/v1/archive`, daily `precipitation_sum`, `temperature_2m_mean`, 2007-08-01 to the latest complete PRODES year) at each municipality's `st_point_on_surface`, aggregated by PRODES year. One cached JSON per municipality, `sleep = 0.2`.
  - `get_fines(refresh) -> tibble(cd_mun, year, fines_flora)`: IBAMA open data, *autos de infração*. Count infractions of type flora by municipality code and PRODES year. Verify the dataset URL; if it isn't a direct download, stop and ask.
  - `get_prices(refresh) -> tibble(cd_mun, year, crop_price_idx, cattle_price_idx)`: crop index = national implicit prices (production value / quantity, IBGE SIDRA table 1612, via `https://apisidra.ibge.gov.br/values/t/1612/...`) for soy, maize, rice, cassava, beans and sugarcane, deflated, weighted by the municipality's 2005–2007 harvested-area shares from the same table. Cattle index: a deflated national fat-cattle price series. If no scriptable cattle price source is found quickly, stop and ask the owner.
  - `get_mapbiomas(refresh)` (optional, spec §4.1): municipal annual deforestation from MapBiomas, used only as a robustness outcome. If there's no direct download for the latest collection, skip it and note this in spec §11.
  - `get_clouds(refresh)`: **verify** whether PRODES publishes cloud/unobserved area per municipality-year. If not, return `NULL` and add a note to spec §11; do not impute.
- Produces (`R/panel.R`):
  - `allocate_money(disbursements, links, coverage, ibge_mun, stock, method = c("equal", "area", "forest")) -> tibble(cd_mun, year, money_brl2025)`. Only `coverage_type %in% c("municipalities", "indigenous_lands")`. Indigenous-land projects use their overlay `weight` within the project, then the chosen method applies across municipalities. Money goes to `prodes_year(date)`.
  - `allocate_state_money(disbursements, coverage, ibge_mun, stock) -> tibble(cd_mun, year, state_money_brl2025)`: state-level projects split across that state's municipalities by remaining forest (robustness, §6.4).
  - `build_panel() -> tibble`, written to `data/derived/panel.csv`. One row per `cd_mun × year` (2008–latest), with columns `uf, microregion, defor_km2, y = log(defor_km2 + 0.01), money (R$ mi, equal split), money_area, money_forest, money_state, money_lag1, first_treat_year, freeze_exposure, freeze_intensity, restart_exposure, priority, precip_mm, temp_c, crop_price_idx, cattle_price_idx, fines_flora, stock_2007_km2, area_km2`, plus the sample flag `in_sample`.
  - Exposure definitions (spec §6.3): `freeze_intensity` = mean annual money 2016–2018; `freeze_exposure` = that minus the mean annual money 2019–2022; `restart_exposure` = real money **contracted** (project `valor_apoio` × allocation share, by `date_contract`) from 2023 on. All are time-invariant per municipality. `first_treat_year` = the PRODES year of the first municipality-specific contract (NA if never treated).
  - Sample rule (§5): drop municipalities with `stock_2007_km2 + total forest` negligible. Concretely, drop those whose mean `defor_km2` over 2008–2012 is 0 **or** whose forest at baseline (municipal area minus PRODES non-forest minus accumulated stock) is < 2 % of area. Log the counts dropped.

- [ ] **Step 1: Write failing tests:**
  - **Conservation:** for `method = "equal"`, `sum(allocate_money(...)$money_brl2025)` equals the deflated sum of disbursements of municipality-specific projects (tolerance 1 real). The same holds for `"area"` and `"forest"`.
  - A toy project covering 2 municipalities with one disbursement of 100 on `2019-08-15` gives 50 + 50 in year 2020.
  - A project with no disbursements contributes 0 and no rows.
  - `build_panel()` has unique `(cd_mun, year)` keys, no NA in `cd_mun`, `year` or `y`, and `money_lag1` for year *t* equals `money` for year *t−1*.
  - `freeze_exposure == freeze_intensity - mean(money 2019:2022)` on a toy panel.
- [ ] **Step 2:** Run. Expected: FAIL.
- [ ] **Step 3:** Implement the controls, then the panel.
- [ ] **Step 4:** Run. Expected: PASS. Print panel dimensions, sample counts, number of treated municipalities and the money totals by coverage type.
- [ ] **Step 5:** Append `build_panel()` (which calls the controls) to `run_all.R`. Commit.

### Task 7: Estimation

**Files:**
- Create: `R/estimate.R`, `tests/testthat/test-estimate.R`

**Interfaces:**
- Consumes: `panel` from Task 6.
- Produces (each returns a model object or a tidy tibble, and saves an `.rds` in `output/models/`):
  - `est_main(panel) -> fixest`: `feols(y ~ money_lag1 + priority + precip_mm + temp_c + crop_price_idx + cattle_price_idx [+ clouds] | cd_mun + uf^year, cluster = ~cd_mun, data = in_sample rows)`.
  - `est_staggered(panel) -> list(cs = did::att_gt object, es = aggte(type = "dynamic"), sunab = fixest)`. CS uses `control_group = "notyettreated"`, `gname = first_treat_year` (0 for never-treated), `idname` = integer municipality id, the same controls as baseline covariates, and clustering by municipality.
  - `est_exposure(panel, exposure = c("freeze_exposure", "freeze_intensity", "restart_exposure"), ref_year) -> fixest`: `feols(y ~ i(year, exposure, ref = ref_year) + controls + priority:i(year) + stock_2007_km2:i(year) | cd_mun + uf^year, cluster = ~cd_mun)`. `ref_year` = 2018 for freeze and 2022 for restart. The variant `ever_treated_only = TRUE` restricts to `!is.na(first_treat_year)`.
  - `est_robustness(panel) -> tibble(spec, estimate, std.error, p.value, n)`. One row per item in spec §6.4: normalized outcome; share of remaining forest; plus state money; area allocation; forest allocation; lag 0; lag 2; baseline-stock × trend; two-way clustering `~cd_mun + microregion^year`; drop each state.
  - `est_mechanism(panel) -> fixest`: the main spec with `log(fines_flora + 1)` as outcome.
  - `est_heterogeneity(panel) -> tibble`: money split by project axis (*eixos*), and money × the state having an active state-agency Fund project that year.
  - `pct_effect(beta) -> numeric` = `100 * (exp(beta) - 1)`, the % change per R$ 1 million.

- [ ] **Step 1: Write failing tests:**
  - Simulated panel (200 municipalities × 15 years, two states, true β = −0.05 on `money_lag1`, municipality and state-year effects, noise SD 0.3): `coef(est_main(sim))["money_lag1"]` lies within ±0.015 of −0.05.
  - Simulated staggered adoption with a true effect of −0.2 from treatment onward: the CS overall ATT lies within ±0.08 of −0.2.
  - `pct_effect(log(0.9))` is −10.
- [ ] **Step 2:** Run. Expected: FAIL.
- [ ] **Step 3:** Implement.
- [ ] **Step 4:** Run. Expected: PASS. Then run all estimators on the real panel and print the main coefficient, its SE and N. **Checkpoint:** if the main estimate or an event study looks implausible (pre-trends clearly non-zero, sign flips across nearly identical specs), stop and report to the owner before tuning anything.
- [ ] **Step 5:** Append the estimation calls to `run_all.R`. Commit.

### Task 8: Figures, tables, README and final run

**Files:**
- Create: `R/report.R`
- Modify: `README.md` (full rewrite), `docs/superpowers/specs/2026-10-29-amazon-fund-rebuild-design.md` (§11 notes only)
- Delete: `docs/exploration/` once the R port reproduces its numbers (the files stay in git history)

**Interfaces:**
- Consumes: the models from Task 7 and the panel.
- Produces (`R/report.R`), all into `output/`:
  - `fig_overview(panel, fund)`: disbursements per year stacked by coverage type, with Legal Amazon deforestation 2008–2026 on a second panel, and vertical lines at the 2019 freeze and the 2023 restart.
  - `fig_event_study(staggered)`: the CS dynamic effects with 95 % CIs.
  - `fig_exposure(models)`: freeze and restart event studies.
  - `tab_main(models)`: main spec, CS ATT, mechanism (Markdown and LaTeX via `fixest::etable` / `modelsummary` if installed).
  - `fig_robustness(rob)`: a coefficient plot of all robustness rows.

- [ ] **Step 1:** Implement the report functions and append them to `run_all.R`.
- [ ] **Step 2:** Full run from a clean `data/derived/` and `output/` with `Rscript run_all.R`. Expected: exit 0, all figures and tables present. Run the full test suite. Expected: PASS.
- [ ] **Step 3:** Rewrite the README:
  - the question and why it's interesting
  - "then and now" (2021 seminar → 2026 rebuild, link to tag `v2021-seminar` and `archive/2021/`)
  - data sources with download dates
  - how to run (RStudio `.Rproj` / `Rscript run_all.R`)
  - the main result in plain language using `pct_effect`
  - the freeze/restart figure with the enforcement-policy caveat
  - "what this can and cannot say", including that the estimate covers municipality-specific projects (~23 % of Fund money)
  - "provisional until manual-review spot-check" if the owner hasn't confirmed yet
- [ ] **Step 4:** Check the line count with `wc -l R/*.R run_all.R`. Expected: well under 1,000.
- [ ] **Step 5:** Commit and push the branch (`git push`). Do **not** merge; report the results to the owner with links to the figures.
