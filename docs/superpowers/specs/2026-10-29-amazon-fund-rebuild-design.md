# Amazon Fund and Deforestation — rebuild design (phases 1 and 2)

Date: 2026-10-29
Status: draft for approval

## 1. Intent

This repository holds a seminar paper written in the second semester of an LMU Munich economics bachelor (summer 2021): *The Amazon Fund and Deforestation*. The owner wants anyone who opens the repo to come away thinking it is **a genuinely interesting topic, and an ambitious one for an undergraduate to have taken on**.

The repo is now to become the best current version of the project. The 2021 state is preserved in git history and in the owner's local folder, so nothing in the working tree has to stay as it was.

Work happens in three phases. This document covers phases 1 and 2.

1. **Clean code and new data.** Everything is rebuilt by script from public sources, through PRODES 2026.
2. **Results and interpretation.** A credible design built around money disbursed per municipality per year, with the 2019 freeze and 2023 restart as a test of whether that money matters.
3. *(Later, out of scope here.)* Possibly a new paper reporting the results.

## 2. Goals, non-goals, anti-goals

**Goals**
- `git clone` → open `.Rproj` → run one entry script → all data downloaded, panel built, results and figures produced. The same works with `Rscript` from the repo root.
- Every number traces back to a public source and a script. No hand-built spreadsheets. The only exceptions are two small, version-controlled, cited manual files: coverage corrections (§4.3) and the priority-municipality list transcribed from MMA decrees (§4.5).
- The main result reads in plain terms, for example "R$ 1 million more Fund money in a year is followed by X % less deforestation", with uncertainty.
- The code is short and readable: a handful of focused scripts instead of one 1,800-line exploratory file.

**Non-goals**
- Writing the new paper (phase 3).
- Replicating the 2021 numbers exactly. The 2021 code is kept in git history (tagged).
- Causal claims beyond what the design supports.
- Interactive dashboards or web apps.

**Anti-goals** (failure even if everything runs)
- Hiding that this started as an undergraduate project, or presenting it as something it isn't.
- A treatment variable whose construction a reader can't follow or audit.
- Overstated interpretation. In particular, the freeze/restart coincides with national changes in enforcement policy (Bolsonaro 2019–2022, Lula 2023–), and that must be stated wherever the freeze is discussed.
- Many specifications with no clear main result ("garden of forking paths"). There is one main specification, and everything else is labelled as robustness or heterogeneity.

## 3. Constraints

- **Language: R** (≥ 4.5), as in the original project. The repo already runs in the owner's RStudio via `amazon-fund-deforestation.Rproj`. *(The web scraper was prototyped in Python during exploration; it will be ported to R so the repo stays single-language. Builder's call if a port is impractical.)*
- **Public data only**, downloaded by script. Raw downloads are cached in `data/raw/` with the download date recorded, so reruns don't re-scrape. A `refresh` flag forces a new download.
- **Polite scraping:** at most 1 request per second against fundoamazonia.gov.br (robots.txt allows `/pt/projeto/`). Cache every page.
- **Public repo:** no personal data. Raw data that is very large (> 50 MB) is not committed; it's re-downloadable by script.
- Must run on macOS. LaTeX is only needed if figures are exported as TikZ. The default figure output is PDF/PNG via ggplot2.

## 4. Data

All sources below were checked during exploration on 2026-10-29 unless marked *(to verify)*.

### 4.1 Deforestation (outcome)

- **PRODES Legal Amazon, municipal annual increments, 2008–2026**, from the JSON files behind the TerraBrasilis dashboard:
  - data: `https://terrabrasilis.dpi.inpe.br/app/prodes/dashboard/deforestation/files/data/prodes_legal_amazon.json`
  - names: `.../files/config/loinames/prodes_legal_amazon.json`
  - municipal polygons: `.../files/config/mun/prodes_legal_amazon.json`
  - Last updated 2026-10-16. Legal Amazon totals match the 2021 hand export within INPE's routine revisions (e.g. 2020: 10,495 vs 10,496 km²).
  - PRODES year *t* = 1 Aug *t−1* to 31 Jul *t*.
  - Municipalities are identified by **name only**. Link them to IBGE 7-digit codes by name + state, with a polygon overlay (`sf`) as a fallback for ambiguous names.
  - The `1500–2007` period is accumulated deforestation, not an annual increment. Use it only as the baseline stock.
- **PRODES cloud cover and unobserved area per municipality-year** *(to verify)*. Both reference papers control for these. If they aren't available for recent years, document the gap and drop the control rather than impute it.
- **MapBiomas municipal deforestation** *(optional robustness, to verify the Collection 10 download)*.

### 4.2 Amazon Fund (treatment)

- **Project pages**: all 156 pages at `https://www.fundoamazonia.gov.br/pt/projeto/<slug>/`. The list of slugs comes from paginating `/pt/projetos-apoiados/index.html?reloaded&page=N`. Each page gives:
  - value of the project and of Fund support
  - type of executor (*Natureza do Responsável*: União / Estados / Municípios / Terceiro Setor / Universidades / Internacionais)
  - themes and axes (*eixos*)
  - state(s)
  - approval, contract and completion dates
  - **free-text territorial coverage** (*Abrangência territorial*)
  - **every disbursement with its date and amount**

  Validation already done: the parsed disbursements sum to R$ 2,926 million, exactly the Fund's published total. That gives 644 disbursements across 156 projects.
- **BNDES open data, non-automatic operations CSV** (`dadosabertos.bndes.gov.br`, filter `fonte_de_recurso_desembolsos == "RECURSOS VINCULADOS - FUNDO AMAZÔNIA"`): 163 contracts with contract dates and contracted/disbursed values. Used as a **cross-check** of the scraped project list. Its municipality field is almost always "SEM MUNICÍPIO", so it is not used for location.
- **Cancelled projects** (separate listing on the Fund site): scrape and keep them flagged, but exclude them from treatment.

### 4.3 Project → municipality mapping

Coverage types found in the first pass over all 156 projects (share of Fund support):

| Type | Projects | Share | Treatment handling |
|---|---|---|---|
| Named municipalities | 61 | 23 % | Match names to IBGE codes within the stated state(s) |
| Whole state(s) | 52 | 29 % | **Not** allocated in the main spec; absorbed by state × year FE. Allocated in a robustness check (§6.4) |
| Biome-wide / federal (e.g. IBAMA enforcement) | 32 | 46 % | Not allocated; national/annual variation is absorbed by year FE. Part of the freeze/restart narrative |
| Indigenous lands / protected areas only | 8 | 2 % | Map to municipalities by overlaying FUNAI indigenous-land and protected-area polygons with municipal polygons; weight by overlap area |
| Outside the Amazon / unclear | 3 | 1 % | Excluded (e.g. CAR Bahia, CAR Espírito Santo) |

- **Exploration artefacts available to the builder:** the cached raw pages (`data/raw/fundo_amazonia/`, 156 project pages + listing), the PRODES JSON (`data/raw/prodes/`), the BNDES CSV (`data/raw/bndes/`), and the Python prototype with its outputs in `docs/exploration/` (`parse_fa.py`, `match_munis.py`, `projects_classified.csv`, `disbursements.csv`, `project_municipalities.csv`). The R port must reproduce R$ 2,926 million / 644 disbursements and the coverage table above, up to the manual fixes.
- **Classification of all 156 projects (coverage type) is done by script rules,** with the same overrides file able to change a project's coverage type (column `action = set_type`). So every project's classification is auditable in one place, including the biome-wide and state-level ones.
- Automatic matching works but has known errors: names that are also rivers or regions ("Juruá"), reserve names that collide with municipality names ("Cujubim"), and typos on the site ("Senador José Profírio"). It also misses "N municipalities in state X" phrasing with no names. A **manual review** of the ~70 municipality/indigenous-land projects produces `data/manual/coverage_overrides.csv` (slug, action add/remove, IBGE code, note). This is the one hand-maintained input, and it's reviewed in a pull request so it's auditable.
- Report coverage diagnostics in the README: number of municipalities ever treated and money by coverage type.

### 4.4 Allocation of a project's money across its municipalities

The owner left this as a technical choice. The main specification uses an **equal split** across covered municipalities. Robustness checks split by municipal area and by remaining forest area at baseline (PRODES accumulated stock). **Never allocate by past deforestation**, which would mechanically correlate treatment with the outcome.

Disbursements are assigned to PRODES years by disbursement date (Aug–Jul) and deflated to 2025 reais with the IPCA (IBGE).

### 4.5 Controls and mechanism variables

| Variable | Source | Role |
|---|---|---|
| Priority-municipality blacklist (*municípios prioritários*), by year | MMA decrees/portarias 2008 onward (short lists, ~80 municipalities in total; hand-transcribed into `data/manual/priority_municipalities.csv` with decree references) | **Control** (key confounder: Fund programs target these municipalities) |
| PRODES cloud / unobserved area | INPE *(to verify)* | Control (measurement) |
| Precipitation and temperature, annual by municipality | Gridded public data (e.g. TerraClimate or CHIRPS + ERA5) aggregated over municipal polygons; builder chooses the product | Control |
| Crop and cattle price indices | Following Assunção & Rocha: national/state prices weighted by each municipality's baseline production shares (IBGE PAM tab. 1612 and PPM for herds) | Control |
| Protected-area and indigenous-land share of municipal area | FUNAI / MMA (CNUC) polygons | Time-invariant: absorbed by municipality FE. Used in baseline × trend robustness |
| IBAMA environmental fines per municipality-year | IBAMA open data (*autos de infração*) | **Mechanism outcome**, not a control |
| Municipal area, state, microregion, IBGE code | IBGE (Legal Amazon municipality list 2020 is already in the owner's files; the builder should fetch the current list from IBGE) | Keys and clustering |

Indigenous land, agriculture and commodity prices come from current IBGE/FUNAI downloads, not from the 2021 spreadsheets (owner's decision).

## 5. Sample

- Unit: municipality × PRODES year.
- Years: 2008–2026 for the outcome. Treatment starts in 2010, so 2008–2009 are pre-period.
- Municipalities: Legal Amazon municipalities that have PRODES coverage and show variation in deforestation, dropping those with negligible forest at baseline (following Assunção et al. 2023). The builder documents the exact threshold and how many municipalities each rule drops.

## 6. Empirical design (phase 2)

### 6.1 Main specification: dose–response

$$ y_{it} = \beta \, \text{Money}_{i,t-1} + X_{it}'\gamma + \alpha_i + \delta_{s(i)t} + \varepsilon_{it} $$

- $y_{it}$: $\ln(\text{deforestation}_{it} + 0.01)$, as in Assunção, Gandour & Rocha (2023).
- $\text{Money}_{i,t-1}$: real Fund money disbursed to municipality *i* in the previous PRODES year, from municipality-specific projects (named municipalities and indigenous lands), in R$ millions. The builder checks the distribution and uses `asinh` if it's very skewed. The one-year lag follows both reference papers.
- $\alpha_i$: municipality fixed effects.
- $\delta_{s(i)t}$: **state × year fixed effects**. These absorb all state-wide shocks, including state-level Fund projects and state enforcement.
- $X_{it}$: blacklist status, cloud/unobserved area, weather, price indices.
- Standard errors clustered by municipality. Robustness: two-way clustering by municipality and microregion × year.
- Estimate with `fixest`.

### 6.2 Staggered DiD (binary, comparable to 2021)

- Treatment: the first year a municipality is covered by a municipality-specific project (contract date, or first disbursement as a variant).
- Estimator: **Callaway & Sant'Anna** (`did` package), with not-yet-treated units as controls, plus an event-study plot. Sun & Abraham (`fixest::sunab`) is a cross-check.
- This replaces the 2021 two-way FE staggered DiD, which is biased when effects vary across cohorts. The README says so explicitly, as a point of growth from the original.

### 6.3 Freeze and restart

Disbursements of existing contracts **continued** through 2019–2022 (about R$ 90–130 million per year). The freeze stopped *new* contracts, not payments. The freeze design therefore measures the **money a municipality lost relative to what it would have received**, not the stock of money still owed.

- **Freeze exposure (designer's call):** the shortfall $E_i^{F} = \bar{m}_i^{\,2016\text{–}18} - \bar{m}_i^{\,2019\text{–}22}$. This is a municipality's average annual municipality-specific disbursements before the freeze, minus its average during the freeze, both from the scraped dated disbursements. It's large where the Fund mattered before 2019 *and* the money dried up during the freeze. As a variant, use pre-freeze intensity $\bar{m}_i^{\,2016\text{–}18}$ alone, since realised freeze-period flows partly reflect project choices.
- **Restart exposure:** real municipality-specific money contracted from 2023 on (contracted, not disbursed, so slow disbursement doesn't count as low exposure).
- **Projects counted:** only municipality-specific projects (named municipalities and indigenous lands, §4.3), as in the main specification. State-level projects are absorbed by the fixed effects.
- **Estimating equation:** an event study $y_{it} = \sum_{k \neq 2018} \theta_k \,(E_i \times 1[t=k]) + X_{it}'\gamma + \alpha_i + \delta_{s(i)t} + \varepsilon_{it}$, with **state × year FE** as in §6.1. The reference year is 2018 for the freeze and 2022 for the restart. Expected sign: $\theta_k > 0$ for freeze years if lost Fund money raised deforestation.
- **Sample:** all municipalities in the main sample (zero exposure for never-treated ones). Robustness: restrict to municipalities ever covered by a municipality-specific project, so the comparison is purely between more- and less-exposed treated places.

The interpretation must state that national enforcement policy changed at the same moments. The state × year FE absorb the national and state level, so identification comes only from *differences in exposure within a state*. The remaining threat is that exposed municipalities respond differently to national enforcement changes. That is addressed with baseline deforestation × year controls and blacklist × year controls.

### 6.4 Robustness and heterogeneity

Robustness (each as one line in a single table or coefficient plot):
- outcome as normalized deforestation (municipality mean/SD over 2008–2012) and as share of remaining forest
- state-level project money allocated to municipalities by forest area, added to the treatment
- allocation by area and by forest area instead of equal split
- contemporaneous and 2-year lags
- baseline accumulated deforestation × linear trend
- MapBiomas outcome *(if available)*
- dropping each state in turn

Heterogeneity and mechanism:
- by project axis (*eixos*: monitoring and control, sustainable production, land-tenure regularization, science)
- local money × the state having an active Fund-backed state-agency project
- IBAMA fines as an outcome (mechanism: does Fund money raise local enforcement?)

### 6.5 Interpretation outputs

- The main coefficient converted into "% change in deforestation per R$ 1 million", with a back-of-envelope implied km² for the average treated municipality.
- One main figure: the event study for the staggered DiD.
- One figure: Fund disbursements per year (coloured by coverage type) against Legal Amazon deforestation, 2008–2026, annotated with the freeze and restart.
- A short "what this can and cannot say" paragraph in the README.

## 7. Code and repository structure

The builder may adjust this, but the intent is a small set of focused scripts run by a single entry point:

```
amazon-fund-deforestation.Rproj
run_all.R                  # sources the steps in order; refresh flag for downloads
R/
  01_download_prodes.R
  02_scrape_fund.R         # list + project pages + BNDES cross-check, cached
  03_map_coverage.R        # coverage classification, name matching, TI overlay, overrides
  04_download_controls.R   # IBGE, FUNAI, IBAMA, weather, prices, blacklist
  05_build_panel.R         # municipality × year panel → data/derived/panel.parquet (or .csv)
  06_estimate.R            # main spec, DiD, freeze/restart, robustness
  07_figures_tables.R
data/
  raw/                     # cached downloads (large files gitignored)
  manual/                  # coverage_overrides.csv, priority_municipalities.csv
  derived/                 # panel and treatment tables
output/figures, output/tables
archive/2021/              # the original 2021 report PDF, for the story (owner's history)
docs/
```

- Target: well under 1,000 lines of R in total, with no dead code. Comments explain *why*, not *what*.
- Package management: `renv` lockfile *(builder's call; recommended)*.
- The 2021 version: tag the current `main` as `v2021-seminar` before restructuring. Keep the 2021 PDF in `archive/2021/` *(my call, so the README can tell the "then and now" story)*.
- README rewritten around: the question, the data pipeline, the main result, what changed since 2021 and why.

## 8. Decisions made by the owner

- The repo becomes the best current version; 2021 survives in history.
- Order: clean code + new data → results and interpretation → maybe a paper.
- Main analysis: money per municipality per year; the freeze and restart as a test of whether that money matters.
- Keep searching for project municipality locations (done: project pages give them).
- Splitting money across municipalities is a technical choice, with alternatives tested.
- State-level projects: absorbed by state × year FE in the main spec, used for robustness and heterogeneity.
- Controls from current IBGE/FUNAI downloads, not the 2021 spreadsheets.
- **Manual files:** the builder does the manual coverage review (~70 projects) and transcribes the priority-municipality decrees, with a source and note per row. The owner spot-checks a sample; results count as final only after that spot-check.
- **Git:** tag the current `main` as `v2021-seminar` and push the tag. Do the rebuild on a branch (`rebuild-2026`), pushed to GitHub. Merge to `main` only after the owner has seen the results.

## 9. Calls made by the designer (flag if wrong)

- Outcome $\ln(y+0.01)$ as main; one-year lag; state × year FE; municipality clustering.
- Equal split as main allocation.
- Blacklist as control, fines as mechanism outcome.
- Callaway–Sant'Anna for the binary staggered DiD.
- Freeze exposure = pre-freeze minus freeze-period municipality-specific money (shortfall), with pre-freeze intensity as a variant; restart exposure = money contracted from 2023; both with state × year FE, full sample, ever-treated-only as robustness (§6.3).
- R-only repo, with the Python prototype ported.
- 2021 PDF moved to `archive/2021/`.
- Raw caches under 50 MB (Fund pages, PRODES JSON) are committed so the build is reproducible even if the sites change; the BNDES CSV (19 MB) is also committed. Anything larger is gitignored and re-downloaded.

## 10. Left to the builder

- Exact weather product and price index construction details.
- Low-forest exclusion threshold (documented).
- File formats for derived data.
- Whether to use `targets` instead of numbered scripts.
- Table/plot styling.

## 11. Known risks

- PRODES cloud/unobserved data for recent years may not be published per municipality.
- Blacklist decrees must be transcribed by hand (small but must be cited).
- The Fund site may change structure; cached raw pages make the build reproducible regardless.
- Treatment covers only ~23 % of Fund money (municipality-specific projects). The README must say that the estimate concerns *local* projects, not the Fund as a whole.
