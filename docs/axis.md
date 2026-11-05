# Heterogeneity by project axis

## Question
Does the Fund's money lower deforestation only when it is spent on a particular kind of project? The previous objectives analysis (docs and `results/objectives.csv`) put the four axes side by side in the dose-response. This check adds the restart design by axis, each axis on its own, and a test of whether the axes differ.

## Classification rule
Every Fund project carries exactly one axis (*eixo*) on its project page, so we use the Fund's own label and do not invent a rule. Mapping to column suffixes (`OBJECTIVES` in `R/panel.R`):

| Axis (Fund label) | Suffix |
|---|---|
| Produção sustentável | production |
| Monitoramento e controle | monitoring |
| Ordenamento territorial | territorial |
| Ciência, inovação e instrumentos econômicos | science |

Only municipality-specific projects (named municipalities and indigenous lands) reach the treatment variables, as in the headline. Cancelled projects are excluded. Axis money adds up to total local money (tested).

## Counts (`results/axis_counts.csv`)
| Axis | Projects (all / local) | Support, R$ m (all / local) | Disbursed in sample, R$ m | Municipalities with money | Municipalities with restart contracts | Restart R$ m (summed over municipalities) |
|---|---|---|---|---|---|---|
| production | 43 / 21 | 1,265 / 430 | 149 | 137 | 90 | 6,469 |
| monitoring | 48 / 13 | 2,615 / 333 | 237 | 91 | 74 | 4,883 |
| territorial | 31 / 12 | 1,078 / 228 | 128 | 60 | 70 | 2,347 |
| science | 34 / 14 | 1,011 / 371 | 176 | 70 | 56 | 4,772 |

Restart R$ is the exposure summed over municipalities, so it counts a project's money once per place at the equal split. The largest axis in total money (monitoring, mostly biome-wide or state projects that do not enter the local treatment) is only 13 local projects.

No axis falls under the 30-municipality threshold in the table, but that count hides the real problem. For the restart, only 3 to 11 contracts since 2023 sit behind each axis (production 11, science 5, territorial 4, monitoring 3), and exposure is nearly the same number in every place that a project covers. So a restart axis estimate is really a handful of projects, not a dose. Treat them as thin:
- **Territorial restart is one project plus one outlier.** 56 municipalities have exactly R$ 0.58 million (one regularisation programme, 85 municipalities) and one municipality has R$ 54.5 million (a single-municipality health project). The coefficient on this axis and its tiny standard error (0.003) come from that one high-leverage point. Without it the standard error is 0.063 and the estimate is not distinguishable from zero.
- **Monitoring restart** rests on 3 contracts, mostly one project at about R$ 2.99 million per municipality.

## Method
Outcome, fixed effects (municipality and state x year), controls and clustering are as in the headline (`est_main`, `est_exposure`).
- **Dose-response:** last year's money of one axis alone, then all four jointly (horse race), R$ million.
- **Restart:** exposure is real R$ million contracted since 2023 (no cutoff), split by axis; the four axes add up exactly to `restart_exposure_all` (tested). Event study as in `est_exposure` (reference 2022), plus one pooled number: exposure x 1[year after 2022].
- Equality of axes is tested with a Wald test in the joint models.

## Results
Coefficients are log points per R$ 1 million (standard error in brackets). 0.05 is about 5%.

Dose-response (`results/axis_effects.csv`; 11,837 municipality-years):

| | All local money | Production | Monitoring | Territorial | Science |
|---|---|---|---|---|---|
| Headline, current panel | 0.049 (0.034) | | | | |
| One axis at a time | | -0.046 (0.161) | -0.004 (0.047) | 0.103 (0.057) | 0.124 (0.090) |
| Horse race | | -0.043 (0.159) | -0.002 (0.048) | 0.103 (0.057) | 0.122 (0.088) |

No axis reduces deforestation. Territorial and science point to more deforestation after money arrives, the territorial one at p = 0.07, and that pattern is consistent with money going where deforestation is rising (the objectives analysis found rising pre-trends in these groups). Equal effects across axes are not rejected (p = 0.27). The headline in `results/main_and_mechanism.csv` shows 0.0485; the small gap to 0.0487 is probably the current `panel.csv` having been rebuilt since that table was written; not checked.

Restart, pooled 2023 to 2026 (`results/axis_effects.csv`):

| | All | Production | Monitoring | Territorial | Science |
|---|---|---|---|---|---|
| One axis at a time | -0.019 (0.013) | -0.033 (0.023) | 0.075 (0.039) | -0.040 (0.003)* | 0.013 (0.026) |
| Horse race | | -0.041 (0.022) | 0.082 (0.039) | -0.040 (0.003)* | 0.016 (0.026) |
| Territorial without the top municipality, alone / race | | | | -0.047 (0.063) / -0.060 (0.062) | |

\* Not reliable, see above. The Wald test of equal restart effects (p < 1e-40) is driven by this same point and should be ignored.

Restart event study, one axis at a time (`results/axis_restart_event_study.csv`, `figures/axis_restart_event_study.png`), 2025 and 2026 coefficients against the all-axes headline (-0.053 and -0.067, both p < 0.001):
- production: -0.051 (0.034) and -0.140 (0.043)
- science: -0.094 (0.047) and -0.062 (0.049)
- territorial: -0.042 and -0.058 (SEs of 0.005 to 0.008, unreliable)
- monitoring: -0.071 (0.051) and +0.005 (0.058), with large negative coefficients in 2023 and 2024 (-0.097, -0.106) that come before the money could act and look like pre-trends or coincident enforcement

The pooled 2023 to 2026 numbers are weaker than the 2025 and 2026 coefficients: the all-axes pooled estimate is -0.019 (p = 0.16) and production alone is -0.033 (p = 0.15), because 2023 and 2024 are near zero. Restart exposure here includes contracts signed after August 2024 (no cutoff, as `restart_exposure_all`), so it is not strictly fixed before the outcomes it explains.

So the headline restart fall is, on the year-by-year paths, carried by production and, less clearly, science, but each rests on 5 to 11 contracts and the all-axes pre-period coefficients are also not flat (2015 and 2017 are significant).

## Caveats
- Axes are project labels, not what the money did on the ground. A "monitoring" project can include production components.
- Restart exposure by axis is lumpy: few contracts, near-constant amounts. Do not read it as a dose.
- Pooled restart numbers average over the pre-trend problem in the headline event study. Prefer the year-by-year coefficients.
- Joint (horse race) estimates are only as good as the lag-1 money by axis; axes are correlated because the same municipalities get several kinds of projects.
- Many comparisons (4 axes x 2 designs), no multiplicity correction. Read the pattern, not single p-values.
