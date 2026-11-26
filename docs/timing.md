# Contract-date event studies on monthly DETER alerts

**Question.** The restart result is a fall in deforestation where new Fund money arrived (README). Did the
fall start *before* the contracts were signed (money followed the decline) or *at* signing? Annual PRODES
cannot tell, so this uses the municipality x month panel (`R/monthly.R`) and DETER clearing alerts
(`DESMATAMENTO_CR`, `_VEG`, `MINERACAO`), with NASA POWER cloud amount as a control. Code in `R/timing.R`,
results in `results/timing_uniao.csv` and `results/timing_contract.csv`, figure `figures/13_contract_timing.png`.

## Designs

1. **Uniao com Municipios** (48 municipalities, all on the priority list, all signed in Sep 2025). Blocks of 3 months
   aligned to the signing month (block 0 = Sep-Nov 2025, reference = block -1), 12 blocks before and 4 after.
   Municipality and state x block fixed effects, cloud control, clustered by municipality. Controls: all other
   sample municipalities, or only the other priority-list municipalities (33). Outcome asinh(km2) and Poisson.
2. **Staggered first contracts, 2023-2026** against never-contracted municipalities (Sun and Abraham, quarterly,
   2021Q1-2026Q2). Units contracted before 2023 are dropped. Excluding the Uniao municipalities gives the same picture.

## Results

| Design | Mean of leads | Mean after signing |
|---|---|---|
| Uniao vs other priority municipalities, asinh | +0.12 (se 0.18) | -0.03 (0.19) |
| Uniao vs other priority municipalities, Poisson | +0.11 (0.18) | +0.03 (0.21) |
| First contract 2023+ vs never contracted, asinh | +0.06 (0.07) | -0.09 (0.07) |

- **No visible break at signing.** Against comparable (priority) municipalities, Uniao municipalities show no pre-trend and
  no fall after signing. This agrees with the synthetic DiD in the README (+1.4 % against other blacklisted municipalities).
- **Staggered first contracts:** the leads drift down (about +0.15 eight quarters before, -0.18 two quarters before) and the
  post-signing coefficients stay between -0.16 and 0, individually insignificant except quarter 4 (-0.16, se 0.07).
  The mean after signing minus the mean before is about -0.15 (not significant). This is the shape one expects if money
  followed a decline, but it is too imprecise to establish it.
- **Against all other municipalities the Uniao estimates are not interpretable.** They swing between -1.2 and +0.6 even
  before signing, at the same calendar season in each year, so seasonal differences between large frontier municipalities
  and small ones are not absorbed. The post-signing mean (-0.61, se 0.11) should not be read as an effect.

## Caveats

- DETER measures alerts, not final PRODES clearing (it totals 60-95 % of PRODES a year; correlation 0.93 across
  municipality-years in logs). Sensor changes (WFI added from 2020) can shift detection.
- Only 4 post-signing quarters exist for the later cohorts, and the Uniao design has one signing date.
- The cloud series is a coarse modelled proxy (`R/monthly.R`), not the cloud that blocked DETER.
- Signing dates are the Fund's contract dates; announcement and negotiation come earlier, so money can still follow
  a decline that began before the contract.
