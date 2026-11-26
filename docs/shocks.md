# How many independent shocks does the freeze design rest on?

The freeze exposure is a shift-share: each municipality's Fund money before the break, times one
national break (2019). A shift-share instrument for Fund money (national donation inflows times
predetermined exposure) would rest on the same variation, so I checked how much independent
variation there is before building it (`R/shocks.R`, `results/shock_counts.csv`,
`results/shock_placebo_breaks.csv`).

## Findings

- **Few shocks.** The national annual Fund total has an effective number of shocks (1 / sum of squared
  year weights, as in Borusyak, Hull & Jaravel 2022) of 10.4 over 2010–2026 and 4.0 over 2019–2026.
  In practice there are three regimes: about R$ 49 million a year before the freeze, 31 million during
  it, and 54 million from 2023 (2026 alone is R$ 151 million, mostly *União com Municípios* money that
  goes to priority-list municipalities, not to the pre-2019 exposed ones). Shock-level standard errors
  would have almost no power.
- **Exposed municipalities already differed.** 113 of 623 sample municipalities received money before
  2019. They had higher deforestation (mean log 2.44 against 1.11) and a steeper pre-2019 trend
  (+0.007 against −0.050 log points a year). A pre-determined share is not the same as an exogenous one.
- **Placebo breaks.** The same design (exposure = mean money in the 3 years before minus the 4 years from
  the break; effect on log deforestation, years b−3 to b+3, same fixed effects) at every break from 2013
  to 2023. The 2019 estimate is +0.063 (se 0.086, t = 0.7) and ranks 6th of 11 by |t|; no break stands out
  (largest |t| is 1.8, at 2018). The freeze result is therefore not distinguishable from what the design
  produces at other dates, which agrees with the null in the README.

## Reading

This does not add identifying power. It shows the design leans on about three national regimes and that
nothing at 2019 separates from the placebo breaks. A shift-share instrument would have a strong first stage
(F about 26 over 2010–2026, 10 to 15 after 2018 in a rough check) but the same weakness, so I did not build one.

## Caveats

- The placebo breaks are not clean: exposure around other breaks mixes the 2019 freeze and the 2023 restart
  (the 2023 break has the opposite sign, because money rises), and windows overlap.
- Exposure scales differ by break; `per_sd` in the CSV rescales the estimate per standard deviation of exposure.
- The first-stage figures come from an exploratory script, not from the pipeline.
