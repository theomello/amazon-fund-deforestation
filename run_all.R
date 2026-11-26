# Rebuilds everything from public sources: open amazon-fund-deforestation.Rproj
# in RStudio and source this file, or run `Rscript run_all.R` from the repo root.
# Downloads are cached in data/raw/; set REFRESH <- TRUE to fetch them again.
if (!dir.exists("data")) stop("Working directory must be the repository root (open the .Rproj file)")
suppressPackageStartupMessages(library(dplyr))
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
REFRESH <- FALSE
SENSITIVITY <- FALSE  # TRUE re-runs the slow checks (~25 min): HonestDiD bounds, synthetic DiD, interactive FE and microregion clustering in results/

prodes <- build_deforestation(REFRESH)
fund <- build_fund(REFRESH)
coverage <- build_coverage()
panel <- build_panel()
models <- estimate_all(panel)
est_mapb(panel, build_mapbiomas(REFRESH))  # alternative outcome: MapBiomas deforestation
est_axis(panel)                            # heterogeneity by Fund project axis
est_cloud(panel)                           # PRODES residual (earlier-unobserved) area as control
monthly <- build_monthly(panel)                # municipality x month: DETER, enforcement, cloud, Fund money
write_timing(monthly)                      # contract-date event studies on DETER alerts
write_shocks(panel)                        # effective number of national shocks; placebo break years
if (SENSITIVITY) {
  honest_restart(models)
  synthdid_uniao(panel)
  est_ife(panel)
  micro_check(panel)  # microregion clustering, wild bootstrap
}
report_all(panel, models)
