# Estimation (spec §6). Outcome y = log(deforestation km² + 0.01); Fund money in
# R$ million (2025 reais); municipality and state x year fixed effects.

CONTROLS <- "priority + crop_price_idx"

sample_of <- function(panel) panel[panel$in_sample, ]

fe_model <- function(data, rhs, outcome = "y", cluster = ~cd_mun) {
  fixest::feols(stats::as.formula(paste(outcome, "~", rhs, "|", "cd_mun + uf^year")), data = data, cluster = cluster)
}

# Main specification: last year's municipality-specific Fund money.
est_main <- function(panel, treatment = "money_lag1", outcome = "y", extra = NULL, cluster = ~cd_mun) {
  fe_model(sample_of(panel), paste(c(treatment, CONTROLS, extra), collapse = " + "), outcome, cluster)
}

# Binary staggered DiD: first contract of a municipality-specific project.
est_staggered <- function(panel) {
  d <- sample_of(panel) |>
    dplyr::mutate(id = as.integer(factor(cd_mun)), g = dplyr::coalesce(first_treat_year, 0L),
                  cohort = dplyr::coalesce(first_treat_year, 10000L))
  set.seed(2026)  # did bootstraps its standard errors; fix the draw so results are reproducible
  att <- did::att_gt(yname = "y", tname = "year", idname = "id", gname = "g", data = as.data.frame(d),
                     control_group = "notyettreated", allow_unbalanced_panel = TRUE, print_details = FALSE)
  list(att = att,
       dynamic = did::aggte(att, type = "dynamic", min_e = -6, max_e = 8, na.rm = TRUE),
       overall = did::aggte(att, type = "simple", na.rm = TRUE),
       sunab = fixest::feols(y ~ sunab(cohort, year) | cd_mun + year, data = d, cluster = ~cd_mun))
}

# Freeze/restart event studies: exposure x year, relative to `ref_year`.
est_exposure <- function(panel, exposure, ref_year, ever_treated_only = FALSE) {
  d <- sample_of(panel)
  if (ever_treated_only) d <- d[!is.na(d$first_treat_year), ]
  # Programmes target the priority blacklist, so blacklist status at the reference year gets its own year path.
  d <- dplyr::mutate(d, priority_base = priority[year == ref_year][1], .by = cd_mun)
  rhs <- sprintf("i(year, %s, ref = %d) + %s + i(year, stock_2007_km2, ref = %d) + i(year, priority_base, ref = %d)",
                 exposure, ref_year, CONTROLS, ref_year, ref_year)
  fe_model(d, rhs)
}

# Rambachan & Roth (2023) sensitivity: how large may post-period violations of
# parallel trends be, relative to the largest pre-period change (Mbar), before
# the 95% CI for `target_year` includes zero? The breakdown value is that Mbar.
est_honest <- function(model, ref_year, target_year, mbar = seq(0, 2, by = 0.25)) {
  b <- stats::coef(model)
  es <- grep("^year::[0-9]+:", names(b), value = TRUE)
  es <- es[!grepl("stock|priority_base", es)]
  years <- as.integer(sub("^year::([0-9]+):.*", "\\1", es))
  es <- es[order(years)]; years <- sort(years)
  post <- years > ref_year
  l_vec <- as.numeric(years[post] == target_year)
  # At large Mbar the robust CI runs past HonestDiD's grid ("CI is open"); that only widens intervals
  # that already include zero, so it does not move the breakdown value.
  bounds <- suppressWarnings(HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = b[es], sigma = stats::vcov(model)[es, es], numPrePeriods = sum(!post), numPostPeriods = sum(post),
    l_vec = l_vec, Mbarvec = mbar))
  original <- HonestDiD::constructOriginalCS(betahat = b[es], sigma = stats::vcov(model)[es, es],
                                             numPrePeriods = sum(!post), numPostPeriods = sum(post), l_vec = l_vec)
  excludes_zero <- bounds$ub < 0 | bounds$lb > 0
  list(bounds = tibble::as_tibble(bounds), original = tibble::as_tibble(original),
       breakdown_mbar = if (excludes_zero[1]) max(bounds$Mbar[cumprod(excludes_zero) == 1]) else NA_real_)
}

# Sensitivity of the restart headline (all contracts) for 2025 and 2026; slow (~10 min).
honest_restart <- function(models) {
  out <- dplyr::bind_rows(lapply(c(2025L, 2026L), function(y) {
    h <- est_honest(models$restart_all, ref_year = 2022L, target_year = y)
    dplyr::bind_rows(dplyr::mutate(h$original, Mbar = NA_real_), h$bounds) |>
      dplyr::transmute(target_year = y, Mbar = as.numeric(Mbar), lb = as.numeric(lb), ub = as.numeric(ub),
                       breakdown_mbar = h$breakdown_mbar)
  }))
  dir.create("results", showWarnings = FALSE)
  readr::write_csv(out, "results/honest_restart.csv")
  out
}

# Mechanism: does Fund money bring more enforcement (IBAMA flora fines)?
est_mechanism <- function(panel) {
  est_main(dplyr::mutate(panel, log_fines = log(fines_flora + 1)), outcome = "log_fines")
}

# Robustness checks (spec §6.4), one row each.
est_robustness <- function(panel) {
  p <- panel |>
    dplyr::mutate(defor_norm = (defor_km2 - mean(defor_km2[year %in% 2008:2012])) / stats::sd(defor_km2[year %in% 2008:2012]),
                  defor_share = 100 * defor_km2 / forest_km2, y_asinh = asinh(defor_km2),
                  money_with_state_lag1 = money_lag1 + money_state_lag1, uf_year = paste(uf, year), .by = cd_mun) |>
    dplyr::mutate(defor_norm = ifelse(is.finite(defor_norm), defor_norm, NA))
  row <- function(spec, m, treatment = "money_lag1") {
    ct <- fixest::coeftable(m)
    tibble::tibble(spec = spec, estimate = ct[treatment, 1], std.error = ct[treatment, 2],
                   p.value = ct[treatment, 4], n = stats::nobs(m))
  }
  rows <- list(
    row("Main specification", est_main(p)),
    row("Outcome: normalized deforestation", est_main(p, outcome = "defor_norm")),
    row("Outcome: % of baseline forest cleared", est_main(p, outcome = "defor_share")),
    row("Outcome: asinh(deforestation)", est_main(p, outcome = "y_asinh")),
    row("Treatment incl. state-level money", est_main(p, "money_with_state_lag1"), "money_with_state_lag1"),
    row("Allocation by municipal area", est_main(p, "money_area_lag1"), "money_area_lag1"),
    row("Allocation by baseline forest", est_main(p, "money_forest_lag1"), "money_forest_lag1"),
    row("No lag", est_main(p, "money"), "money"),
    row("Two-year lag", est_main(p, "money_lag2"), "money_lag2"),
    row("Baseline deforestation x trend", est_main(p, extra = "stock_2007_km2:year")),
    row("Cattle price control (2010+)", est_main(p, extra = "cattle_price_idx")),
    row("Two-way clustering (municipality, state-year)", est_main(p, cluster = ~cd_mun + uf_year)))
  drop_state <- lapply(sort(unique(p$uf)), function(s) row(paste("Without", s), est_main(p[p$uf != s, ])))
  dplyr::bind_rows(c(rows, drop_state))
}

# A log-point coefficient as % change in deforestation per R$ 1 million.
pct_effect <- function(beta) 100 * (exp(beta) - 1)

estimate_all <- function(panel) {
  dir.create("output/models", recursive = TRUE, showWarnings = FALSE)
  models <- list(main = est_main(panel), staggered = est_staggered(panel), mechanism = est_mechanism(panel),
                 freeze = est_exposure(panel, "freeze_exposure", 2018L),
                 freeze_intensity = est_exposure(panel, "freeze_intensity", 2018L),
                 freeze_treated = est_exposure(panel, "freeze_exposure", 2018L, ever_treated_only = TRUE),
                 restart = est_exposure(panel, "restart_exposure", 2022L),
                 restart_all = est_exposure(panel, "restart_exposure_all", 2022L),
                 restart_no_uniao = est_exposure(panel, "restart_exposure_no_uniao", 2022L),
                 restart_asinh = est_exposure(dplyr::mutate(panel, restart_asinh = asinh(restart_exposure)), "restart_asinh", 2022L),
                 robustness = est_robustness(panel))
  saveRDS(models, "output/models/models.rds")
  models
}
