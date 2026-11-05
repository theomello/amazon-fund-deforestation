# Heterogeneity by project axis (the Fund's own eixo field). Every project carries exactly one
# eixo, so the classification rule is the Fund's, not ours: production = "Produção sustentável",
# monitoring = "Monitoramento e controle", territorial = "Ordenamento territorial",
# science = "Ciência, inovação e instrumentos econômicos" (OBJECTIVES in R/panel.R).
# The panel already holds last year's money per axis (money_<axis>_lag1) and the first-project axis.
# This file adds: (1) a count of projects, R$ and municipalities per axis, (2) restart exposure per
# axis (R$ million contracted from 2023, equal split, as restart_exposure_all), (3) dose-response and
# restart estimates one axis at a time and jointly (horse race).

axes <- function() unname(OBJECTIVES)  # function: panel.R is sourced after this file

# Restart exposure per municipality and axis: contracted from 2023 (no cutoff), local projects only.
axis_restart_exposure <- function(projects, links, ipca, mun_attr) {
  local <- dplyr::filter(projects, coverage_type %in% c("municipalities", "indigenous_lands"))
  out <- lapply(names(OBJECTIVES), function(e) {
    col <- paste0("restart_", OBJECTIVES[[e]])
    restart_exposures(dplyr::filter(local, eixos == e), links, ipca, mun_attr, as.Date("2100-01-01")) |>
      dplyr::rename(!!col := restart_exposure)
  })
  purrr::reduce(out, dplyr::full_join, by = "cd_mun") |>
    dplyr::mutate(dplyr::across(-cd_mun, ~ dplyr::coalesce(.x, 0)))
}

axis_panel <- function(panel) {
  read <- function(f, ...) readr::read_csv(file.path("data/derived", f), show_col_types = FALSE, ...)
  projects <- read("fund_projects.csv") |> dplyr::left_join(read("fund_coverage.csv"), by = "slug")
  ex <- axis_restart_exposure(projects, read("fund_links.csv", col_types = "ccd"), get_ipca(), mun_attributes())
  panel <- dplyr::left_join(panel, ex, by = "cd_mun") |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("restart_") & !dplyr::starts_with("restart_exposure"),
                                ~ dplyr::coalesce(.x, 0)))
  list(panel = panel, projects = projects)
}

# Classification table: counts by axis. Thin = fewer than `min_mun` in-sample municipalities with
# any axis money in the relevant window (the estimates rest on those places alone).
axis_counts <- function(ap, min_mun = 30L) {
  panel <- ap$panel; projects <- dplyr::filter(ap$projects, !cancelled)
  s <- panel[panel$in_sample, ]
  dplyr::bind_rows(lapply(names(OBJECTIVES), function(e) {
    a <- OBJECTIVES[[e]]; pr <- projects[projects$eixos == e, ]
    loc <- pr$coverage_type %in% c("municipalities", "indigenous_lands")
    mun_dose <- unique(s$cd_mun[s[[paste0("money_", a)]] > 0])
    mun_restart <- unique(s$cd_mun[s[[paste0("restart_", a)]] > 0])
    tibble::tibble(axis = a, eixo = e, projects_all = nrow(pr), projects_local = sum(loc),
                   support_all_brl_m = sum(pr$valor_apoio) / 1e6, support_local_brl_m = sum(pr$valor_apoio[loc]) / 1e6,
                   disbursed_in_sample_brl_m = sum(s[[paste0("money_", a)]]),
                   n_mun_dose = length(mun_dose), n_mun_restart = length(mun_restart),
                   restart_brl_m = sum(s[[paste0("restart_", a)]]),
                   thin_dose = length(mun_dose) < min_mun, thin_restart = length(mun_restart) < min_mun)
  }))
}

axis_row <- function(m, term, spec, axis) {
  ct <- fixest::coeftable(m)[term, , drop = FALSE]
  tibble::tibble(spec = spec, axis = axis, term = term, estimate = ct[1, 1], std.error = ct[1, 2],
                 p.value = ct[1, 4], n = stats::nobs(m))
}

# Pooled restart: exposure x 1[year >= 2023] (PRODES 2023 is the first year affected), with the
# same controls as est_exposure(). Gives one number per axis next to the year-by-year paths.
axis_restart_model <- function(panel, exposures, ref_year = 2022L, pooled = TRUE) {
  d <- dplyr::mutate(sample_of(panel), priority_base = priority[year == ref_year][1], .by = cd_mun)
  for (x in exposures) d[[paste0(x, "_post")]] <- d[[x]] * (d$year > ref_year)
  base <- sprintf("%s + i(year, stock_2007_km2, ref = %d) + i(year, priority_base, ref = %d)", CONTROLS, ref_year, ref_year)
  rhs <- if (pooled) paste(c(paste0(exposures, "_post"), base), collapse = " + ")
         else paste(c(sprintf("i(year, %s, ref = %d)", exposures, ref_year), base), collapse = " + ")
  suppressMessages(fe_model(d, rhs))
}

axis_event_table <- function(m, spec, exposures) {
  ct <- fixest::coeftable(m)
  rows <- grep("^year::[0-9]+:restart_", rownames(ct))
  tibble::tibble(spec = spec, term = rownames(ct)[rows], year = as.integer(sub("^year::([0-9]+):.*", "\\1", rownames(ct)[rows])),
                 axis = sub("^.*:restart_", "", rownames(ct)[rows]), estimate = ct[rows, 1], std.error = ct[rows, 2],
                 p.value = ct[rows, 4])
}

est_axis <- function(panel) {
  ap <- axis_panel(panel); p <- ap$panel
  money <- paste0("money_", axes(), "_lag1"); rs <- paste0("restart_", axes())
  # Dose-response: pooled benchmark, each axis alone, all four jointly, and a test of equal effects.
  dose <- dplyr::bind_rows(
    axis_row(est_main(p), "money_lag1", "Dose-response, all local money (headline)", "all"),
    dplyr::bind_rows(lapply(seq_along(axes()), function(i)
      axis_row(est_main(p, money[i]), money[i], "Dose-response, one axis at a time", axes()[i]))))
  joint <- est_main(p, paste(money, collapse = " + "))
  dose <- dplyr::bind_rows(dose, dplyr::bind_rows(lapply(seq_along(axes()), function(i)
    axis_row(joint, money[i], "Dose-response, horse race (all axes jointly)", axes()[i]))))
  w <- fixest::wald(joint, money, print = FALSE)
  # Restart: headline exposure (all axes), each axis alone, all jointly; pooled post and event studies.
  p$restart_total_axes <- rowSums(p[rs])
  head_pool <- axis_restart_model(p, "restart_exposure_all")
  restart <- dplyr::bind_rows(
    axis_row(head_pool, "restart_exposure_all_post", "Restart pooled 2023-26, all axes (headline exposure)", "all"),
    dplyr::bind_rows(lapply(rs, function(x)
      axis_row(axis_restart_model(p, x), paste0(x, "_post"), "Restart pooled 2023-26, one axis at a time", sub("restart_", "", x)))))
  jm <- axis_restart_model(p, rs)
  restart <- dplyr::bind_rows(restart, dplyr::bind_rows(lapply(rs, function(x)
    axis_row(jm, paste0(x, "_post"), "Restart pooled 2023-26, horse race", sub("restart_", "", x)))))
  w_r <- fixest::wald(jm, paste0(rs, "_post"), print = FALSE)
  # Territorial exposure is one project at R$ 0.58 million for most places plus one municipality
  # (a single-municipality project) at R$ 54.5 million; check the pooled estimate without that municipality.
  top <- p$cd_mun[which.max(p$restart_territorial)]
  restart <- dplyr::bind_rows(restart,
    axis_row(axis_restart_model(p[p$cd_mun != top, ], "restart_territorial"), "restart_territorial_post",
             "Restart pooled 2023-26, one axis at a time, without the largest-exposure municipality", "territorial"),
    axis_row(axis_restart_model(p[p$cd_mun != top, ], rs), "restart_territorial_post",
             "Restart pooled 2023-26, horse race, without the largest-exposure municipality", "territorial"))
  events <- dplyr::bind_rows(
    axis_event_table(axis_restart_model(p, "restart_exposure_all", pooled = FALSE), "headline", "restart_exposure_all") |>
      dplyr::mutate(axis = "all"),
    dplyr::bind_rows(lapply(rs, function(x) axis_event_table(axis_restart_model(p, x, pooled = FALSE), "one axis at a time", x))),
    axis_event_table(axis_restart_model(p, rs, pooled = FALSE), "horse race", rs))
  counts <- axis_counts(ap)
  tests <- tibble::tibble(test = c("Equal dose-response across axes (joint model)", "Equal pooled restart effect across axes (joint model)"),
                          statistic = c(w$stat, w_r$stat), p.value = c(w$p, w_r$p), df1 = c(w$df1, w_r$df1))
  out <- list(counts = counts, dose = dose, restart = restart, events = events, tests = tests)
  dir.create("results", showWarnings = FALSE)
  readr::write_csv(counts, "results/axis_counts.csv")
  readr::write_csv(dplyr::bind_rows(dose, restart), "results/axis_effects.csv")
  readr::write_csv(events, "results/axis_restart_event_study.csv")
  readr::write_csv(tests, "results/axis_tests.csv")
  out
}

fig_axis_restart <- function(res) {
  d <- dplyr::filter(res$events, spec == "one axis at a time") |>
    dplyr::mutate(pct = 100 * (exp(estimate) - 1), lo = 100 * (exp(estimate - 1.96 * std.error) - 1),
                  hi = 100 * (exp(estimate + 1.96 * std.error) - 1))
  p <- ggplot2::ggplot(d, ggplot2::aes(year, pct)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) + ggplot2::geom_vline(xintercept = 2022.5, linetype = 3) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.15) + ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::facet_wrap(~ axis, scales = "free_y") +
    ggplot2::labs(x = "PRODES year", y = "% change in deforestation per R$ 1 million contracted since 2023 (vs 2022)",
                  title = "Restart event study by project axis (one axis at a time)") + ggplot2::theme_minimal()
  dir.create("figures", showWarnings = FALSE)
  ggplot2::ggsave("figures/axis_restart_event_study.png", p, width = 8, height = 6, dpi = 150)
  invisible(p)
}
