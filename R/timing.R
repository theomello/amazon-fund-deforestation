# Contract-date event studies on the monthly panel. Question: did DETER clearing alerts fall before
# or after Fund contracts were signed? A fall that starts before signing means the money followed
# the decline (README caveat); a fall that starts at signing would be causal-looking evidence.
# Two designs: (1) the Uniao com Municipios programme, one signing month (Sep 2025) for 48
# municipalities; (2) staggered first contracts signed 2023-2026 against never-contracted municipalities.

UNIAO_MONTH <- as.Date("2025-09-01")

# Mean of the selected coefficients and its cluster-robust standard error.
avg_effect <- function(mod, nm) {
  w <- rep(1 / length(nm), length(nm)); b <- stats::coef(mod)[nm]; V <- stats::vcov(mod)[nm, nm, drop = FALSE]
  c(estimate = sum(w * b), se = sqrt(drop(t(w) %*% V %*% w)))
}

month_index <- function(d) as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m")) - 1L
quarter_index <- function(d) as.integer(format(d, "%Y")) * 4L + (as.integer(format(d, "%m")) - 1L) %/% 3L

# Uniao: 3-month blocks aligned to the signing month (block 0 = Sep-Nov 2025, block -1 = Jun-Aug 2025);
# `treat` = a Uniao municipality; control = every other sample municipality, or only other priority-list ones.
uniao_blocks <- function(monthly, first_block = -12L) {
  uniao <- unique(monthly$cd_mun[monthly$money_signed_uniao > 0])
  pri25 <- monthly |> dplyr::filter(year == 2025L) |> dplyr::summarise(pri25 = max(priority), .by = cd_mun)
  monthly |>
    dplyr::mutate(k = (month_index(month) - month_index(UNIAO_MONTH)) %/% 3L) |>
    dplyr::filter(k >= first_block) |>
    dplyr::summarise(y = sum(deter_clear_km2), cloud = mean(cloud_amt), uf = dplyr::first(uf), .by = c(cd_mun, k)) |>
    dplyr::mutate(treat = as.integer(cd_mun %in% uniao), asy = asinh(y)) |>
    dplyr::left_join(pri25, by = "cd_mun")
}

est_uniao_timing <- function(monthly) {
  b <- uniao_blocks(monthly)
  # coefficient names look like "k::-3:treat"
  run_one <- function(d, label, ppml = FALSE) {
    f <- stats::as.formula(paste(if (ppml) "y" else "asy", "~ i(k, treat, ref = -1) + cloud | cd_mun + uf^k"))
    mod <- suppressMessages(if (ppml) fixest::fepois(f, d, cluster = ~cd_mun) else fixest::feols(f, d, cluster = ~cd_mun))
    ct <- as.data.frame(fixest::coeftable(mod))
    k <- suppressWarnings(as.integer(gsub("^k::|:treat$", "", rownames(ct))))
    pre <- avg_effect(mod, rownames(ct)[!is.na(k) & k <= -2]); post <- avg_effect(mod, rownames(ct)[!is.na(k) & k >= 0])
    tibble::tibble(spec = label, rel = k, estimate = ct[[1]], se = ct[[2]], pre_avg = pre[["estimate"]], pre_avg_se = pre[["se"]],
                   post_avg = post[["estimate"]], post_avg_se = post[["se"]])[!is.na(k), ]
  }
  dplyr::bind_rows(
    run_one(b, "Uniao vs all other municipalities (asinh)"),
    run_one(b[b$treat == 1 | b$pri25 == 1, ], "Uniao vs other priority municipalities (asinh)"),
    run_one(b, "Uniao vs all other municipalities (Poisson)", TRUE),
    run_one(b[b$treat == 1 | b$pri25 == 1, ], "Uniao vs other priority municipalities (Poisson)", TRUE))
}

# Staggered first contracts signed 2023 or later, against municipalities never contracted. Quarterly data,
# calendar window 2021Q1-2026Q2; units contracted before 2023 are dropped (already treated, not clean controls).
contract_quarters <- function(monthly) {
  monthly |>
    dplyr::mutate(q = quarter_index(month), fq = quarter_index(first_contract_month)) |>
    dplyr::summarise(y = sum(deter_clear_km2), cloud = mean(cloud_amt), fq = dplyr::first(fq), uf = dplyr::first(uf),
                     uniao = any(money_signed_uniao > 0), .by = c(cd_mun, q)) |>
    dplyr::filter(q >= quarter_index(as.Date("2021-01-01")), q <= quarter_index(as.Date("2026-04-01")),
                  is.na(fq) | fq >= quarter_index(as.Date("2023-01-01"))) |>
    dplyr::mutate(cohort = ifelse(is.na(fq), 10000L, fq), asy = asinh(y))
}

est_contract_timing <- function(monthly, lo = -8L, hi = 4L) {
  d <- contract_quarters(monthly)
  fit <- function(dd, label) {
    mod <- fixest::feols(asy ~ sunab(cohort, q, ref.p = -1) + cloud | cd_mun + uf^q, data = dd, cluster = ~cd_mun)
    ct <- as.data.frame(fixest::coeftable(mod))
    rel <- suppressWarnings(as.integer(sub("^q::", "", rownames(ct))))
    keep <- !is.na(rel) & rel >= lo & rel <= hi
    pre <- stats::aggregate(mod, c(pre = "q::-[2-8]:cohort"))  # mean of the leads -8..-2
    cm <- fixest::coeftable(summary(mod, agg = "att"))
    tibble::tibble(spec = label, rel = rel[keep], estimate = ct[[1]][keep], se = ct[[2]][keep],
                   pre_avg = pre["pre", "Estimate"], pre_avg_se = pre["pre", "Std. Error"], att = cm["ATT", "Estimate"], att_se = cm["ATT", "Std. Error"], n_units = dplyr::n_distinct(dd$cd_mun))
  }
  dplyr::bind_rows(fit(d, "All municipalities contracted from 2023"), fit(d[!d$uniao, ], "Excluding Uniao municipalities"))
}

fig_timing <- function(uniao, contract) {
  one <- function(d, title) {
    d$lo <- d$estimate - 1.96 * d$se; d$hi <- d$estimate + 1.96 * d$se
    ggplot2::ggplot(d, ggplot2::aes(rel, estimate, colour = spec, fill = spec)) +
      ggplot2::geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
      ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed", colour = INK2, linewidth = 0.4) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.12, colour = NA) +
      ggplot2::geom_line(linewidth = 0.6) + ggplot2::geom_point(size = 1.4) +
      ggplot2::labs(title = title, x = "Periods since signing (reference: the period before)", y = "Effect on asinh(DETER clearing km²)", colour = NULL, fill = NULL) +
      theme_report()
  }
  u <- uniao[grepl("asinh", uniao$spec), ]
  p1 <- one(u, "Uniao com Municipios: signing in Sep 2025 (blocks of 3 months, k = 0 is Sep-Nov 2025)")
  p2 <- one(contract, "First contract 2023-2026 against never-contracted municipalities")
  save_fig(patchwork::wrap_plots(p1, p2, ncol = 1), "13_contract_timing", width = 8, height = 8)
}

write_timing <- function(monthly) {
  u <- est_uniao_timing(monthly); cn <- est_contract_timing(monthly)
  readr::write_csv(u, "results/timing_uniao.csv"); readr::write_csv(cn, "results/timing_contract.csv")
  fig_timing(u, cn)
  invisible(list(uniao = u, contract = cn))
}
