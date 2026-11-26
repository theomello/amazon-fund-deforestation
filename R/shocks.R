# How many independent shocks does the freeze design rest on? The freeze exposure is a
# shift-share: municipal Fund money before the break times one national break. With a single
# break, shock-level inference (Borusyak, Hull & Jaravel 2022; Adao, Kolesar & Morales 2019) has
# nothing to work with, so this file reports (1) the effective number of national shocks and
# (2) a placebo-break test: the same design at every other possible break year, to see whether
# 2019 stands out from breaks at which nothing happened to the Fund.

# Exposure at break `b`: mean municipal money in the three years before minus the four years from
# `b` on (b = 2019 reproduces freeze_exposure: 2016-18 against 2019-22).
shock_exposure <- function(panel, b) {
  d <- sample_of(panel)
  dplyr::summarise(d, exposure = mean(money[year %in% (b - 3):(b - 1)]) - mean(money[year %in% b:(b + 3)]),
                   .by = cd_mun)
}

# Pooled DiD at break `b`: exposure x post, years b-3 .. b+3, same fixed effects and controls.
est_shock_break <- function(panel, b) {
  d <- sample_of(panel)
  d <- d[d$year %in% (b - 3):(b + 3), ]
  d <- dplyr::inner_join(d, shock_exposure(panel, b), by = "cd_mun")
  d$exp_post <- d$exposure * (d$year >= b)
  m <- suppressMessages(fe_model(d, paste("exp_post +", CONTROLS)))  # crop_price_idx is absorbed by state x year in short windows
  tibble::tibble(break_year = b, estimate = unname(stats::coef(m)["exp_post"]),
                 se = unname(fixest::se(m)["exp_post"]),
                 sd_exposure = stats::sd(unique(d[c("cd_mun", "exposure")])$exposure))
}

# Effective number of shocks, 1 / sum of squared year weights (Borusyak et al.'s effective sample size
# for the shock side), on the national annual Fund total.
shock_counts <- function(panel, years = 2010:2026) {
  n <- dplyr::summarise(sample_of(panel), national = sum(money), .by = year)
  n <- n[n$year %in% years, ]
  w <- n$national / sum(n$national)
  1 / sum(w^2)
}

est_shocks <- function(panel, breaks = 2013:2023) {
  out <- dplyr::bind_rows(lapply(breaks, function(b) est_shock_break(panel, b)))
  out$t <- out$estimate / out$se
  # exposure scales differ across breaks, so compare effects per standard deviation of exposure
  out$per_sd <- out$estimate * out$sd_exposure
  out$rank_abs_t <- rank(-abs(out$t))
  out
}

shocks_summary <- function(panel) {
  tibble::tibble(
    quantity = c("effective_shocks_2010_2026", "effective_shocks_2019_2026"),
    value = c(shock_counts(panel), shock_counts(panel, 2019:2026)))
}

write_shocks <- function(panel) {
  readr::write_csv(est_shocks(panel), "results/shock_placebo_breaks.csv")
  readr::write_csv(shocks_summary(panel), "results/shock_counts.csv")
}
