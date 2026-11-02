simulate_panel <- function(n = 200, years = 2008:2022, seed = 1) {
  set.seed(seed)
  units <- tibble::tibble(cd_mun = sprintf("%07d", seq_len(n)), uf = rep(c("PA", "MT"), length.out = n),
                          alpha = rnorm(n), first_treat_year = sample(c(2012L, 2015L, 2018L, NA), n, replace = TRUE))
  shocks <- tidyr::expand_grid(uf = c("PA", "MT"), year = years) |> dplyr::mutate(delta = rnorm(dplyr::n()))
  tidyr::expand_grid(units, year = years) |>
    dplyr::left_join(shocks, by = c("uf", "year")) |>
    dplyr::mutate(money_lag1 = rexp(dplyr::n()) * (!is.na(first_treat_year) & year > first_treat_year),
                  treated = !is.na(first_treat_year) & year >= first_treat_year,
                  priority = rbinom(dplyr::n(), 1, 0.1), crop_price_idx = runif(dplyr::n()),
                  y = alpha + delta - 0.05 * money_lag1 - 0.2 * treated + rnorm(dplyr::n(), sd = 0.3),
                  in_sample = TRUE)
}

test_that("the main specification recovers a known dose-response effect", {
  sim <- simulate_panel()
  sim$y <- sim$y + 0.2 * sim$treated  # dose-response only
  expect_equal(unname(coef(est_main(sim))["money_lag1"]), -0.05, tolerance = 0.015, scale = 1)
})

test_that("Callaway-Sant'Anna recovers a known staggered effect", {
  sim <- simulate_panel()
  sim$y <- sim$y + 0.05 * sim$money_lag1  # binary effect only
  st <- est_staggered(sim)
  expect_equal(st$overall$overall.att, -0.2, tolerance = 0.08, scale = 1)
})

test_that("exposure event studies control for baseline blacklist status by year", {
  sim <- simulate_panel()
  sim$restart_exposure <- ifelse(is.na(sim$first_treat_year), 0, 1)
  sim$stock_2007_km2 <- 1
  m <- est_exposure(sim, "restart_exposure", 2018L)
  expect_true(any(grepl("priority_base", names(coef(m)))))
})

test_that("staggered DiD results are identical across runs (bootstrap is seeded)", {
  sim <- simulate_panel()
  a <- est_staggered(sim)$dynamic$se.egt
  b <- est_staggered(sim)$dynamic$se.egt
  expect_identical(a, b)
})

test_that("honest DiD bounds keep a large clean effect and report a breakdown value", {
  set.seed(3)
  sim <- simulate_panel(n = 400, years = 2014:2020)  # few periods: the bounds are slow to compute
  sim$exposure <- ifelse(sim$cd_mun <= "0000200", 1, 0)
  sim$y <- sim$alpha + sim$delta - 0.5 * sim$exposure * (sim$year >= 2019) + rnorm(nrow(sim), sd = 0.3)
  sim$stock_2007_km2 <- 1
  m <- est_exposure(sim, "exposure", 2018L)
  h <- est_honest(m, ref_year = 2018L, target_year = 2020L, mbar = c(0, 0.5))
  expect_true(h$breakdown_mbar >= 0.5)
  expect_true(all(h$bounds$ub[h$bounds$Mbar == 0] < 0))
})

test_that("synthetic DiD recovers a known effect for a treated group", {
  set.seed(4)
  sim <- simulate_panel(n = 150, years = 2008:2022)
  treated <- unique(sim$cd_mun)[1:20]
  sim$y <- sim$alpha + sim$delta + rnorm(nrow(sim), sd = 0.2) - 0.3 * (sim$cd_mun %in% treated & sim$year >= 2019)
  r <- est_synthdid(sim, treated, first_post = 2019L)
  expect_equal(r$estimate[r$method == "Synthetic DiD"], -0.3, tolerance = 0.1, scale = 1)
  expect_true(all(c("Synthetic DiD", "Synthetic control", "DiD") %in% r$method))
})

test_that("pct_effect converts a log-point coefficient into percent", {
  expect_equal(pct_effect(log(0.9)), -10)
})

test_that("interactive fixed effects remove a factor confounder that two-way FE misses", {
  set.seed(5)
  sim <- simulate_panel(n = 200, years = 2008:2022)
  # Treated municipalities load more on a common trend: parallel trends fail, one factor fixes it.
  sim$loading <- stats::ave(rnorm(nrow(sim)), sim$cd_mun) + !is.na(sim$first_treat_year)
  sim$y <- sim$alpha + 0.3 * sim$loading * (sim$year - 2008) - 0.2 * sim$treated + rnorm(nrow(sim), sd = 0.3)
  sim$first_project <- as.integer(sim$treated)
  sim <- as.data.frame(sim)
  expect_equal(fit_ife(sim, "y", "first_project", r = 1, CV = FALSE, se = FALSE)$att.avg, -0.2, tolerance = 0.05, scale = 1)
  expect_gt(abs(fit_ife(sim, "y", "first_project", r = 0, CV = FALSE, se = FALSE)$att.avg + 0.2), 0.2)
  expect_equal(as.numeric(fit_ife(sim, "y", "first_project", r = c(0, 3), CV = TRUE, se = FALSE)$r.cv), 1)
})

test_that("the objective split recovers an effect of one objective only", {
  sim <- simulate_panel()
  for (o in OBJECTIVES) sim[[paste0("money_", o, "_lag1")]] <- rexp(nrow(sim)) * (sim$money_lag1 > 0)
  sim$y <- sim$alpha + sim$delta - 0.1 * sim$money_monitoring_lag1 + rnorm(nrow(sim), sd = 0.3)
  sim$first_objective <- ifelse(is.na(sim$first_treat_year), NA, rep(OBJECTIVES, length.out = nrow(sim)))
  sim$first_objective <- stats::ave(sim$first_objective, sim$cd_mun, FUN = function(x) x[1])
  sim$restart_exposure_all <- 0
  sim$defor_km2 <- exp(sim$y)
  o <- est_objectives(sim, nboots = 20)
  b <- coef(o$dose)
  expect_equal(unname(b["money_monitoring_lag1"]), -0.1, tolerance = 0.03, scale = 1)
  expect_equal(unname(b["money_production_lag1"]), 0, tolerance = 0.03, scale = 1)
  expect_setequal(o$summary$objective, unname(OBJECTIVES))
})
