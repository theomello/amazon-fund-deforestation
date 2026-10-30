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
  sim <- simulate_panel(n = 400, years = 2008:2022)
  sim$exposure <- ifelse(sim$cd_mun <= "0000200", 1, 0)
  sim$y <- sim$alpha + sim$delta - 0.5 * sim$exposure * (sim$year >= 2019) + rnorm(nrow(sim), sd = 0.3)
  sim$stock_2007_km2 <- 1
  m <- est_exposure(sim, "exposure", 2018L)
  h <- est_honest(m, ref_year = 2018L, target_year = 2020L)
  expect_true(h$breakdown_mbar >= 0.5)
  expect_true(all(h$bounds$ub[h$bounds$Mbar == 0] < 0))
})

test_that("pct_effect converts a log-point coefficient into percent", {
  expect_equal(pct_effect(log(0.9)), -10)
})
