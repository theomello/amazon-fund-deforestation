test_that("Uniao blocks are aligned to the signing month", {
  months <- seq(as.Date("2024-09-01"), as.Date("2026-07-01"), by = "month")
  m <- tibble::tibble(cd_mun = rep(c("1", "2"), each = length(months)), month = rep(months, 2), uf = "PA",
                      deter_clear_km2 = 1, cloud_amt = 50, year = prodes_year(month), priority = 1,
                      money_signed_uniao = ifelse(cd_mun == "1" & month == as.Date("2025-09-01"), 5, 0))
  b <- uniao_blocks(m)
  expect_equal(unique(b$treat[b$cd_mun == "1"]), 1L)
  expect_equal(unique(b$treat[b$cd_mun == "2"]), 0L)
  # block 0 is Sep-Nov 2025 (3 months of 1 km2), block -1 is Jun-Aug 2025
  expect_equal(b$y[b$cd_mun == "1" & b$k == 0], 3)
  expect_equal(b$y[b$cd_mun == "1" & b$k == -1], 3)
})

test_that("contract quarters drop units contracted before 2023", {
  months <- seq(as.Date("2021-01-01"), as.Date("2026-06-01"), by = "month")
  base <- tibble::tibble(month = months, uf = "PA", deter_clear_km2 = 1, cloud_amt = 50, money_signed_uniao = 0)
  m <- dplyr::bind_rows(
    dplyr::mutate(base, cd_mun = "early", first_contract_month = as.Date("2018-05-01")),
    dplyr::mutate(base, cd_mun = "late", first_contract_month = as.Date("2024-05-01")),
    dplyr::mutate(base, cd_mun = "never", first_contract_month = as.Date(NA)))
  q <- contract_quarters(m)
  expect_setequal(unique(q$cd_mun), c("late", "never"))
  expect_true(all(q$y == 3))
  expect_equal(unique(q$cohort[q$cd_mun == "never"]), 10000L)
})

test_that("avg_effect is the mean with a cluster-robust standard error", {
  set.seed(1)
  d <- data.frame(id = rep(1:50, each = 4), t = rep(1:4, 50))
  d$y <- 0.5 * d$t + rnorm(200)
  mod <- fixest::feols(y ~ i(t, ref = 1), d, cluster = ~id)
  nm <- names(stats::coef(mod))
  a <- avg_effect(mod, nm[1:2])
  expect_equal(a[["estimate"]], mean(stats::coef(mod)[nm[1:2]]))
  expect_gt(a[["se"]], 0)
})
