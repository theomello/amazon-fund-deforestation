test_that("break-2019 exposure reproduces freeze_exposure", {
  skip_if_not(has_raw("data/derived/panel.csv"))
  panel <- in_repo(readr::read_csv("data/derived/panel.csv", col_types = readr::cols(cd_mun = "c", .default = "?")))
  a <- shock_exposure(panel, 2019)
  b <- unique(panel[panel$in_sample, c("cd_mun", "freeze_exposure")])
  m <- merge(a, b, by = "cd_mun")
  expect_equal(m$exposure, m$freeze_exposure, tolerance = 1e-9)
})

test_that("effective shocks are 1/HHI of the national series", {
  panel <- tibble::tibble(in_sample = TRUE, year = rep(2019:2022, each = 2), money = c(1, 1, 1, 1, 1, 1, 1, 1))
  expect_equal(shock_counts(panel, 2019:2022), 4)
  panel$money[panel$year == 2019] <- 100
  expect_lt(shock_counts(panel, 2019:2022), 1.1)
})
