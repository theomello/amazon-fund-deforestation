test_that("cloud_panel computes residual share of area, zero when absent, and a lag", {
  panel <- tibble::tibble(cd_mun = "1", year = 2010:2012, area_km2 = 100, in_sample = TRUE)
  resid <- tibble::tibble(cd_mun = "1", year = c(2010L, 2012L), residual_km2 = c(2, 5))
  d <- cloud_panel(panel, resid)
  expect_equal(d$cloud_resid, c(2, 0, 5))
  expect_equal(d$cloud_resid_lag1, c(0, 2, 0))
})

test_that("cloud_restrict drops only municipality-years above the quantile of positive values", {
  d <- tibble::tibble(cd_mun = as.character(1:10), cloud_resid = c(0, 0, 1:8), in_sample = TRUE)
  r <- cloud_restrict(d, "cloud_resid", 0.5, "year")
  expect_equal(sum(!r$in_sample), 4)  # median of 1..8 is 4.5: values 5..8 dropped
  expect_true(all(r$in_sample[d$cloud_resid == 0]))
})

test_that("residual table is complete, unique and small relative to PRODES increments", {
  skip_if_not(has_raw(CLOUD_PATH) && has_raw("data/derived/panel.csv"))
  r <- in_repo(get_residual())
  expect_false(anyDuplicated(r[c("cd_mun", "year")]) > 0)
  expect_true(all(r$residual_km2 > 0))
  expect_true(all(substr(r$cd_mun, 1, 2) %in% c(names(LEGAL_AMAZON_UF), "22", "29")))
  expect_lt(sum(r$residual_km2), 0.1 * 150000)
})
