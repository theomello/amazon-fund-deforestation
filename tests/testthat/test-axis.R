test_that("axis restart exposures sum to the all-axes restart exposure", {
  skip_if_not(has_raw("data/derived/panel.csv") && has_raw("data/derived/fund_projects.csv"))
  panel <- in_repo(readr::read_csv("data/derived/panel.csv", col_types = readr::cols(cd_mun = "c", .default = "?")))
  ap <- in_repo(axis_panel(panel))$panel
  m <- ap[ap$year == 2022, ]
  expect_equal(rowSums(m[paste0("restart_", axes())]), m$restart_exposure_all, tolerance = 1e-9)
  # Money by axis adds up to local money.
  expect_equal(rowSums(ap[paste0("money_", axes())]), ap$money, tolerance = 1e-9)
})

test_that("axis counts cover every project once and flag thin axes", {
  skip_if_not(has_raw("data/derived/panel.csv") && has_raw("data/derived/fund_projects.csv"))
  panel <- in_repo(readr::read_csv("data/derived/panel.csv", col_types = readr::cols(cd_mun = "c", .default = "?")))
  ap <- in_repo(axis_panel(panel))
  cnt <- axis_counts(ap)
  expect_equal(sum(cnt$projects_all), sum(!ap$projects$cancelled))
  expect_setequal(cnt$axis, axes())
  expect_equal(nrow(axis_counts(ap, min_mun = 1000L)[axis_counts(ap, min_mun = 1000L)$thin_dose, ]), 4L)
})
