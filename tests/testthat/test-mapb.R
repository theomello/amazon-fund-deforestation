test_that("mapb_panel pairs PRODES year t with MapBiomas year t, or t-1 with shift", {
  panel <- tibble::tibble(cd_mun = "1", year = 2010:2012, defor_km2 = 1, y = 0)
  mb <- tibble::tibble(cd_mun = "1", year = 2009:2012, mb_forest_km2 = c(1, 2, 3, 4), mb_primary_km2 = 5, mb_all_km2 = 6)
  expect_equal(mapb_panel(panel, mb)$mb_forest_km2, c(2, 3, 4))
  expect_equal(mapb_panel(panel, mb, shift = 1L)$mb_forest_km2, c(1, 2, 3))
  expect_equal(mapb_panel(panel, mb)$y_mb_forest, log(c(2, 3, 4) + 0.01))
})

test_that("MapBiomas municipal table is complete and its measures are nested", {
  skip_if_not(has_raw(MAPB_PATH) && has_raw("data/derived/panel.csv"))
  mb <- in_repo(read_mapbiomas())
  panel <- in_repo(readr::read_csv("data/derived/panel.csv", col_types = readr::cols(cd_mun = "c", .default = "?")))
  expect_true(all(panel$cd_mun %in% mb$cd_mun))
  expect_false(anyDuplicated(mb[c("cd_mun", "year")]) > 0)
  expect_true(all(mb$mb_forest_km2 <= mb$mb_primary_km2 + 1e-9))
  expect_true(all(mb$mb_primary_km2 <= mb$mb_all_km2 + 1e-9))
  expect_equal(max(mb$year), 2025L)
  # The two satellite series should agree on where deforestation happens.
  d <- dplyr::inner_join(panel, mb, by = c("cd_mun", "year"))
  expect_gt(cor(d$defor_km2, d$mb_forest_km2), 0.9)
})
