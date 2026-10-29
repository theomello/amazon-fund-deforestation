skip_if_not(has_raw("data/manual/coverage_overrides.csv"))
overrides <- in_repo(readr::read_csv("data/manual/coverage_overrides.csv", col_types = readr::cols(.default = "c")))
munis <- in_repo(prodes_municipalities("data/raw/prodes/prodes_legal_amazon_loinames.json"))

test_that("every override names a known municipality and project, with a source", {
  codes <- overrides$cd_mun[overrides$action %in% c("add", "remove")]
  expect_true(all(codes %in% munis$cd_mun))
  expect_true(all(nzchar(overrides$source_url)))
  slugs <- sub("\\.html$", "", list.files(file.path(repo_root, "data/raw/fundo_amazonia/pages")))
  expect_true(all(overrides$slug %in% slugs))
})

test_that("priority list starts with the 36 municipalities of Portaria MMA 28/2008", {
  p <- in_repo(read_priority())
  expect_equal(sum(p$priority[p$year == 2008L]), 36)
  expect_true(all(p$cd_mun %in% munis$cd_mun))
})

test_that("priority status after the last published list is carried forward", {
  p <- in_repo(read_priority(years = 2008:2026))
  s <- function(y) p$priority[p$year == y][order(p$cd_mun[p$year == y])]
  expect_equal(s(2026L), s(2022L))
  expect_false(anyDuplicated(p[c("cd_mun", "year")]) > 0)
})
