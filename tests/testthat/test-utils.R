test_that("prodes_year follows Aug-Jul", {
  expect_equal(prodes_year(as.Date(c("2019-07-31", "2019-08-01", "2020-06-26"))), c(2019L, 2020L, 2020L))
})

test_that("parse_brl handles thousands, decimals and negatives", {
  expect_equal(parse_brl(c("R$ 1.800.104,08", "-R$ 267,95", "R$ -267,95", "n/a")),
               c(1800104.08, -267.95, -267.95, NA))
})

test_that("norm_key strips accents and punctuation", {
  expect_equal(norm_key(c("São Félix do Xingu", "Pau D'Arco", " Xapurí ")),
               c("SAO FELIX DO XINGU", "PAU D ARCO", "XAPURI"))
})

test_that("cache_download does not hit the network when cached", {
  f <- tempfile(); writeLines("x", f)
  expect_equal(cache_download("http://invalid.invalid/x", f), f)
})

test_that("a failed download leaves no file that would later count as cached", {
  path <- file.path(tempfile(), "x.csv")
  expect_error(cache_download("http://invalid.invalid/x", path))
  expect_false(file.exists(path))
})

test_that("an iCloud placeholder counts as cached and is materialized instead of re-downloaded", {
  dir <- tempfile(); dir.create(dir)
  path <- file.path(dir, "big.csv")
  file.create(file.path(dir, ".big.csv.icloud"))
  calls <- character()
  fake_brctl <- function(p) { calls <<- c(calls, p); writeLines("x", p) }
  expect_equal(cache_download("http://invalid.invalid/x", path, materialize = fake_brctl), path)
  expect_equal(calls, path)
})

test_that("deflate_to_2025 scales by index ratio", {
  ipca <- tibble::tibble(year = c(2020L, 2025L), index = c(100, 150))
  expect_equal(deflate_to_2025(10, 2020L, ipca), 15)
})
