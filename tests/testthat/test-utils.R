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

test_that("deflate_to_2025 scales by index ratio", {
  ipca <- tibble::tibble(year = c(2020L, 2025L), index = c(100, 150))
  expect_equal(deflate_to_2025(10, 2020L, ipca), 15)
})
