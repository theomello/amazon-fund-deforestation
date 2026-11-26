test_that("DETER alerts are grouped by class and summed by month", {
  d <- tibble::tibble(classname = c("DESMATAMENTO_CR", "MINERACAO", "DEGRADACAO", "CICATRIZ_DE_QUEIMADA", "OTHER"),
                      view_date = c("2020-03-02", "2020-03-20", "2020-03-21", "2020-04-01", "2020-03-05"),
                      mun_geocod = "1100015", areamunkm = c("1.5", "0.5", "2", "3", "9"))
  m <- deter_monthly(d)
  mar <- m[m$month == as.Date("2020-03-01"), ]
  expect_equal(mar$deter_clear_km2, 2)
  expect_equal(mar$deter_clear_n, 2)
  expect_equal(mar$deter_degrad_km2, 2)
  expect_equal(m$deter_fire_km2[m$month == as.Date("2020-04-01")], 3)
  expect_false(any(m$deter_clear_km2 > 2))  # the unknown class is dropped
})

test_that("embargo terms are counted once per term and month", {
  e <- tibble::tibble(SEQ_TAD = c("1", "1", "2"), COD_MUNICIPIO = "1100015", UF = "RO",
                      DAT_EMBARGO = c("2020-03-02 10:00:00", "2020-03-02 10:00:00", "2020-03-30 10:00:00"), SIT_CANCELADO = "N")
  e <- dplyr::distinct(e, SEQ_TAD, .keep_all = TRUE)
  expect_equal(embargoes_monthly(e)$embargoes, 2)
})

test_that("monthly money allocation sums to the annual allocation", {
  disb <- tibble::tibble(slug = "a", date = as.Date(c("2020-09-15", "2020-10-02", "2021-03-01")), value_brl = c(100, 50, 25))
  links <- tibble::tibble(slug = "a", cd_mun = c("1", "2"), weight = 1)
  attr <- tibble::tibble(cd_mun = c("1", "2"), area_km2 = 1, forest_km2 = 1)
  ipca <- tibble::tibble(year = 2015:2026, index = 1)
  mo <- allocate_money(disb, links, ipca, attr, period = "month")
  yr <- allocate_money(disb, links, ipca, attr, period = "year")
  expect_equal(sum(mo$money_brl), sum(yr$money_brl))
  expect_equal(nrow(mo), 6)
  expect_equal(mo$money_brl[mo$cd_mun == "1" & mo$month == as.Date("2020-09-01")], 50)
})
