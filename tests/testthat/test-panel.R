flat_ipca <- tibble::tibble(year = 2008:2026, index = 100)
mun_attr <- tibble::tibble(cd_mun = c("A", "B", "C"), uf = c("PA", "PA", "MT"),
                           area_km2 = c(100, 300, 50), forest_km2 = c(90, 30, 50))
disb <- tibble::tibble(slug = c("p1", "p1", "p2", "p3"), date = as.Date(c("2019-08-15", "2021-03-01", "2012-05-05", "2015-01-01")),
                       value_brl = c(100, -10, 40, 999))
links <- tibble::tibble(slug = c("p1", "p1", "p2"), cd_mun = c("A", "B", "C"), weight = 1)

test_that("a disbursement is split equally and dated by PRODES year", {
  m <- allocate_money(disb, links, flat_ipca, mun_attr, method = "equal")
  expect_equal(m$money_brl[m$cd_mun == "A" & m$year == 2020L], 50)
  expect_equal(m$money_brl[m$cd_mun == "B" & m$year == 2021L], -5)
})

test_that("allocation conserves money for every method, and unlinked projects add nothing", {
  linked_total <- sum(disb$value_brl[disb$slug %in% links$slug])
  for (method in c("equal", "area", "forest")) {
    m <- allocate_money(disb, links, flat_ipca, mun_attr, method = method)
    expect_equal(sum(m$money_brl), linked_total, tolerance = 1e-9)
  }
  expect_false(any(m$money_brl == 999))
})

test_that("area weights follow municipal area", {
  m <- allocate_money(disb, links, flat_ipca, mun_attr, method = "area")
  expect_equal(m$money_brl[m$cd_mun == "B" & m$year == 2020L], 75)
})

test_that("money is deflated to 2025 reais", {
  ipca <- tibble::tibble(year = 2008:2026, index = ifelse(2008:2026 == 2025, 200, 100))
  m <- allocate_money(disb, links, ipca, mun_attr, method = "equal")
  expect_equal(m$money_brl[m$cd_mun == "A" & m$year == 2020L], 100)
})

test_that("state-level money is spread over the state's municipalities by forest", {
  proj <- tibble::tibble(slug = "s1", local = "Pará", abrangencia = "Estado do Pará")
  d <- tibble::tibble(slug = "s1", date = as.Date("2015-01-01"), value_brl = 120)
  m <- allocate_state_money(d, proj, flat_ipca, mun_attr)
  expect_equal(m$state_money_brl[m$cd_mun == "A"], 90)
  expect_equal(m$state_money_brl[m$cd_mun == "B"], 30)
  expect_false("C" %in% m$cd_mun)
})

test_that("freeze exposure is pre-freeze minus freeze-period mean money", {
  money <- tidyr::expand_grid(cd_mun = c("A", "B"), year = 2016:2022) |>
    dplyr::mutate(money = ifelse(cd_mun == "A" & year <= 2018, 3, ifelse(cd_mun == "A", 1, 0)))
  e <- freeze_exposures(money)
  expect_equal(e$freeze_intensity[e$cd_mun == "A"], 3)
  expect_equal(e$freeze_exposure[e$cd_mun == "A"], 2)
  expect_equal(e$freeze_exposure[e$cd_mun == "B"], 0)
})

test_that("restart exposure only counts contracts signed before the cutoff", {
  proj <- tibble::tibble(slug = c("p1", "p2"), valor_apoio = c(10e6, 20e6),
                         date_contract = as.Date(c("2023-05-01", "2025-03-01")))
  l <- tibble::tibble(slug = c("p1", "p2"), cd_mun = c("A", "A"), weight = 1)
  e <- restart_exposures(proj, l, flat_ipca, mun_attr, cutoff = as.Date("2024-08-01"))
  expect_equal(e$restart_exposure[e$cd_mun == "A"], 10)
})

test_that("flora fines are counted per municipality and PRODES year, without cancelled ones", {
  f <- tibble::tibble(COD_MUNICIPIO = c("1500602", "1500602", "1500602", "1500602"),
                      DAT_HORA_AUTO_INFRACAO = c("2019-08-02 10:00:00", "2020-07-30 10:00:00", "2020-08-01 10:00:00", "2019-09-01 00:00:00"),
                      TIPO_INFRACAO = c("Flora", "Flora", "Flora", "Fauna"), SIT_CANCELADO = c("N", "N", "N", "N"))
  n <- count_fines(f)
  expect_equal(n$fines_flora[n$year == 2020L], 2)
  expect_equal(n$fines_flora[n$year == 2021L], 1)
})

test_that("the built panel is a clean municipality x year grid", {
  skip_if_not(has_raw("data/raw/ibama/auto_infracao_csv.zip"))
  p <- in_repo(build_panel())
  expect_false(anyDuplicated(p[c("cd_mun", "year")]) > 0)
  expect_false(anyNA(p$cd_mun) || anyNA(p$year) || anyNA(p$y))
  a <- p[p$cd_mun == p$cd_mun[1], ]
  expect_equal(a$money_lag1[-1], a$money[-nrow(a)])
})
