test_that("APL-Babacu page parses values, dates, coverage and a refund", {
  p <- parse_project(test_path("fixtures/APL-Babacu.html"))
  expect_equal(p$project$valor_apoio, 4897085.37)
  expect_equal(p$project$date_contract, as.Date("2014-10-29"))
  expect_equal(p$project$abrangencia, "Estado do Maranhão, nos municípios de Lago do Junco, Lago da Pedra e Bacabal")
  expect_equal(nrow(p$disbursements), 4)
  last <- p$disbursements[4, ]
  expect_equal(last$value_brl, -267.95)
  expect_equal(last$date, as.Date("2020-06-26"))
})

test_that("Sementes do Portal disbursements add up to the support value", {
  p <- parse_project(test_path("fixtures/Sementes-do-Portal.html"))
  expect_equal(nrow(p$disbursements), 7)
  expect_equal(sum(p$disbursements$value_brl), 5397778.87, tolerance = 0.01, scale = 1)
  expect_equal(p$project$date_contract, as.Date("2010-03-25"))
  expect_match(p$project$abrangencia, "Apiacás", fixed = TRUE)
})

test_that("a page without a disbursement table gives zero disbursements", {
  f <- tempfile(fileext = ".html")
  writeLines("<html><body><p>Valor do apoio do Fundo Amazônia</p><p>R$ 10,00</p>
             <p>Abrangência territorial</p><p>Estado do Pará</p><p>Descrição</p></body></html>", f)
  p <- parse_project(f)
  expect_equal(nrow(p$disbursements), 0)
  expect_equal(p$project$abrangencia, "Estado do Pará")
})

test_that("all cached project pages reproduce the Fund's published total", {
  skip_if_not(has_raw("data/raw/fundo_amazonia/pages/APL-Babacu.html"))
  fund <- in_repo(build_fund())
  active <- dplyr::filter(fund$projects, !cancelled)
  expect_equal(nrow(active), 156)
  expect_equal(nrow(dplyr::semi_join(fund$disbursements, active, by = "slug")), 644)
  expect_equal(sum(fund$disbursements$value_brl[fund$disbursements$slug %in% active$slug]) / 1e6,
               2926, tolerance = 0.5, scale = 1)
})

test_that("BNDES open data lists the 163 Fund contracts with correct encoding", {
  skip_if_not(has_raw("data/raw/bndes/operacoes-nao-automaticas.csv"))
  b <- in_repo(read_bndes_fund())
  expect_equal(nrow(b), 163)
  expect_true(any(grepl("FUNDAÇÃO|FUNDACAO", b$cliente)))
  expect_false(any(grepl("�", b$cliente)))
})
