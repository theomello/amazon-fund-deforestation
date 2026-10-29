munis <- tibble::tribble(
  ~cd_mun,   ~name,                ~uf,
  "2106003", "Lago do Junco",      "MA",
  "2105906", "Lago da Pedra",      "MA",
  "2101202", "Bacabal",            "MA",
  "1302405", "Juruá",              "AM",
  "1303205", "Novo Aripuanã",      "AM",
  "5101408", "Aripuanã",           "MT",
  "1100254", "Presidente Médici",  "RO",
  "2109239", "Presidente Médici",  "MA",
  "1500602", "Altamira",           "PA")

project <- function(abrangencia, local = "") {
  tibble::tibble(slug = "p", local = local, abrangencia = abrangencia, valor_apoio = 1)
}
classify <- function(...) classify_coverage(project(...), munis)$coverage_type
links <- function(...) match_municipalities(project(...), munis)$cd_mun

test_that("named municipalities are matched within the stated state", {
  txt <- "Estado do Maranhão, nos municípios de Lago do Junco, Lago da Pedra e Bacabal"
  expect_setequal(links(txt, "Maranhão"), c("2106003", "2105906", "2101202"))
  expect_equal(classify(txt, "Maranhão"), "municipalities")
})

test_that("a region named after a river is not a municipality", {
  expect_length(links("Região do Alto Juruá, no estado do Acre", "Acre"), 0)
  expect_equal(classify("Região do Alto Juruá, no estado do Acre", "Acre"), "state")
})

test_that("projects outside the Legal Amazon and biome-wide projects are classified", {
  expect_equal(classify("Estado da Bahia", "Bahia"), "outside")
  expect_equal(classify("Amazônia Legal", "Acre, Amazonas, Pará"), "biome_national")
})

test_that("indigenous-land projects without municipality names are flagged", {
  expect_equal(classify("Terras Indígenas Kayapó e Las Casas, no sul do Pará", "Pará"), "indigenous_lands")
})

test_that("a shorter name inside a longer matched name is dropped", {
  expect_equal(links("Município de Novo Aripuanã", "Amazonas"), "1303205")
})

test_that("a shorter name listed on its own is kept next to a longer one", {
  m <- dplyr::bind_rows(munis, tibble::tribble(~cd_mun, ~name, ~uf,
                                               "2104800", "Grajaú", "MA",
                                               "2105351", "Itaipava do Grajaú", "MA"))
  l <- match_municipalities(project("Maranhão: Grajaú, Itaipava do Grajaú e Bacabal", "Maranhão"), m)
  expect_setequal(l$cd_mun, c("2104800", "2105351", "2101202"))
})

test_that("duplicate municipality names resolve by state", {
  expect_equal(links("Município de Presidente Médici", "Rondônia"), "1100254")
})

test_that("municipalities whose names start with Rio or Alto still match", {
  m <- dplyr::bind_rows(munis, tibble::tribble(~cd_mun, ~name, ~uf,
                                               "1303569", "Rio Preto da Eva", "AM",
                                               "1100403", "Alto Paraíso", "RO"))
  l <- match_municipalities(project("Manaus, Rio Preto da Eva e o Rio Juruá; Alto Paraíso (RO)", "Amazonas"), m)
  expect_setequal(l$cd_mun, c("1303569", "1100403"))
})

test_that("indigenous-land money is weighted by the land's overlap with each municipality", {
  sq <- function(x0, x1) sf::st_polygon(list(rbind(c(x0, 0), c(x1, 0), c(x1, 1), c(x0, 1), c(x0, 0))))
  mun_geo <- sf::st_sf(cd_mun = c("A", "B"), geometry = sf::st_sfc(sq(0, 3), sq(3, 6), crs = 5880))
  tis <- sf::st_sf(name = c("Kayapó", "Baú"), geometry = sf::st_sfc(sq(1.5, 3.5), sq(10, 11), crs = 5880))
  w <- match_indigenous_lands(tibble::tibble(slug = "p", abrangencia = "Terra Indígena Kayapó"), tis, mun_geo)
  expect_equal(w$weight[w$cd_mun == "A"], 0.75)
  expect_equal(w$weight[w$cd_mun == "B"], 0.25)
})

test_that("overrides add, remove and retype", {
  l <- tibble::tibble(slug = "p", cd_mun = "1500602")
  ov <- tibble::tribble(~slug, ~action, ~cd_mun, ~coverage_type,
                        "p", "remove", "1500602", NA,
                        "p", "add", "2101202", NA,
                        "p", "set_type", NA, "municipalities")
  out <- apply_overrides(l, tibble::tibble(slug = "p", coverage_type = "state"), ov)
  expect_equal(out$links$cd_mun, "2101202")
  expect_equal(out$projects$coverage_type, "municipalities")
})

test_that("coverage shares match the exploration within 3 points", {
  skip_if_not(has_raw("data/raw/fundo_amazonia/pages/APL-Babacu.html"))
  cov <- in_repo(build_coverage(overrides = FALSE, write = FALSE))
  s <- dplyr::summarise(cov$projects, v = sum(valor_apoio, na.rm = TRUE), .by = coverage_type)
  share <- setNames(s$v / sum(s$v), s$coverage_type)
  expect_equal(unname(share["municipalities"]), 0.23, tolerance = 0.03, scale = 1)
  expect_equal(unname(share["state"]), 0.29, tolerance = 0.03, scale = 1)
  expect_equal(unname(share["biome_national"]), 0.46, tolerance = 0.03, scale = 1)
})
