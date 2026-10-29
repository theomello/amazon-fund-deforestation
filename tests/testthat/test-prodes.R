fixture <- list(data = test_path("fixtures/prodes_small_data.json"),
                names = test_path("fixtures/prodes_small_loinames.json"))

test_that("read_prodes sums municipal increments into a full gid x year grid", {
  p <- read_prodes(fixture)
  expect_equal(nrow(p), 6)  # 3 municipalities x {2008, 2020}; 1500-2007 stock excluded
  expect_setequal(unique(p$year), c(2008L, 2020L))
  get <- function(code, y) p$defor_km2[p$cd_mun == code & p$year == y]
  expect_equal(get("1500602", 2020L), 797.662352, tolerance = 1e-6)   # Altamira
  expect_equal(get("1100254", 2008L), 3.501181, tolerance = 1e-6)     # Presidente Médici (RO)
  expect_equal(get("2109239", 2020L), 0)                               # Presidente Médici (MA): no polygons
})

test_that("read_prodes_stock returns the 1500-2007 accumulated area", {
  s <- read_prodes_stock(fixture)
  expect_equal(s$stock_2007_km2[s$cd_mun == "1500602"], 5916.066348, tolerance = 1e-6)
})

test_that("municipalities sharing a name keep distinct IBGE codes", {
  p <- read_prodes(fixture)
  medici <- unique(p[p$prodes_name == "Presidente Médici", c("cd_mun", "uf")])
  expect_equal(nrow(medici), 2)
  expect_setequal(medici$uf, c("RO", "MA"))
})

test_that("the full PRODES file covers exactly the 772 Legal Amazon municipalities", {
  skip_if_not(has_raw("data/raw/prodes/prodes_legal_amazon_data.json"))
  d <- in_repo(build_deforestation())
  expect_equal(dplyr::n_distinct(d$deforestation$cd_mun), 772)
  expect_false(anyDuplicated(d$deforestation[c("cd_mun", "year")]) > 0)
  total <- function(y) sum(d$deforestation$defor_km2[d$deforestation$year == y])
  expect_equal(total(2020L), 10496, tolerance = 1.5, scale = 1)
  expect_equal(total(2010L), 6279, tolerance = 1.5, scale = 1)
})
