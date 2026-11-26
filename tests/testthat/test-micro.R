test_that("the wild cluster bootstrap agrees with the analytic cluster test on well-behaved data", {
  set.seed(3)
  n <- 120; sim <- tidyr::expand_grid(cd_mun = sprintf("%07d", seq_len(n)), year = 2008:2022) |>
    dplyr::mutate(uf = rep(c("PA", "MT", "AM"), length.out = dplyr::n()), alpha = rnorm(n)[as.integer(factor(cd_mun))],
                  money_lag1 = rexp(dplyr::n()) * (as.integer(factor(cd_mun)) %% 3 == 0 & year > 2014),
                  priority = rbinom(dplyr::n(), 1, 0.1), crop_price_idx = runif(dplyr::n()),
                  y = alpha - 0.05 * money_lag1 + rnorm(dplyr::n(), sd = 0.3), in_sample = TRUE,
                  g = (as.integer(factor(cd_mun)) - 1) %% 30 + 1)
  m <- est_main(sim)
  w <- wild_cluster_p(m, "money_lag1", sim$g, B = 399, weights = "rademacher")
  analytic <- fixest::coeftable(m, vcov = ~g)["money_lag1", 4]
  expect_equal(w$G, 30L)
  expect_lt(abs(w$p - analytic), 0.1)
  expect_lt(w$p, 0.05)  # the simulated effect is -0.05 with small noise
})

test_that("microregion mapping covers every Legal Amazon sample municipality", {
  skip_if_not(file.exists(file.path(repo_root, "data/derived/micro_microregions.csv")) && has_raw("data/derived/panel.csv"))
  in_repo({
    mr <- readr::read_csv("data/derived/micro_microregions.csv", col_types = "ccc")
    panel <- readr::read_csv("data/derived/panel.csv", show_col_types = FALSE, col_types = readr::cols(cd_mun = "c"))
    ids <- unique(panel$cd_mun[panel$in_sample])
    expect_true(all(ids %in% mr$cd_mun))
    expect_false(anyDuplicated(mr$cd_mun) > 0)
  })
})
