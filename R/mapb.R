# MapBiomas robustness: do the PRODES results hold with an independent
# deforestation measure? MapBiomas Collection 11 publishes native vegetation loss
# per municipality, biome, year and lost land-cover class (free xlsx, brasil.mapbiomas.org).
# Both measures come from Landsat but use different methods, so agreement is informative
# and disagreement shows how much of a result is measurement.

# Link found on https://brasil.mapbiomas.org/estatisticas/ (section "Deforestation").
MAPB_URL <- "https://brasil.mapbiomas.org/wp-content/uploads/sites/3/2026/10/MAPBIOMAS-BRAZIL-DEFORESTATION-ANNUAL-MUNICIPALITY-COLLECTION_11.xlsx"
MAPB_PATH <- "data/raw/mapbiomas/MAPBIOMAS-BRAZIL-DEFORESTATION-ANNUAL-MUNICIPALITY-COLLECTION_11.xlsx"

download_mapbiomas <- function(refresh = FALSE) cache_download(MAPB_URL, MAPB_PATH, refresh)

# One row per municipality and year, km². Three nested measures (loss of ...):
#   mb_forest_km2   primary forest (forest formation, floodable forest), closest to what PRODES maps
#   mb_primary_km2  primary natural vegetation of any class (adds savanna, grassland, wetland)
#   mb_all_km2      primary and secondary natural vegetation of any class
# Municipalities with no row in a year had no loss (the xlsx lists only nonzero rows' municipalities
# and all 772 panel municipalities appear in it). `year` is the calendar year of the MapBiomas map.
read_mapbiomas <- function(path = MAPB_PATH) {
  raw <- readxl::read_excel(path, sheet = "ANNUAL_DEFORESTATION", col_types = "text")
  ycols <- grep("^y[0-9]{4}$", names(raw), value = TRUE)
  long <- raw |>
    dplyr::transmute(cd_mun = GEOCODE, primary = grepl("Prim", TRANSITION_CLASS_NAME),
                     forest = grepl("^1\\.[15]\\.", CLASS_LEVEL_2), dplyr::across(dplyr::all_of(ycols), as.numeric)) |>
    tidyr::pivot_longer(dplyr::all_of(ycols), names_to = "year", names_prefix = "y", values_to = "ha") |>
    dplyr::mutate(year = as.integer(year), ha = dplyr::coalesce(ha, 0))
  long |>
    dplyr::filter(substr(cd_mun, 1, 2) %in% names(LEGAL_AMAZON_UF), year >= 2008L) |>  # keeps the derived file small
    dplyr::summarise(mb_forest_km2 = sum(ha[primary & forest]) / 100, mb_primary_km2 = sum(ha[primary]) / 100,
                     mb_all_km2 = sum(ha) / 100, .by = c(cd_mun, year))
}

build_mapbiomas <- function(refresh = FALSE) {
  mb <- read_mapbiomas(download_mapbiomas(refresh))
  readr::write_csv(mb, "data/derived/mapbiomas.csv")
  mb
}

# Join MapBiomas to the panel. `shift` = 0 pairs PRODES year t (Aug t-1 to Jul t) with MapBiomas
# year t; shift = 1 pairs it with t-1 (5 of 12 months overlap instead of 7). Outcomes are
# log(km² + 0.01) as for PRODES, so coefficients are comparable.
mapb_panel <- function(panel, mb, shift = 0L) {
  mb <- dplyr::mutate(mb, year = year + shift)
  d <- dplyr::left_join(panel, mb, by = c("cd_mun", "year"))
  d |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c("mb_forest_km2", "mb_primary_km2", "mb_all_km2")),
                                list(y = ~ log(.x + 0.01)), .names = "y_{.col}"),
                  y_mb_forest_asinh = asinh(mb_forest_km2), y_prodes_same = ifelse(is.na(mb_forest_km2), NA, y)) |>
    dplyr::rename_with(~ sub("_km2$", "", .x), dplyr::starts_with("y_mb_") & dplyr::ends_with("_km2"))
}

# Exposure x year event study, as est_exposure() but with a chosen outcome.
mapb_exposure <- function(panel, exposure, ref_year, outcome) {
  d <- sample_of(panel)
  d <- dplyr::mutate(d, priority_base = priority[year == ref_year][1], .by = cd_mun)
  rhs <- sprintf("i(year, %s, ref = %d) + %s + i(year, stock_2007_km2, ref = %d) + i(year, priority_base, ref = %d)",
                 exposure, ref_year, CONTROLS, ref_year, ref_year)
  # fixest drops crop_price_idx as collinear here (as in est_exposure); the message is noise.
  suppressMessages(fe_model(d, rhs, outcome))
}

mapb_event_table <- function(m, label) {
  ct <- fixest::coeftable(m)
  k <- grep("^year::[0-9]+:", rownames(ct))
  k <- k[!grepl("stock|priority_base", rownames(ct)[k])]
  tibble::tibble(outcome = label, year = as.integer(sub("^year::([0-9]+):.*", "\\1", rownames(ct)[k])),
                 estimate = ct[k, 1], std.error = ct[k, 2], p.value = ct[k, 4], n = stats::nobs(m))
}

# Dose-response and restart headline for PRODES and MapBiomas outcomes on the same
# municipality-years (2008-2025, where both exist).
est_mapb <- function(panel, mb) {
  p <- mapb_panel(panel, mb)
  p <- p[p$year <= 2025L, ]
  outcomes <- c("PRODES (same sample)" = "y_prodes_same", "MapBiomas primary forest loss" = "y_mb_forest",
                "MapBiomas primary vegetation loss, any class" = "y_mb_primary",
                "MapBiomas all native vegetation loss" = "y_mb_all", "MapBiomas primary forest loss, asinh" = "y_mb_forest_asinh")
  dose_row <- function(label, m, treatment = "money_lag1") {
    ct <- fixest::coeftable(m)
    tibble::tibble(spec = label, estimate = ct[treatment, 1], std.error = ct[treatment, 2], p.value = ct[treatment, 4],
                   n = stats::nobs(m))
  }
  dose <- dplyr::bind_rows(lapply(names(outcomes), function(l) dose_row(l, est_main(p, outcome = outcomes[[l]]))))
  # Timing: MapBiomas year t-1 against PRODES year t, and a lag-0 treatment.
  p1 <- mapb_panel(panel, mb, shift = 1L); p1 <- p1[p1$year <= 2025L, ]
  dose <- dplyr::bind_rows(dose,
    dose_row("MapBiomas primary forest loss, calendar year t-1", est_main(p1, outcome = "y_mb_forest")),
    dose_row("MapBiomas primary forest loss, no treatment lag", est_main(p, "money", outcome = "y_mb_forest"), "money"),
    dose_row("MapBiomas primary forest loss, 2-year treatment lag", est_main(p, "money_lag2", outcome = "y_mb_forest"), "money_lag2"))
  events <- dplyr::bind_rows(lapply(c(all_contracts = "restart_exposure_all", before_aug2024 = "restart_exposure"), function(x)
    dplyr::bind_rows(lapply(names(outcomes)[1:3], function(l)
      mapb_event_table(mapb_exposure(p, x, 2022L, outcomes[[l]]), l))) ) , .id = "exposure")
  freeze <- dplyr::bind_rows(lapply(names(outcomes)[1:3], function(l)
    mapb_event_table(mapb_exposure(p, "freeze_exposure", 2018L, outcomes[[l]]), l)))
  # Level agreement of the two measures in the estimation sample.
  s <- sample_of(p)
  agree <- tibble::tibble(
    corr_levels = stats::cor(s$defor_km2, s$mb_forest_km2, use = "complete.obs"),
    corr_log = stats::cor(s$y, s$y_mb_forest, use = "complete.obs"),
    corr_within = stats::cor(stats::resid(fixest::feols(y ~ 1 | cd_mun + uf^year, s)),
                             stats::resid(fixest::feols(y_mb_forest ~ 1 | cd_mun + uf^year, s))),
    total_prodes_km2 = sum(s$defor_km2), total_mapbiomas_forest_km2 = sum(s$mb_forest_km2),
    total_mapbiomas_primary_km2 = sum(s$mb_primary_km2))
  out <- list(dose = dose, events = events, freeze = freeze, agreement = agree)
  dir.create("results", showWarnings = FALSE)
  readr::write_csv(dose, "results/mapb_dose_response.csv")
  readr::write_csv(events, "results/mapb_restart_event_study.csv")
  readr::write_csv(freeze, "results/mapb_freeze_event_study.csv")
  readr::write_csv(agree, "results/mapb_agreement.csv")
  out
}

fig_mapb_restart <- function(res) {
  d <- dplyr::filter(res$events, exposure == "all_contracts", outcome != "MapBiomas primary vegetation loss, any class") |>
    dplyr::mutate(pct = 100 * (exp(estimate) - 1), lo = 100 * (exp(estimate - 1.96 * std.error) - 1),
                  hi = 100 * (exp(estimate + 1.96 * std.error) - 1))
  p <- ggplot2::ggplot(d, ggplot2::aes(year, pct, colour = outcome, fill = outcome)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) + ggplot2::geom_vline(xintercept = 2022.5, linetype = 3) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.12, colour = NA) + ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::labs(x = "PRODES year", y = "% change in deforestation per R$ 1 million contracted since 2023 (vs 2022)",
                  colour = NULL, fill = NULL, title = "Restart event study: PRODES and MapBiomas outcomes") +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")
  dir.create("figures", showWarnings = FALSE)
  ggplot2::ggsave("figures/mapb_restart_event_study.png", p, width = 8, height = 4.8, dpi = 150)
  invisible(p)
}
