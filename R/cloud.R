# Check: PRODES cloud / unobserved area. INPE does not publish cloud cover per municipality
# (see docs/cloud.md for every source tried). The closest public quantity is the PRODES
# "residual" layer: forest cleared in an earlier year but hidden under cloud (or otherwise
# unobserved) until it was seen later. Residual area in a municipality is therefore a proxy
# for how much of it was unobserved before. This file builds that proxy from the TerraBrasilis
# WFS, adds it as a control and as a sample restriction, and re-estimates the headline
# specifications. Nothing here edits the existing estimators.

CLOUD_WFS <- paste0("https://terrabrasilis.dpi.inpe.br/geoserver/ows?service=WFS&version=2.0.0&request=GetFeature",
                    "&typeNames=prodes-legal-amz:residual&outputFormat=application/json&sortBy=fid",
                    "&propertyName=geom,class_name,def_cloud,year,area_km,state")
CLOUD_PATH <- "data/raw/prodes/prodes_residual_by_municipality.csv"
CLOUD_PAGE <- 10000L

# Residual polygons -> km2 per municipality and residual year. Each polygon is assigned to the
# municipality containing a point inside it (polygons are small; a boundary-straddling polygon
# would be split in a strict overlay, which changes totals by well under 1 %).
residual_by_municipality <- function(pages, geo) {
  r <- purrr::map_dfr(pages, function(p) {
    s <- sf::read_sf(p, quiet = TRUE)
    s <- s[!sf::st_is_empty(s), ]
    pts <- suppressWarnings(sf::st_point_on_surface(sf::st_set_crs(sf::st_zm(s), 4674)))
    sf::st_sf(year = as.integer(s$year), area_km = s$area_km, geometry = sf::st_geometry(pts))
  })
  r <- sf::st_transform(r, sf::st_crs(geo))
  hit <- sf::st_intersects(r, geo)
  # A point on a boundary can hit two municipalities; keep the first.
  r$cd_mun <- geo$cd_mun[vapply(hit, function(h) if (length(h)) h[1] else NA_integer_, integer(1))]
  sf::st_drop_geometry(r) |>
    dplyr::filter(!is.na(cd_mun)) |>
    dplyr::summarise(residual_km2 = sum(area_km), n_polygons = dplyr::n(), .by = c(cd_mun, year))
}

# Download the layer in pages (the server caps one answer at 50,000 features), aggregate, cache the
# small result. The raw pages (about 80 MB) are not kept.
get_residual <- function(refresh = FALSE) {
  if (file.exists(CLOUD_PATH) && !refresh) return(readr::read_csv(CLOUD_PATH, col_types = "cidi"))
  dir <- tempfile("residual"); dir.create(dir); on.exit(unlink(dir, recursive = TRUE))
  hits <- httr2::request(paste0(sub("request=GetFeature", "request=GetFeature&resultType=hits", CLOUD_WFS))) |>
    httr2::req_perform() |> httr2::resp_body_string()
  n <- as.integer(sub('.*numberMatched="([0-9]+)".*', "\\1", hits))
  pages <- vapply(seq(0, n - 1, by = CLOUD_PAGE), function(i)
    cache_download(sprintf("%s&count=%d&startIndex=%d", CLOUD_WFS, CLOUD_PAGE, i), file.path(dir, sprintf("p%06d.json", i))), "")
  out <- residual_by_municipality(pages, get_ibge_mun())
  stopifnot(abs(sum(out$residual_km2) - sum(purrr::map_dbl(pages, function(p) sum(sf::read_sf(p, quiet = TRUE)$area_km)))) /
              sum(out$residual_km2) < 0.01)
  dir.create(dirname(CLOUD_PATH), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(out, CLOUD_PATH)
  out
}

# Panel plus the unobserved-area proxies. Residual detected in PRODES year t is a *later* signal
# about earlier cloud, so three versions are built:
#   cloud_resid       residual of the same PRODES year, share of municipal area (percent)
#   cloud_resid_lag1  residual of the previous year
#   cloud_resid_avg   municipality mean over all years (time-invariant exposure to cloud)
# Each is residual km2 / municipal area km2 x 100.
cloud_panel <- function(panel, resid) {
  d <- dplyr::left_join(panel, dplyr::select(resid, cd_mun, year, residual_km2), by = c("cd_mun", "year")) |>
    dplyr::mutate(residual_km2 = dplyr::coalesce(residual_km2, 0), cloud_resid = 100 * residual_km2 / area_km2) |>
    dplyr::arrange(cd_mun, year) |>
    dplyr::mutate(cloud_resid_lag1 = dplyr::lag(cloud_resid, default = 0),
                  cloud_resid_avg = mean(cloud_resid[year <= 2022]), .by = cd_mun)
  d
}

# Sample restriction: drop municipality-years whose proxy is above the `q` quantile of the
# estimation sample (strictly positive values only), or municipalities whose mean is.
cloud_restrict <- function(d, var = "cloud_resid", q = 0.95, by = c("year", "municipality")) {
  by <- match.arg(by)
  s <- d[d$in_sample, ]
  if (by == "year") {
    cut <- stats::quantile(s[[var]][s[[var]] > 0], q, na.rm = TRUE)
    d$in_sample <- d$in_sample & !(d[[var]] > cut)
  } else {
    m <- tapply(s[[var]], s$cd_mun, mean)
    cut <- stats::quantile(m[m > 0], q, na.rm = TRUE)
    d$in_sample <- d$in_sample & !(d$cd_mun %in% names(m)[m > cut])
  }
  attr(d, "cut") <- unname(cut)
  d
}

# Exposure x year event study with extra right-hand-side terms (the controls), as est_exposure().
cloud_exposure <- function(panel, exposure, ref_year, extra = NULL) {
  d <- sample_of(panel)
  d <- dplyr::mutate(d, priority_base = priority[year == ref_year][1], .by = cd_mun)
  rhs <- paste(c(sprintf("i(year, %s, ref = %d)", exposure, ref_year), CONTROLS, extra,
                 sprintf("i(year, stock_2007_km2, ref = %d)", ref_year), sprintf("i(year, priority_base, ref = %d)", ref_year)),
               collapse = " + ")
  suppressMessages(fe_model(d, rhs))
}

cloud_event_table <- function(m, label, exposure) {
  ct <- fixest::coeftable(m)
  k <- grep(sprintf("^year::[0-9]+:%s$", exposure), rownames(ct))
  tibble::tibble(spec = label, year = as.integer(sub("^year::([0-9]+):.*", "\\1", rownames(ct)[k])),
                 estimate = ct[k, 1], std.error = ct[k, 2], p.value = ct[k, 4], n = stats::nobs(m))
}

est_cloud <- function(panel, resid = get_residual()) {
  p <- cloud_panel(panel, resid)
  dose_row <- function(label, m, treatment = "money_lag1") {
    ct <- fixest::coeftable(m)
    tibble::tibble(spec = label, estimate = ct[treatment, 1], std.error = ct[treatment, 2], p.value = ct[treatment, 4], n = stats::nobs(m))
  }
  # Specifications: baseline; control (same year, lagged, both with municipality mean absorbed by the FE);
  # control interacted with year (cloudy municipalities may trend differently); restrictions.
  r95 <- cloud_restrict(p, "cloud_resid", 0.95, "year")
  r90m <- cloud_restrict(p, "cloud_resid_avg", 0.90, "municipality")
  r75 <- cloud_restrict(p, "cloud_resid", 0.75, "year")
  yrs <- "i(year, cloud_resid_avg)"
  dose <- dplyr::bind_rows(
    dose_row("Baseline", est_main(p)),
    dose_row("Control: residual share, same year", est_main(p, extra = "cloud_resid")),
    dose_row("Control: residual share, previous year", est_main(p, extra = "cloud_resid_lag1")),
    dose_row("Control: mean residual share x year", est_main(p, extra = yrs)),
    dose_row("Drop municipality-years above 95th pct of residual share", est_main(r95)),
    dose_row("Drop municipality-years above 75th pct of residual share", est_main(r75)),
    dose_row("Drop 10 % municipalities with highest mean residual share", est_main(r90m)))
  specs <- list(baseline = list(p, NULL), control_same_year = list(p, "cloud_resid"),
                control_mean_x_year = list(p, yrs), drop_years_p95 = list(r95, NULL), drop_years_p75 = list(r75, NULL),
                drop_municipalities_p90 = list(r90m, NULL))
  events <- dplyr::bind_rows(lapply(c(all_contracts = "restart_exposure_all", before_aug2024 = "restart_exposure"), function(x)
    dplyr::bind_rows(lapply(names(specs), function(l) cloud_event_table(cloud_exposure(specs[[l]][[1]], x, 2022L, specs[[l]][[2]]), l, x)))), .id = "exposure")
  s <- sample_of(p)
  desc <- tibble::tibble(
    total_residual_km2 = sum(resid$residual_km2), total_prodes_km2 = sum(s$defor_km2), sample_residual_km2 = sum(s$residual_km2),
    share_zero = mean(s$residual_km2 == 0), p95_share_pct = unname(stats::quantile(s$cloud_resid[s$cloud_resid > 0], 0.95)),
    n_dropped_p95 = sum(s$cloud_resid > attr(r95, "cut")), cut_p95 = attr(r95, "cut"),
    n_dropped_p75 = sum(s$cloud_resid > attr(r75, "cut")), n_municipalities_dropped_p90 = dplyr::n_distinct(s$cd_mun) - dplyr::n_distinct(sample_of(r90m)$cd_mun),
    corr_resid_defor = stats::cor(s$cloud_resid, s$defor_km2 / s$area_km2), corr_within = stats::cor(
      stats::resid(fixest::feols(cloud_resid ~ 1 | cd_mun + uf^year, s)), stats::resid(fixest::feols(y ~ 1 | cd_mun + uf^year, s))))
  out <- list(dose = dose, events = events, desc = desc)
  dir.create("results", showWarnings = FALSE)
  readr::write_csv(dose, "results/cloud_dose_response.csv")
  readr::write_csv(events, "results/cloud_restart_event_study.csv")
  readr::write_csv(desc, "results/cloud_description.csv")
  out
}
