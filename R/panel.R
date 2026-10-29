# Municipality x PRODES-year panel: deforestation, Fund money (treatment) and controls.

# Municipal area and a baseline forest proxy (area minus deforestation up to 2007;
# PRODES does not publish a municipal non-forest mask, so this overstates forest
# in municipalities with much savanna).
mun_attributes <- function() {
  geo <- get_ibge_mun()
  stock <- readr::read_csv("data/derived/stock_2007.csv", col_types = "cd")
  sf::st_drop_geometry(geo) |>
    dplyr::mutate(area_km2 = as.numeric(sf::st_area(geo)) / 1e6) |>
    dplyr::left_join(stock, by = "cd_mun") |>
    dplyr::mutate(forest_km2 = pmax(area_km2 - stock_2007_km2, 0)) |>
    dplyr::select(cd_mun, uf, area_km2, stock_2007_km2, forest_km2)
}

# Split each disbursement across the project's municipalities. `weight` is 1 for
# named municipalities and the overlap share for indigenous lands; `method` scales
# it by nothing (equal), municipal area or baseline forest.
allocate_money <- function(disbursements, links, ipca, mun_attr, method = c("equal", "area", "forest")) {
  method <- match.arg(method)
  scale <- switch(method, equal = 1, area = mun_attr$area_km2, forest = mun_attr$forest_km2)
  shares <- links |>
    dplyr::left_join(tibble::tibble(cd_mun = mun_attr$cd_mun, scale = scale), by = "cd_mun") |>
    dplyr::mutate(w = weight * dplyr::coalesce(scale, 0), share = w / sum(w), .by = slug)
  dplyr::inner_join(disbursements, shares, by = "slug", relationship = "many-to-many") |>
    dplyr::mutate(year = prodes_year(date), money_brl = deflate_to_2025(value_brl * share, year, ipca)) |>
    dplyr::summarise(money_brl = sum(money_brl), .by = c(cd_mun, year))
}

# State-level projects spread over their states' municipalities by baseline forest
# (robustness only; the main specification absorbs them with state x year effects).
allocate_state_money <- function(disbursements, state_projects, ipca, mun_attr) {
  links <- purrr::pmap_dfr(state_projects[c("slug", "local", "abrangencia")], function(slug, local, abrangencia) {
    ufs <- intersect(project_states(local, abrangencia), LEGAL_AMAZON_UF)
    tibble::tibble(slug = slug, cd_mun = mun_attr$cd_mun[mun_attr$uf %in% ufs], weight = 1)
  })
  allocate_money(disbursements, links, ipca, mun_attr, "forest") |>
    dplyr::rename(state_money_brl = money_brl)
}

# Restart exposure: money *contracted* from 2023 (so slow disbursement is not low
# exposure), signed before `cutoff` so it is not chosen after the outcomes it explains.
restart_exposures <- function(projects, links, ipca, mun_attr, cutoff = as.Date("2024-08-01")) {
  contracts <- dplyr::filter(projects, date_contract >= as.Date("2023-01-01"), date_contract < cutoff) |>
    dplyr::transmute(slug, date = date_contract, value_brl = valor_apoio)
  allocate_money(contracts, links, ipca, mun_attr, "equal") |>
    dplyr::summarise(restart_exposure = sum(money_brl) / 1e6, .by = cd_mun)
}

freeze_exposures <- function(money) {
  money |>
    dplyr::summarise(freeze_intensity = mean(money[year %in% 2016:2018]),
                     freeze_exposure = freeze_intensity - mean(money[year %in% 2019:2022]), .by = cd_mun)
}

build_panel <- function() {
  read <- function(f, ...) readr::read_csv(file.path("data/derived", f), show_col_types = FALSE, ...)
  defor <- read("deforestation.csv", col_types = "ccid")
  projects <- read("fund_projects.csv") |> dplyr::left_join(read("fund_coverage.csv"), by = "slug")
  disb <- read("fund_disbursements.csv")
  links <- read("fund_links.csv", col_types = "ccd")
  ipca <- get_ipca()
  mun_attr <- mun_attributes()
  local_slugs <- projects$slug[projects$coverage_type %in% c("municipalities", "indigenous_lands")]
  local_disb <- dplyr::filter(disb, slug %in% local_slugs)
  money_m <- function(method) allocate_money(local_disb, links, ipca, mun_attr, method) |>
    dplyr::mutate(money_brl = money_brl / 1e6)
  local_projects <- dplyr::filter(projects, slug %in% local_slugs)
  uniao <- "Uniao-com-Municipios-pela-Reducao-de-Desmatamento-e-Incendios-Florestais"
  restart <- restart_exposures(local_projects, links, ipca, mun_attr) |>
    dplyr::full_join(dplyr::rename(restart_exposures(local_projects, links, ipca, mun_attr, as.Date("2100-01-01")),
                                   restart_exposure_all = restart_exposure), by = "cd_mun") |>
    dplyr::full_join(dplyr::rename(restart_exposures(dplyr::filter(local_projects, slug != uniao), links, ipca, mun_attr),
                                   restart_exposure_no_uniao = restart_exposure), by = "cd_mun")
  first <- dplyr::filter(projects, slug %in% local_slugs) |>
    dplyr::inner_join(links, by = "slug") |>
    dplyr::filter(!is.na(date_contract)) |>  # approved but not yet contracted: not treated yet
    dplyr::summarise(first_treat_year = min(prodes_year(date_contract)), .by = cd_mun)
  years <- sort(unique(defor$year))
  grid_join <- function(d, col, by = c("cd_mun", "year")) {
    dplyr::left_join(tidyr::expand_grid(cd_mun = unique(defor$cd_mun), year = years), d, by = by) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(col), ~ dplyr::coalesce(.x, 0)))
  }
  money <- grid_join(dplyr::rename(money_m("equal"), money = money_brl), "money")
  panel <- defor |>
    dplyr::left_join(money, by = c("cd_mun", "year")) |>
    dplyr::left_join(grid_join(dplyr::rename(money_m("area"), money_area = money_brl), "money_area"), by = c("cd_mun", "year")) |>
    dplyr::left_join(grid_join(dplyr::rename(money_m("forest"), money_forest = money_brl), "money_forest"), by = c("cd_mun", "year")) |>
    dplyr::left_join(grid_join(dplyr::mutate(allocate_state_money(dplyr::filter(disb, slug %in% projects$slug[projects$coverage_type == "state"]),
                                                                  dplyr::filter(projects, coverage_type == "state"), ipca, mun_attr),
                                             money_state = state_money_brl / 1e6, .keep = "unused"), "money_state"),
                     by = c("cd_mun", "year")) |>
    dplyr::arrange(cd_mun, year) |>
    dplyr::mutate(money_lag1 = dplyr::lag(money, default = 0), money_lag2 = dplyr::lag(money, 2, default = 0),
                  money_area_lag1 = dplyr::lag(money_area, default = 0),
                  money_forest_lag1 = dplyr::lag(money_forest, default = 0),
                  money_state_lag1 = dplyr::lag(money_state, default = 0), .by = cd_mun) |>
    dplyr::left_join(freeze_exposures(money), by = "cd_mun") |>
    dplyr::left_join(restart, by = "cd_mun") |>
    dplyr::left_join(first, by = "cd_mun") |>
    dplyr::left_join(read_priority(years), by = c("cd_mun", "year")) |>
    dplyr::left_join(get_prices(mun_attr, ipca), by = c("cd_mun", "year")) |>
    dplyr::left_join(grid_join(get_fines(), "fines_flora"), by = c("cd_mun", "year")) |>
    dplyr::left_join(dplyr::select(mun_attr, -uf), by = "cd_mun") |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("restart_exposure"), ~ dplyr::coalesce(.x, 0)),
                  y = log(defor_km2 + 0.01),
                  # Sample (spec §5): some deforestation at baseline and a non-negligible forest share.
                  in_sample = mean(defor_km2[year %in% 2008:2012]) > 0 & forest_km2 / area_km2 >= 0.02, .by = cd_mun)
  readr::write_csv(panel, "data/derived/panel.csv")
  message(sprintf("Panel: %d municipalities x %d years; %d in sample; %d ever treated",
                  dplyr::n_distinct(panel$cd_mun), length(years), dplyr::n_distinct(panel$cd_mun[panel$in_sample]),
                  dplyr::n_distinct(panel$cd_mun[!is.na(panel$first_treat_year)])))
  panel
}
