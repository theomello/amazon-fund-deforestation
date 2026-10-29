# Control and mechanism variables. Weather is not included: free gridded/API
# sources need far more downloading than the state x year fixed effects justify.

SIDRA <- "https://apisidra.ibge.gov.br/values/t/"
AMAZON_STATES <- "in%20n3%2011,12,13,14,15,16,17,21,51"
CROPS <- c("2713", "2711", "2692", "2708", "2702", "2696")  # soy, maize, rice, cassava, beans, sugarcane

read_sidra <- function(url, path, refresh) {
  d <- jsonlite::fromJSON(cache_download(url, path, refresh))[-1, ]
  tibble::tibble(cd_mun = d$D1C, var = d$D2C, year = as.integer(d$D3C), item = d$D4C,
                 value = suppressWarnings(as.numeric(d$V)))
}

# Monthly IPCA (BCB SGS 433) chained into an index, averaged by PRODES year.
get_ipca <- function(refresh = FALSE) {
  path <- cache_download("https://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados?formato=csv",
                         "data/raw/bcb/ipca_433.csv", refresh)
  readr::read_delim(path, delim = ";", locale = readr::locale(decimal_mark = ","), show_col_types = FALSE) |>
    dplyr::mutate(date = as.Date(data, "%d/%m/%Y"), index = cumprod(1 + valor / 100), year = prodes_year(date)) |>
    dplyr::summarise(index = mean(index), .by = year) |>
    dplyr::filter(year >= 2005)
}

# Deforestation-related enforcement: IBAMA flora infractions (not cancelled).
count_fines <- function(fines) {
  fines |>
    dplyr::filter(TIPO_INFRACAO == "Flora", SIT_CANCELADO != "S") |>
    dplyr::mutate(year = prodes_year(as.Date(substr(DAT_HORA_AUTO_INFRACAO, 1, 10)))) |>
    dplyr::count(cd_mun = as.character(COD_MUNICIPIO), year, name = "fines_flora")
}

get_fines <- function(refresh = FALSE) {
  zip <- cache_download("https://stibamadadosabertosprd.blob.core.windows.net/dados-abertos/dados/SIFISC/auto_infracao/auto_infracao/auto_infracao_csv.zip",
                        "data/raw/ibama/auto_infracao_csv.zip", refresh)
  cols <- c("COD_MUNICIPIO", "DAT_HORA_AUTO_INFRACAO", "TIPO_INFRACAO", "SIT_CANCELADO")
  files <- utils::unzip(zip, list = TRUE)$Name
  purrr::map_dfr(files, function(f) {
    readr::read_delim(unz(zip, f), delim = ";", col_select = dplyr::all_of(cols),
                      col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE)
  }) |> count_fines()
}

# Price indices following Assunção & Rocha (2019): national prices weighted by each
# municipality's 2005-2007 crop area / cattle herd per km², so they vary across
# municipalities and are not absorbed by year fixed effects. Each real price is
# relative to its first three years (the beef series only starts in 2010, so the
# cattle index covers PRODES 2011 onward).
get_prices <- function(mun_attr, ipca, refresh = FALSE) {
  rel <- function(d) dplyr::mutate(d, price = deflate_to_2025(price, year, ipca),
                                   rel_price = price / mean(price[year %in% sort(unique(year))[1:3]]))
  crop_price <- read_sidra(paste0(SIDRA, "1612/n1/all/v/214,215/p/all/c81/", paste(CROPS, collapse = ",")),
                           "data/raw/ibge/pam_1612_national.json", refresh) |>
    tidyr::pivot_wider(names_from = var, values_from = value) |>
    dplyr::transmute(item, year, price = `215` / `214`) |>
    dplyr::group_by(item) |> rel() |> dplyr::ungroup()
  crop_area <- read_sidra(paste0(SIDRA, "1612/n6/", AMAZON_STATES, "/v/216/p/2005,2006,2007/c81/", paste(CROPS, collapse = ",")),
                          "data/raw/ibge/pam_1612_area_2005_2007.json", refresh) |>
    dplyr::summarise(ha = mean(dplyr::coalesce(value, 0)), .by = c(cd_mun, item))
  herd <- read_sidra(paste0(SIDRA, "3939/n6/", AMAZON_STATES, "/v/105/p/2005,2006,2007/c79/2670"),
                     "data/raw/ibge/ppm_3939_cattle_2005_2007.json", refresh) |>
    dplyr::summarise(head = mean(dplyr::coalesce(value, 0)), .by = cd_mun)
  beef <- readr::read_csv(cache_download("https://fred.stlouisfed.org/graph/fredgraph.csv?id=PBEEFUSDM",
                                         "data/raw/fred/PBEEFUSDM.csv", refresh), show_col_types = FALSE)
  fx <- readr::read_delim(cache_download("https://api.bcb.gov.br/dados/serie/bcdata.sgs.3698/dados?formato=csv&dataInicial=01/01/2004&dataFinal=31/12/2030",
                                         "data/raw/bcb/usdbrl_3698.csv", refresh),
                          delim = ";", locale = readr::locale(decimal_mark = ","), show_col_types = FALSE)
  beef_price <- dplyr::inner_join(dplyr::transmute(beef, date = observation_date, usd = PBEEFUSDM),
                                  dplyr::transmute(fx, date = as.Date(data, "%d/%m/%Y"), brl = valor), by = "date") |>
    dplyr::mutate(year = prodes_year(date)) |>
    dplyr::summarise(price = mean(usd * brl), .by = year) |>
    dplyr::filter(year >= 2005) |> rel()
  years <- 2008:max(beef_price$year)
  crops <- tidyr::expand_grid(crop_area, year = years) |>
    dplyr::left_join(dplyr::select(crop_price, item, year, rel_price), by = c("item", "year")) |>
    dplyr::group_by(item) |> tidyr::fill(rel_price) |> dplyr::ungroup() |>  # PAM lags a year: carry forward
    dplyr::left_join(dplyr::select(mun_attr, cd_mun, area_km2), by = "cd_mun") |>
    dplyr::summarise(crop_price_idx = sum(ha / (area_km2 * 100) * rel_price, na.rm = TRUE), .by = c(cd_mun, year))
  cattle <- tidyr::expand_grid(herd, dplyr::select(beef_price, year, rel_price)) |>
    dplyr::left_join(dplyr::select(mun_attr, cd_mun, area_km2), by = "cd_mun") |>
    dplyr::transmute(cd_mun, year, cattle_price_idx = head / area_km2 * rel_price)
  dplyr::full_join(crops, cattle, by = c("cd_mun", "year"))
}
