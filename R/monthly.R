# Municipality x month panel (PRODES years 2017-2026, Aug 2016 to Jul 2026) for timing tests:
# DETER alerts, IBAMA enforcement, a cloud proxy and Fund money. The annual panel stays the main
# analysis; this one asks whether deforestation fell before or after contracts were signed.

MONTHLY_START <- as.Date("2016-08-01")
MONTHLY_END <- as.Date("2026-07-01")
DETER_WFS <- paste0("https://terrabrasilis.dpi.inpe.br/geoserver/ows?service=WFS&version=2.0.0&request=GetFeature",
                    "&typeNames=deter-amz:deter_amz&outputFormat=csv&sortBy=gid&count=50000",
                    "&propertyName=classname,view_date,sensor,areamunkm,mun_geocod")
EMBARGO_URL <- "https://stibamadadosabertosprd.blob.core.windows.net/dados-abertos/dados/TERMOS_DE_EMBARGO/TERMO_EMBARGO/termo_de_embargo.csv"
LEGAL_AMAZON_STATES <- c("AC", "AM", "AP", "MA", "MT", "PA", "RO", "RR", "TO")

# DETER alert classes: clear-cut-type alerts are the outcome closest to PRODES; degradation
# (selective logging, general degradation) and fire scars are kept apart.
DETER_CLASSES <- c(DESMATAMENTO_CR = "clear", DESMATAMENTO_VEG = "clear", MINERACAO = "clear",
                   DEGRADACAO = "degrad", CS_DESORDENADO = "degrad", CS_GEOMETRICO = "degrad",
                   CORTE_SELETIVO = "degrad", CICATRIZ_DE_QUEIMADA = "fire")

# All DETER-Amazon alert attributes since 2016, downloaded in pages (the server caps a response at 50,000).
get_deter <- function(refresh = FALSE) {
  out <- "data/raw/deter/deter_amz_attributes.csv"
  if (!refresh && file.exists(out)) return(readr::read_csv(out, col_types = readr::cols(.default = "c"), progress = FALSE))
  pages <- list()
  repeat {
    i <- length(pages)
    path <- cache_download(paste0(DETER_WFS, "&startIndex=", i * 50000), sprintf("data/raw/deter/pages/p%02d.csv", i), refresh)
    d <- readr::read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE)
    pages[[i + 1]] <- d
    if (nrow(d) < 50000) break
  }
  d <- dplyr::bind_rows(pages)
  readr::write_csv(d, out)
  unlink("data/raw/deter/pages", recursive = TRUE)
  d
}

deter_monthly <- function(deter) {
  deter |>
    dplyr::mutate(month = as.Date(paste0(substr(view_date, 1, 7), "-01")), cd_mun = as.character(mun_geocod),
                  group = unname(DETER_CLASSES[classname]), km2 = as.numeric(areamunkm)) |>
    dplyr::filter(!is.na(group)) |>
    dplyr::summarise(km2 = sum(km2), n = dplyr::n(), .by = c(cd_mun, month, group)) |>
    tidyr::pivot_wider(names_from = group, values_from = c(km2, n), values_fill = 0, names_glue = "deter_{group}_{.value}")
}

# IBAMA embargo terms (one row per polygon, so deduplicate on the term), not cancelled, Legal Amazon.
get_embargoes <- function(refresh = FALSE) {
  path <- cache_download(EMBARGO_URL, "data/raw/ibama_embargo/termo_de_embargo.csv", refresh)
  readr::read_delim(path, delim = ";", col_select = c(SEQ_TAD, COD_MUNICIPIO, UF, DAT_EMBARGO, SIT_CANCELADO),
                    col_types = readr::cols(.default = "c"), progress = FALSE) |>
    dplyr::filter(UF %in% LEGAL_AMAZON_STATES, SIT_CANCELADO != "S") |>
    dplyr::distinct(SEQ_TAD, .keep_all = TRUE)
}

embargoes_monthly <- function(emb) {
  emb |>
    dplyr::mutate(month = as.Date(paste0(substr(DAT_EMBARGO, 1, 7), "-01")), cd_mun = as.character(COD_MUNICIPIO)) |>
    dplyr::filter(!is.na(month), month >= MONTHLY_START, month <= MONTHLY_END) |>
    dplyr::count(cd_mun, month, name = "embargoes")
}

fines_monthly <- function(fines_raw) {
  fines_raw |>
    dplyr::filter(TIPO_INFRACAO == "Flora", SIT_CANCELADO != "S") |>
    dplyr::mutate(month = as.Date(paste0(substr(DAT_HORA_AUTO_INFRACAO, 1, 7), "-01"))) |>
    dplyr::count(cd_mun = as.character(COD_MUNICIPIO), month, name = "fines_flora")
}

# NASA POWER monthly cloud amount (percent, modelled on a ~0.5 degree grid) at each municipality's
# representative point. A proxy: it is not the cloud that blocked DETER's optical sensors.
get_cloud <- function(mun, refresh = FALSE) {
  pts <- sf::st_coordinates(sf::st_transform(sf::st_point_on_surface(sf::st_geometry(mun)), 4326))
  purrr::map_dfr(seq_len(nrow(mun)), function(i) {
    url <- sprintf(paste0("https://power.larc.nasa.gov/api/temporal/monthly/point?parameters=CLOUD_AMT&community=AG",
                          "&longitude=%.3f&latitude=%.3f&start=2016&end=2026&format=JSON"), pts[i, 1], pts[i, 2])
    path <- cache_download(url, sprintf("data/raw/cloud_power/pts/%s.json", mun$cd_mun[i]), refresh, sleep = 0.3)
    v <- jsonlite::fromJSON(path)$properties$parameter$CLOUD_AMT
    v <- v[!grepl("13$", names(v))]  # POWER appends an annual value as month 13
    tibble::tibble(cd_mun = mun$cd_mun[i], month = as.Date(paste0(names(v), "01"), "%Y%m%d"),
                   cloud_amt = ifelse(unlist(v) < -900, NA_real_, unlist(v)))
  })
}

build_monthly <- function(panel, refresh = FALSE) {
  read <- function(f, ...) readr::read_csv(file.path("data/derived", f), show_col_types = FALSE, ...)
  sample <- unique(panel[panel$in_sample, c("cd_mun", "uf")])
  months <- seq(MONTHLY_START, MONTHLY_END, by = "month")
  grid <- tidyr::expand_grid(cd_mun = sample$cd_mun, month = months)

  projects <- read("fund_projects.csv") |> dplyr::left_join(read("fund_coverage.csv"), by = "slug")
  disb <- read("fund_disbursements.csv")
  links <- read("fund_links.csv", col_types = "ccd")
  ipca <- get_ipca(); mun_attr <- mun_attributes()
  local_slugs <- projects$slug[projects$coverage_type %in% c("municipalities", "indigenous_lands")]
  uniao <- "Uniao-com-Municipios-pela-Reducao-de-Desmatamento-e-Incendios-Florestais"
  alloc <- function(d) allocate_money(d, links, ipca, mun_attr, "equal", "month") |> dplyr::mutate(money_brl = money_brl / 1e6)
  paid <- alloc(dplyr::filter(disb, slug %in% local_slugs)) |> dplyr::rename(money_paid = money_brl)
  contracts <- dplyr::filter(projects, slug %in% local_slugs, !is.na(date_contract), !cancelled) |>
    dplyr::transmute(slug, date = date_contract, value_brl = valor_apoio)
  signed <- alloc(contracts) |> dplyr::rename(money_signed = money_brl)
  signed_uniao <- alloc(dplyr::filter(contracts, slug == uniao)) |> dplyr::rename(money_signed_uniao = money_brl)
  first_contract <- dplyr::inner_join(contracts, links, by = "slug") |>
    dplyr::summarise(first_contract_month = as.Date(format(min(date), "%Y-%m-01")), .by = cd_mun)

  fines_zip <- cache_download("https://stibamadadosabertosprd.blob.core.windows.net/dados-abertos/dados/SIFISC/auto_infracao/auto_infracao/auto_infracao_csv.zip",
                              "data/raw/ibama/auto_infracao_csv.zip", refresh)
  fines_raw <- purrr::map_dfr(utils::unzip(fines_zip, list = TRUE)$Name, function(f)
    readr::read_delim(unz(fines_zip, f), delim = ";", col_select = dplyr::all_of(c("COD_MUNICIPIO", "DAT_HORA_AUTO_INFRACAO", "TIPO_INFRACAO", "SIT_CANCELADO")),
                      col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE))
  mun_sf <- get_ibge_mun()
  mun_sf <- mun_sf[mun_sf$cd_mun %in% sample$cd_mun, ]

  zero <- function(d, cols) dplyr::mutate(d, dplyr::across(dplyr::all_of(cols), ~ dplyr::coalesce(.x, 0)))
  dm <- deter_monthly(get_deter(refresh))
  m <- grid |>
    dplyr::left_join(dm, by = c("cd_mun", "month")) |>
    dplyr::left_join(embargoes_monthly(get_embargoes(refresh)), by = c("cd_mun", "month")) |>
    dplyr::left_join(fines_monthly(fines_raw), by = c("cd_mun", "month")) |>
    dplyr::left_join(paid, by = c("cd_mun", "month")) |>
    dplyr::left_join(signed, by = c("cd_mun", "month")) |>
    dplyr::left_join(signed_uniao, by = c("cd_mun", "month")) |>
    dplyr::left_join(get_cloud(mun_sf, refresh), by = c("cd_mun", "month")) |>
    dplyr::left_join(first_contract, by = "cd_mun")
  num0 <- c(grep("^deter_", names(m), value = TRUE), "embargoes", "fines_flora", "money_paid", "money_signed", "money_signed_uniao")
  m <- zero(m, num0) |>
    dplyr::mutate(year = prodes_year(month), uf = sample$uf[match(cd_mun, sample$cd_mun)],
                  months_since_first_contract = ifelse(is.na(first_contract_month), NA_integer_,
                    (as.integer(format(month, "%Y")) - as.integer(format(first_contract_month, "%Y"))) * 12L +
                      as.integer(format(month, "%m")) - as.integer(format(first_contract_month, "%m")))) |>
    dplyr::left_join(dplyr::select(panel, cd_mun, year, priority) |> dplyr::distinct(), by = c("cd_mun", "year")) |>
    dplyr::arrange(cd_mun, month)
  readr::write_csv(m, "data/derived/monthly_panel.csv")
  message(sprintf("Monthly panel: %d municipalities x %d months", dplyr::n_distinct(m$cd_mun), length(months)))
  m
}
