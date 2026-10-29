# PRODES annual deforestation per municipality, from the JSON files behind the
# TerraBrasilis dashboard (https://terrabrasilis.dpi.inpe.br).

PRODES_URL <- "https://terrabrasilis.dpi.inpe.br/app/prodes/dashboard/deforestation/files/"
# IBGE state codes of the Legal Amazon. The PRODES grid also touches 33
# municipalities in Piauí and Bahia, which are not part of the Legal Amazon.
LEGAL_AMAZON_UF <- c("11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA",
                     "16" = "AP", "17" = "TO", "21" = "MA", "51" = "MT")

download_prodes <- function(refresh = FALSE) {
  dir <- "data/raw/prodes"
  get <- function(remote, local) cache_download(paste0(PRODES_URL, remote), file.path(dir, local), refresh)
  list(data  = get("data/prodes_legal_amazon.json", "prodes_legal_amazon_data.json"),
       names = get("config/loinames/prodes_legal_amazon.json", "prodes_legal_amazon_loinames.json"),
       mun   = get("config/mun/prodes_legal_amazon.json", "prodes_legal_amazon_mun.json"))
}

# Municipality names carry their IBGE code (`codibge`), so no name matching is needed.
prodes_municipalities <- function(names_path) {
  lois <- jsonlite::fromJSON(names_path)$lois
  m <- lois$loinames[[which(lois$name == "mun")]]
  tibble::tibble(gid = m$gid, prodes_name = m$loiname, cd_mun = as.character(m$codibge),
                 uf = unname(LEGAL_AMAZON_UF[substr(as.character(m$codibge), 1, 2)])) |>
    dplyr::filter(!is.na(uf))
}

# One row per municipality polygon area and period; `start` 1500 marks the stock up to 2007.
prodes_features <- function(data_path) {
  periods <- jsonlite::fromJSON(data_path, simplifyVector = FALSE)$periods
  purrr::map_dfr(periods, function(p) {
    mun <- Filter(function(f) f$loi == 2, p$features)
    tibble::tibble(start = p$startDate$year, year = as.integer(p$endDate$year),
                   gid = vapply(mun, function(f) f$loiname, numeric(1)),
                   area = vapply(mun, function(f) sum(vapply(f$areas, function(a) a$area, numeric(1))), numeric(1)))
  })
}

read_prodes <- function(paths) {
  munis <- prodes_municipalities(paths$names)
  f <- dplyr::filter(prodes_features(paths$data), start != 1500)
  tidyr::expand_grid(munis, year = sort(unique(f$year))) |>
    dplyr::left_join(dplyr::summarise(f, defor_km2 = sum(area), .by = c(gid, year)), by = c("gid", "year")) |>
    dplyr::mutate(defor_km2 = dplyr::coalesce(defor_km2, 0))
}

read_prodes_stock <- function(paths) {
  stock <- dplyr::filter(prodes_features(paths$data), start == 1500) |>
    dplyr::summarise(stock_2007_km2 = sum(area), .by = gid)
  prodes_municipalities(paths$names) |>
    dplyr::left_join(stock, by = "gid") |>
    dplyr::transmute(cd_mun, stock_2007_km2 = dplyr::coalesce(stock_2007_km2, 0))
}

build_deforestation <- function(refresh = FALSE) {
  paths <- download_prodes(refresh)
  out <- list(deforestation = dplyr::select(read_prodes(paths), cd_mun, uf, year, defor_km2),
              stock = read_prodes_stock(paths))
  dir.create("data/derived", showWarnings = FALSE)
  readr::write_csv(out$deforestation, "data/derived/deforestation.csv")
  readr::write_csv(out$stock, "data/derived/stock_2007.csv")
  out
}
