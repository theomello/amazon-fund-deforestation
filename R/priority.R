# Priority-municipality blacklist (municípios prioritários), 2008-2022, from the
# compilation of MMA portarias by Seixas (Zenodo, doi:10.5281/zenodo.8412144, MIT).
# Later lists (Portarias MMA 834/2023 and 1.202/2024) are not freely published in
# machine-readable form, so the 2022 status is carried forward.

PRIORITY_URL <- "https://zenodo.org/records/8412144/files/priority_list.csv?download=1"

read_priority <- function(years = 2008:2026, refresh = FALSE) {
  path <- cache_download(PRIORITY_URL, "data/raw/priority/priority_list_zenodo_8412144.csv", refresh)
  listed <- readr::read_csv(path, col_types = readr::cols(.default = "c")) |>
    dplyr::filter(biome == "Amazônia", priority == "1") |>
    dplyr::transmute(cd_mun = code_muni, year = as.integer(year))
  last <- max(listed$year)
  listed <- dplyr::bind_rows(listed, tidyr::expand_grid(cd_mun = listed$cd_mun[listed$year == last],
                                                        year = setdiff(years, listed$year)[setdiff(years, listed$year) > last]))
  munis <- prodes_municipalities("data/raw/prodes/prodes_legal_amazon_loinames.json")$cd_mun
  tidyr::expand_grid(cd_mun = munis, year = years) |>
    dplyr::mutate(priority = as.integer(paste(cd_mun, year) %in% paste(listed$cd_mun, listed$year)))
}
