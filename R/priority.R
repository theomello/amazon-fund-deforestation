# Priority-municipality blacklist (municípios prioritários), 2008-2022, from the
# compilation of MMA portarias by Seixas (Zenodo, doi:10.5281/zenodo.8412144, MIT).
# Later lists (Portarias MMA 834/2023 and 1.202/2024) are not freely published in
# machine-readable form; hand-transcribe them into data/manual/priority_lists.csv
# (cd_mun, name_muni, uf, list_year, portaria, portaria_date, source_url). The last
# available list is carried forward.
#
# Timing: Seixas codes each list by the calendar year of its portaria, but most lists
# come out in Sep-Nov and are based on that year's PRODES figures. A municipality
# therefore counts as listed in PRODES year t (Aug t-1 to Jul t) if it was listed at
# the end of calendar year t-1, or added by a portaria dated before 1 August t, so
# that blacklist status never depends on the same year's deforestation.

PRIORITY_URL <- "https://zenodo.org/records/8412144/files/priority_list.csv?download=1"
PRIORITY_MANUAL <- "data/manual/priority_lists.csv"

read_priority <- function(years = 2008:2026, refresh = FALSE) {
  path <- cache_download(PRIORITY_URL, "data/raw/priority/priority_list_zenodo_8412144.csv", refresh)
  listed <- readr::read_csv(path, col_types = readr::cols(.default = "c")) |>
    dplyr::filter(biome == "Amazônia", priority == "1") |>
    dplyr::transmute(cd_mun = code_muni, list_year = as.integer(year),
                     added = as.Date(sub(".* de ([0-9/]+)$", "\\1", portaria), "%d/%m/%Y"))
  if (file.exists(PRIORITY_MANUAL)) {
    manual <- readr::read_csv(PRIORITY_MANUAL, col_types = readr::cols(.default = "c")) |>
      dplyr::transmute(cd_mun, list_year = as.integer(list_year), added = as.Date(portaria_date))
    listed <- dplyr::bind_rows(dplyr::filter(listed, !list_year %in% manual$list_year), manual)
  }
  last <- max(listed$list_year)
  carried <- tidyr::expand_grid(cd_mun = listed$cd_mun[listed$list_year == last], list_year = (last + 1L):max(years))
  listed <- dplyr::bind_rows(listed, carried)
  status <- function(t) {
    early <- listed$list_year == t & !is.na(listed$added) & listed$added < as.Date(sprintf("%d-08-01", t))
    unique(listed$cd_mun[listed$list_year == t - 1L | early])
  }
  munis <- prodes_municipalities("data/raw/prodes/prodes_legal_amazon_loinames.json")$cd_mun
  dplyr::bind_rows(lapply(years, function(t) tibble::tibble(cd_mun = munis, year = t,
                                                            priority = as.integer(munis %in% status(t)))))
}
