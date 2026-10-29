# Where each Fund project works: classify its free-text territorial coverage and
# match the municipalities it names. data/manual/coverage_overrides.csv corrects
# what the text rules get wrong.

BR_STATES <- c(ACRE = "AC", ALAGOAS = "AL", AMAPA = "AP", AMAZONAS = "AM", BAHIA = "BA", CEARA = "CE",
               "DISTRITO FEDERAL" = "DF", "ESPIRITO SANTO" = "ES", GOIAS = "GO", MARANHAO = "MA",
               "MATO GROSSO" = "MT", "MATO GROSSO DO SUL" = "MS", "MINAS GERAIS" = "MG", PARA = "PA",
               PARAIBA = "PB", PARANA = "PR", PERNAMBUCO = "PE", PIAUI = "PI", "RIO DE JANEIRO" = "RJ",
               "RIO GRANDE DO NORTE" = "RN", "RIO GRANDE DO SUL" = "RS", RONDONIA = "RO", RORAIMA = "RR",
               "SANTA CATARINA" = "SC", "SAO PAULO" = "SP", SERGIPE = "SE", TOCANTINS = "TO")

has_word <- function(text, key) grepl(paste0("\\b", key, "\\b"), text, perl = TRUE)

project_states <- function(local, abrangencia) {
  text <- norm_key(paste(local, abrangencia))
  # "Mato Grosso do Sul" must not also count as "Mato Grosso", nor "Pará" inside "Paraíba".
  found <- names(BR_STATES)[vapply(names(BR_STATES), function(s) has_word(text, s), logical(1))]
  found <- found[!vapply(found, function(s) any(found != s & grepl(s, found, fixed = TRUE)), logical(1))]
  union(unname(BR_STATES[found]), stringr::str_match_all(abrangencia, "\\((AC|AM|AP|MA|MT|PA|RO|RR|TO)\\)")[[1]][, 2])
}

PLACE_PREFIX <- "\\b(RIO|ALTO|MEDIO|BAIXO|TI|TERRA INDIGENA|RESERVA|RESEX|RDS) [A-Z]+( [A-Z]+)?"

match_one <- function(local, abrangencia, munis) {
  raw <- norm_key(abrangencia)
  # Rivers, regions and protected areas often share a municipality's name ("Alto Juruá"),
  # so they are blanked out - except municipalities that themselves start with such a word.
  text <- gsub(PLACE_PREFIX, " ", raw, perl = TRUE)
  ufs <- project_states(local, abrangencia)
  cand <- munis[!(munis$key %in% names(BR_STATES)) & nchar(munis$key) >= 4 &
                  (length(ufs) == 0 | munis$uf %in% ufs), ]
  source_for <- function(k) if (grepl(paste0("^", substring(PLACE_PREFIX, 3)), k, perl = TRUE)) raw else text
  hit <- cand[vapply(cand$key, function(k) has_word(source_for(k), k), logical(1)), ]
  hit <- hit[!duplicated(hit$key) & !(hit$key %in% hit$key[duplicated(hit$key)]), ]  # still ambiguous: skip
  # A name found only inside a longer matched name (Aripuanã in Novo Aripuanã) is dropped;
  # one that is also listed on its own (Grajaú next to Itaipava do Grajaú) is kept.
  standalone <- function(k) {
    longer <- hit$key[hit$key != k & has_word(hit$key, k)]
    rest <- source_for(k)
    for (l in longer) rest <- gsub(paste0("\\b", l, "\\b"), " ", rest, perl = TRUE)
    has_word(rest, k)
  }
  hit[vapply(hit$key, standalone, logical(1)), ]
}

match_municipalities <- function(projects, munis) {
  munis$key <- norm_key(munis$name)
  purrr::pmap_dfr(projects[c("slug", "local", "abrangencia")], function(slug, local, abrangencia) {
    m <- match_one(local, abrangencia, munis)
    tibble::tibble(slug = rep(slug, nrow(m)), cd_mun = m$cd_mun)
  })
}

# Indigenous lands named in the coverage text, overlaid on municipal polygons: a
# municipality's weight is its share of the named lands' total area.
match_indigenous_lands <- function(projects, tis, mun_geo) {
  tis$key <- norm_key(tis$name)
  purrr::pmap_dfr(projects[c("slug", "abrangencia")], function(slug, abrangencia) {
    named <- tis[vapply(tis$key, function(k) nchar(k) >= 3 && has_word(norm_key(abrangencia), k), logical(1)), ]
    if (nrow(named) == 0) return(NULL)
    ov <- suppressWarnings(sf::st_intersection(mun_geo, sf::st_union(named)))
    tibble::tibble(slug = slug, cd_mun = ov$cd_mun, area = as.numeric(sf::st_area(ov))) |>
      dplyr::summarise(area = sum(area), .by = c("slug", "cd_mun")) |>
      dplyr::mutate(weight = area / sum(area), .keep = "unused")
  })
}

# IBGE municipal polygons for the Legal Amazon, in an equal-area projection (SIRGAS 2000 Polyconic).
get_ibge_mun <- function(refresh = FALSE) {
  path <- "data/raw/ibge/municipalities_2022.gpkg"
  if (!refresh && !file.exists(path) && file.exists(file.path(dirname(path), paste0(".", basename(path), ".icloud"))))
    icloud_download(path)
  if (refresh || !file.exists(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    m <- geobr::read_municipality(code_muni = "all", year = 2022, simplified = TRUE, showProgress = FALSE)
    sf::st_write(m, path, quiet = TRUE, delete_dsn = TRUE)
  }
  codes <- prodes_municipalities("data/raw/prodes/prodes_legal_amazon_loinames.json")$cd_mun
  sf::st_read(path, quiet = TRUE) |>
    dplyr::transmute(cd_mun = as.character(code_muni), name = name_muni, uf = abbrev_state) |>
    dplyr::filter(cd_mun %in% codes) |>
    sf::st_make_valid() |>
    sf::st_transform(5880)
}

read_prodes_tis <- function(path = "data/raw/prodes/prodes_legal_amazon_indi.json") {
  sf::st_make_valid(sf::st_transform(sf::st_read(path, quiet = TRUE), 5880))
}

classify_coverage <- function(projects, munis) {
  linked <- unique(match_municipalities(projects, munis)$slug)
  type <- purrr::pmap_chr(projects[c("slug", "local", "abrangencia")], function(slug, local, abrangencia) {
    text <- norm_key(abrangencia)
    ufs <- project_states(local, abrangencia)
    if (slug %in% linked) "municipalities"
    else if (has_word(text, "AMAZONIA LEGAL|BIOMA AMAZONIA|NACIONAL|BRASIL|PAN AMAZ\\w*|AMAZONIA REGIONAL") &&
             !has_word(text, "ESTADO D[AEO]")) "biome_national"
    else if (has_word(text, "TI|TIS|TERRAS? INDIGENAS?")) "indigenous_lands"
    else if (length(ufs) > 0 && all(!ufs %in% LEGAL_AMAZON_UF)) "outside"
    else if (length(ufs) > 0) "state"
    else "biome_national"
  })
  dplyr::mutate(projects, coverage_type = type)
}

apply_overrides <- function(links, projects, overrides) {
  ov <- function(a) overrides[overrides$action == a, ]
  links <- dplyr::anti_join(links, ov("remove"), by = c("slug", "cd_mun")) |>
    dplyr::bind_rows(dplyr::select(ov("add"), slug, cd_mun)) |>
    dplyr::distinct(slug, cd_mun, .keep_all = TRUE)
  retype <- ov("set_type")
  i <- match(projects$slug, retype$slug)
  projects$coverage_type <- dplyr::coalesce(retype$coverage_type[i], projects$coverage_type)
  list(links = links, projects = projects)
}

build_coverage <- function(overrides = TRUE, write = TRUE) {
  projects <- readr::read_csv("data/derived/fund_projects.csv", show_col_types = FALSE)
  munis <- dplyr::transmute(prodes_municipalities("data/raw/prodes/prodes_legal_amazon_loinames.json"),
                            cd_mun, name = prodes_name, uf)
  projects <- classify_coverage(dplyr::filter(projects, !cancelled), munis)
  links <- match_municipalities(projects, munis)
  if (overrides) {
    ov <- readr::read_csv("data/manual/coverage_overrides.csv", col_types = readr::cols(.default = "c"))
    out <- apply_overrides(links, projects, ov)
    projects <- out$projects; links <- out$links
  }
  # Named municipalities share a project's money equally (weight 1 each); projects that
  # name only indigenous lands are allocated by the lands' overlap with each municipality.
  ti_projects <- dplyr::filter(projects, coverage_type == "indigenous_lands")
  links <- dplyr::filter(links, slug %in% projects$slug[projects$coverage_type == "municipalities"]) |>
    dplyr::mutate(weight = 1) |>
    dplyr::bind_rows(match_indigenous_lands(ti_projects, read_prodes_tis(), get_ibge_mun()))
  if (write) {
    readr::write_csv(dplyr::select(projects, slug, coverage_type), "data/derived/fund_coverage.csv")
    readr::write_csv(links, "data/derived/fund_links.csv")
  }
  list(projects = projects, links = links)
}
