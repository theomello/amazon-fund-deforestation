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

match_one <- function(local, abrangencia, munis) {
  text <- norm_key(abrangencia)
  # Rivers, regions and indigenous lands often share a municipality's name ("Alto Juruá").
  text <- gsub("\\b(RIO|ALTO|MEDIO|BAIXO|TI|TERRA INDIGENA|RESERVA|RESEX|RDS) [A-Z]+( [A-Z]+)?", " ", text, perl = TRUE)
  ufs <- project_states(local, abrangencia)
  cand <- munis[!(munis$key %in% names(BR_STATES)) & nchar(munis$key) >= 4 &
                  (length(ufs) == 0 | munis$uf %in% ufs), ]
  hit <- cand[vapply(cand$key, function(k) has_word(text, k), logical(1)), ]
  hit <- hit[!duplicated(hit$key) & !(hit$key %in% hit$key[duplicated(hit$key)]), ]  # still ambiguous: skip
  hit[!vapply(hit$key, function(k) any(hit$key != k & has_word(hit$key, k)), logical(1)), ]
}

match_municipalities <- function(projects, munis) {
  munis$key <- norm_key(munis$name)
  purrr::pmap_dfr(projects[c("slug", "local", "abrangencia")], function(slug, local, abrangencia) {
    m <- match_one(local, abrangencia, munis)
    tibble::tibble(slug = rep(slug, nrow(m)), cd_mun = m$cd_mun)
  })
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
    dplyr::distinct(slug, cd_mun)
  retype <- ov("set_type")
  i <- match(projects$slug, retype$slug)
  projects$coverage_type <- dplyr::coalesce(retype$coverage_type[i], projects$coverage_type)
  list(links = links, projects = projects)
}

build_coverage <- function(overrides = TRUE) {
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
  links <- dplyr::semi_join(links, dplyr::filter(projects, coverage_type %in% c("municipalities", "indigenous_lands")),
                            by = "slug")
  readr::write_csv(dplyr::select(projects, slug, coverage_type), "data/derived/fund_coverage.csv")
  readr::write_csv(links, "data/derived/fund_links.csv")
  list(projects = projects, links = links)
}
