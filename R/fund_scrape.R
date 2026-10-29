# Amazon Fund projects and their dated disbursements, scraped from the Fund's
# project pages (the only public source that lists where each project works).

FUND_URL <- "https://www.fundoamazonia.gov.br"
FUND_DIR <- "data/raw/fundo_amazonia"

# Follows the paginated listing until a page has no project links; 1 request/second.
scrape_listing <- function(first_url, dir, refresh) {
  slugs <- character()
  for (page in 1:100) {
    url <- if (page == 1) first_url else paste0(first_url, "index.html?reloaded&page=", page)
    html <- readLines(cache_download(url, file.path(dir, paste0("p", page, ".html")), refresh, sleep = 1),
                      warn = FALSE, encoding = "UTF-8")
    found <- unique(stringr::str_match(html, 'href="(/pt/projeto/[^"]+)"')[, 2])
    found <- found[!is.na(found)]
    if (length(found) == 0) break
    slugs <- union(slugs, found)
  }
  slugs
}

# Cancelled projects are listed on a page rendered by JavaScript, so they are not
# scraped; they received no money, and `cancelled` is kept for the record format.
scrape_fund <- function(refresh = FALSE) {
  page_of <- function(slug) file.path(FUND_DIR, "pages", paste0(basename(slug), ".html"))
  slugs <- scrape_listing(paste0(FUND_URL, "/pt/projetos-apoiados/"), file.path(FUND_DIR, "listing"), refresh)
  for (s in slugs) cache_download(paste0(FUND_URL, s), page_of(s), refresh, sleep = 1)
  tibble::tibble(path = page_of(slugs), cancelled = FALSE)
}

page_lines <- function(path) {
  doc <- xml2::read_html(path, encoding = "UTF-8")
  xml2::xml_remove(xml2::xml_find_all(doc, "//script|//style"))
  lines <- trimws(unlist(strsplit(xml2::xml_text(xml2::xml_find_all(doc, "//body//text()")), "\n")))
  lines[lines != ""]
}

# The value printed on the line after a label ("" if the label is absent).
after_label <- function(lines, label) {
  i <- match(label, lines)
  if (is.na(i) || i == length(lines)) "" else lines[i + 1]
}

dmy <- function(x) as.Date(stringr::str_extract(x, "\\d{2}\\.\\d{2}\\.\\d{4}"), format = "%d.%m.%Y")

parse_project <- function(path) {
  L <- page_lines(path)
  slug <- sub("\\.html$", "", basename(path))
  # Territorial coverage is free text that runs until the next section heading.
  cov <- ""
  i <- match("Abrangência territorial", L)
  if (!is.na(i)) {
    end <- which(L %in% c("Descrição", "Beneficiários", "Objetivos") & seq_along(L) > i)[1]
    cov <- paste(L[(i + 1):(dplyr::coalesce(end, length(L) + 1L) - 1)], collapse = " ")
  }
  d <- which(grepl("^\\d+º desembolso$", L) & seq_along(L) + 2 <= length(L))
  list(
    project = tibble::tibble(
      slug, valor_total = parse_brl(after_label(L, "Valor Total do Projeto")),
      valor_apoio = parse_brl(after_label(L, "Valor do apoio do Fundo Amazônia")),
      natureza = after_label(L, "Natureza do Responsável"), temas = after_label(L, "Temas"),
      local = after_label(L, "Local"), eixos = after_label(L, "Eixos"),
      date_approval = dmy(after_label(L, "Data da aprovação")),
      date_contract = dmy(after_label(L, "Data da contratação")),
      date_completion = dmy(after_label(L, "Data da conclusão")),
      abrangencia = cov),
    disbursements = tibble::tibble(slug = rep(slug, length(d)), n = as.integer(sub("º.*", "", L[d])),
                                   date = dmy(L[d + 1]), value_brl = parse_brl(L[d + 2])))
}

read_bndes_fund <- function(path = "data/raw/bndes/operacoes-nao-automaticas.csv") {
  readr::read_delim(path, delim = ";", locale = readr::locale(encoding = "windows-1252", decimal_mark = ","),
                    col_types = readr::cols(.default = "c"), progress = FALSE) |>
    dplyr::filter(fonte_de_recurso_desembolsos == "RECURSOS VINCULADOS - FUNDO AMAZÔNIA")
}

build_fund <- function(refresh = FALSE) {
  pages <- scrape_fund(refresh)
  parsed <- lapply(pages$path, parse_project)
  projects <- dplyr::bind_rows(lapply(parsed, `[[`, "project")) |> dplyr::mutate(cancelled = pages$cancelled)
  disbursements <- dplyr::bind_rows(lapply(parsed, `[[`, "disbursements"))
  bndes <- read_bndes_fund()
  message(sprintf("Fund: %d active projects (BNDES lists %d contracts); R$ %.0f mi disbursed",
                  sum(!projects$cancelled), nrow(bndes),
                  sum(disbursements$value_brl[disbursements$slug %in% projects$slug[!projects$cancelled]]) / 1e6))
  dir.create("data/derived", showWarnings = FALSE)
  readr::write_csv(projects, "data/derived/fund_projects.csv")
  readr::write_csv(disbursements, "data/derived/fund_disbursements.csv")
  list(projects = projects, disbursements = disbursements)
}
