# Shared helpers used by every pipeline step.

# Download `url` to `path` unless a cached copy exists. Cached raw files make the
# build reproducible even if the source websites change.
cache_download <- function(url, path, refresh = FALSE, sleep = 0) {
  if (file.exists(path) && !refresh) return(path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  resp <- httr2::req_perform(httr2::req_user_agent(httr2::request(url), "amazon-fund-deforestation research"),
                             path = path)
  if (httr2::resp_status(resp) != 200) stop("Download failed (", httr2::resp_status(resp), "): ", url)
  if (sleep > 0) Sys.sleep(sleep)
  path
}

# PRODES year t runs from 1 August of t-1 to 31 July of t.
prodes_year <- function(date) {
  as.integer(format(date, "%Y")) + (as.integer(format(date, "%m")) >= 8L)
}

# "R$ 1.800.104,08", "-R$ 267,95" or "R$ -267,95" -> numeric; anything else -> NA.
parse_brl <- function(x) {
  m <- stringr::str_match(x, "(-?)\\s*R\\$\\s*(-?)\\s*([0-9.]+,[0-9]{2})")
  value <- as.numeric(gsub(",", ".", gsub(".", "", m[, 4], fixed = TRUE), fixed = TRUE))
  ifelse(m[, 2] == "-" | m[, 3] == "-", -value, value)
}

# Matching key for place names: uppercase, no accents, no punctuation.
norm_key <- function(x) {
  x <- toupper(stringi::stri_trans_general(x, "Latin-ASCII"))
  x <- gsub("['’`-]", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# Express `value` from `year` in 2025 reais; `ipca` has columns year and index.
deflate_to_2025 <- function(value, year, ipca) {
  value * ipca$index[ipca$year == 2025L] / ipca$index[match(year, ipca$year)]
}
