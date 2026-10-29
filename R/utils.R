# Shared helpers used by every pipeline step.

# macOS iCloud ("Optimize Mac Storage") can offload a cached file, leaving only
# a ".<name>.icloud" placeholder; ask iCloud for the file back and wait for it.
icloud_download <- function(path) {
  system2("brctl", c("download", shQuote(path)))
  for (i in 1:60) if (file.exists(path)) return(invisible(path)) else Sys.sleep(1)
  stop("iCloud did not restore ", path)
}

# Download `url` to `path` unless a cached copy exists. Cached raw files make the
# build reproducible even if the source websites change.
cache_download <- function(url, path, refresh = FALSE, sleep = 0, materialize = icloud_download) {
  placeholder <- file.path(dirname(path), paste0(".", basename(path), ".icloud"))
  if (!refresh && !file.exists(path) && file.exists(placeholder)) materialize(path)
  if (file.exists(path) && !refresh) return(path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  req <- httr2::request(url) |>  # httr2's default user agent (some servers reject custom ones)
    httr2::req_options(http_version = 2L) |>  # HTTP/1.1: some servers drop HTTP/2 streams
    httr2::req_retry(max_tries = 3)
  # Write to a temporary file first, so a failed download never looks cached.
  part <- paste0(path, ".part")
  on.exit(unlink(part), add = TRUE)
  resp <- httr2::req_perform(req, path = part)
  if (httr2::resp_status(resp) != 200 || file.size(part) == 0) stop("Download failed: ", url)
  file.rename(part, path)
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
