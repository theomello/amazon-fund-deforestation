# Check: headline standard errors clustered by IBGE microregion (and state) instead of
# municipality or state x year. Microregions come from IBGE's 2017 territorial division
# (DTB 2017, the last edition that still publishes them; the Legal Amazon municipality
# codes in the panel all exist in it). Nothing here edits the existing estimators.

DTB_URL <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/divisao_territorial/2017/DTB_2017.zip"

# Municipality -> microregion, cached in data/derived/micro_microregions.csv.
get_microregions <- function(refresh = FALSE) {
  out <- "data/derived/micro_microregions.csv"
  if (file.exists(out) && !refresh) return(readr::read_csv(out, col_types = "ccc"))
  zip <- cache_download(DTB_URL, "data/raw/ibge/DTB_2017.zip", refresh)
  dir <- tempfile("dtb"); dir.create(dir); on.exit(unlink(dir, recursive = TRUE))
  utils::unzip(zip, "DTB_BRASIL_MUNICIPIO.xls", exdir = dir)
  x <- readxl::read_excel(file.path(dir, "DTB_BRASIL_MUNICIPIO.xls"), col_types = "text")
  res <- tibble::tibble(cd_mun = x[["Código Município Completo"]], uf_code = x[["UF"]],
                        microregion = paste0(x[["UF"]], "-", x[["Microrregião Geográfica"]]),
                        microregion_name = x[["Nome_Microrregião"]]) |>
    dplyr::select(cd_mun, microregion, microregion_name)
  dir.create("data/derived", showWarnings = FALSE)
  readr::write_csv(res, out)
  res
}

# Panel plus the cluster identifiers (microregion, state x year, state).
micro_panel <- function(panel, mr = get_microregions()) {
  panel <- dplyr::left_join(panel, dplyr::select(mr, cd_mun, microregion), by = "cd_mun")
  if (anyNA(panel$microregion[panel$in_sample])) stop("Sample municipalities without a microregion")
  panel$microregion[is.na(panel$microregion)] <- "none"
  dplyr::mutate(panel, uf_year = paste(uf, year), id_micro = as.integer(factor(microregion)), id_uf = as.integer(factor(uf)))
}

# Cluster definitions for fixest models: baseline (municipality), the three alternatives.
MICRO_CLUSTERS <- c("Municipality (baseline)" = "cd_mun", "State x year" = "uf_year",
                    "Microregion" = "microregion", "State" = "uf")

# Wild cluster restricted (WCR) bootstrap p-value (Cameron, Gelbach & Miller 2008; Webb 2014
# weights when there are few clusters) for one coefficient of a fixest model with absorbed
# fixed effects. By Frisch-Waugh the model is demeaned once; each draw re-demeans only the
# reweighted restricted residuals.
wild_cluster_p <- function(model, coef_name, cluster, B = 999, seed = 2026, weights = c("webb", "rademacher")) {
  weights <- match.arg(weights)
  X <- stats::model.matrix(model, type = "rhs"); yv <- as.numeric(stats::model.matrix(model, type = "lhs"))
  fe <- as.data.frame(stats::model.matrix(model, type = "fixef"))
  fe[] <- lapply(fe, function(z) as.integer(factor(z)))
  Xd <- fixest::demean(X, fe); yd <- as.numeric(fixest::demean(yv, fe))
  g <- as.integer(factor(cluster)); G <- max(g); j <- match(coef_name, colnames(X))
  if (is.na(j)) stop("Unknown coefficient ", coef_name)
  tstat <- function(y) {
    bread <- chol2inv(chol(crossprod(Xd))); b <- bread %*% crossprod(Xd, y); e <- y - Xd %*% b
    S <- rowsum(Xd * as.numeric(e), g); V <- bread %*% crossprod(S) %*% bread
    b[j] / sqrt(V[j, j])
  }
  Z <- Xd[, -j, drop = FALSE]; fit_r <- Z %*% solve(crossprod(Z), crossprod(Z, yd)); u <- as.numeric(yd - fit_r)
  t0 <- tstat(yd)
  set.seed(seed)
  draw <- if (weights == "webb") {
    support <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5)); function() sample(support, G, TRUE)
  } else function() sample(c(-1, 1), G, TRUE)
  tb <- vapply(seq_len(B), function(i) {
    ystar <- as.numeric(fit_r) + as.numeric(fixest::demean(matrix(u * draw()[g], ncol = 1), fe))
    tstat(ystar)
  }, numeric(1))
  list(t = t0, p = mean(abs(tb) >= abs(t0)), G = G, B = B, weights = weights)
}

# One row per headline coefficient and clustering choice.
micro_row <- function(m, coef_name, label, spec, panel_d) {
  rows <- lapply(names(MICRO_CLUSTERS), function(nm) {
    v <- MICRO_CLUSTERS[[nm]]
    ct <- fixest::coeftable(m, vcov = stats::as.formula(paste0("~", v)))
    tibble::tibble(spec = spec, term = label, cluster = nm, n_clusters = dplyr::n_distinct(panel_d[[v]]),
                   estimate = ct[coef_name, 1], std.error = ct[coef_name, 2], p.value = ct[coef_name, 4], p.wild = NA_real_)
  })
  dplyr::bind_rows(rows)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

micro_check <- function(panel, B = 999, write = TRUE) {
  p <- micro_panel(panel)
  s <- p[p$in_sample, ]
  rows <- list()
  add <- function(spec, m, coefs, wild = TRUE) for (cn in names(coefs)) {
    r <- micro_row(m, cn, coefs[[cn]], spec, s)
    if (wild) for (i in which(r$cluster %in% c("Microregion", "State"))) {
      v <- MICRO_CLUSTERS[[r$cluster[i]]]
      r$p.wild[i] <- wild_cluster_p(m, cn, s[[v]][fixest::obs(m)], B, weights = if (v == "uf") "webb" else "rademacher")$p
    }
    rows[[length(rows) + 1]] <<- r
  }
  main <- est_main(p)
  add("Dose-response (money, lag 1)", main, c(money_lag1 = "R$ 1 million, lagged one year"))
  restart_all <- est_exposure(p, "restart_exposure_all", 2022L)
  restart <- est_exposure(p, "restart_exposure", 2022L)
  freeze <- est_exposure(p, "freeze_exposure", 2018L)
  yrs <- function(x, years) stats::setNames(sprintf("PRODES %d vs 2022", years), sprintf("year::%d:%s", years, x))
  add("Restart, all contracts", restart_all, yrs("restart_exposure_all", 2024:2026))
  add("Restart, contracts before Aug 2024", restart, yrs("restart_exposure", 2024:2026))
  add("Freeze exposure", freeze, stats::setNames(sprintf("PRODES %d vs 2018", 2019:2022),
                                                 sprintf("year::%d:freeze_exposure", 2019:2022)))
  out <- dplyr::bind_rows(rows)
  # Staggered DiD: the clustered bootstrap needs a time-invariant cluster variable, so no state x year.
  stag <- dplyr::bind_rows(
    micro_staggered(s, NULL, "Municipality (baseline)"), micro_staggered(s, "id_micro", "Microregion"),
    micro_staggered(s, "id_uf", "State"))
  d <- dplyr::mutate(s, cohort = dplyr::coalesce(first_treat_year, 10000L))
  m <- fixest::feols(y ~ sunab(cohort, year) | cd_mun + year, data = d, cluster = ~cd_mun)
  sunab <- dplyr::bind_rows(lapply(names(MICRO_CLUSTERS), function(nm) {
    v <- MICRO_CLUSTERS[[nm]]
    ct <- fixest::coeftable(summary(m, agg = "ATT", vcov = stats::as.formula(paste0("~", v))))
    tibble::tibble(spec = "Sun-Abraham, overall ATT", cluster = nm, n_clusters = dplyr::n_distinct(d[[v]]),
                   estimate = ct[1, 1], std.error = ct[1, 2], p.value = ct[1, 4])
  }))
  stag <- dplyr::bind_rows(stag, sunab)
  res <- list(table = out, staggered = stag)
  if (write) {
    dir.create("results", showWarnings = FALSE)
    readr::write_csv(out, "results/micro_clustering.csv")
    readr::write_csv(stag, "results/micro_staggered.csv")
  }
  res
}

# Callaway & Sant'Anna overall ATT with the multiplier bootstrap clustered at `cluster`.
micro_staggered <- function(s, cluster, label) {
  d <- dplyr::mutate(s, id = as.integer(factor(cd_mun)), g = dplyr::coalesce(first_treat_year, 0L))
  set.seed(2026)
  att <- did::att_gt(yname = "y", tname = "year", idname = "id", gname = "g", data = as.data.frame(d),
                     control_group = "notyettreated", allow_unbalanced_panel = TRUE, print_details = FALSE,
                     clustervars = cluster)
  ov <- did::aggte(att, type = "simple", na.rm = TRUE)
  n_cl <- dplyr::n_distinct(d[[cluster %||% "id"]])  # tibble() would shadow `cluster` with the label column
  tibble::tibble(spec = "Staggered DiD, overall ATT", cluster = label, n_clusters = n_cl,
                 estimate = ov$overall.att, std.error = ov$overall.se,
                 p.value = 2 * stats::pnorm(-abs(ov$overall.att / ov$overall.se)))
}
