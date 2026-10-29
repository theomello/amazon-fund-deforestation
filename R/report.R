# Figures (committed to figures/ so they show on GitHub) and result tables.

INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"; ACCENT <- "#2a78d6"
COVERAGE_COLORS <- c("Named municipalities" = "#2a78d6", "Indigenous lands" = "#eb6834",
                     "Whole state" = "#1baf7a", "Biome-wide / federal" = "#eda100")

theme_report <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(text = ggplot2::element_text(colour = INK), axis.text = ggplot2::element_text(colour = INK2),
                   panel.grid.major = ggplot2::element_line(colour = GRID, linewidth = 0.3),
                   panel.grid.minor = ggplot2::element_blank(), legend.position = "top",
                   plot.title = ggplot2::element_text(face = "bold"), plot.title.position = "plot",
                   plot.background = ggplot2::element_rect(fill = "#fcfcfb", colour = NA),
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

save_fig <- function(plot, name, width = 8, height = 5) {
  dir.create("figures", showWarnings = FALSE)
  ggplot2::ggsave(file.path("figures", paste0(name, ".png")), plot, width = width, height = height, dpi = 200)
}

policy_lines <- function() {
  list(ggplot2::geom_vline(xintercept = c(2018.5, 2022.5), linetype = "dashed", colour = INK2, linewidth = 0.4),
       ggplot2::annotate("text", x = c(2018.6, 2022.6), y = Inf, label = c("Freeze", "Restart"),
                         hjust = 0, vjust = 1.5, size = 3, colour = INK2))
}

# Context: where the Fund's money went each year, next to Legal Amazon deforestation.
fig_overview <- function(panel) {
  read <- function(f) readr::read_csv(file.path("data/derived", f), show_col_types = FALSE)
  ipca <- get_ipca()
  money <- read("fund_disbursements.csv") |>
    dplyr::inner_join(read("fund_coverage.csv"), by = "slug") |>
    dplyr::filter(coverage_type != "outside") |>
    dplyr::mutate(year = prodes_year(date), value = deflate_to_2025(value_brl, year, ipca) / 1e6,
                  coverage = factor(dplyr::recode(coverage_type, municipalities = "Named municipalities",
                                                  indigenous_lands = "Indigenous lands", state = "Whole state",
                                                  biome_national = "Biome-wide / federal"), names(COVERAGE_COLORS))) |>
    dplyr::summarise(value = sum(value), .by = c(year, coverage)) |>
    dplyr::filter(year <= max(panel$year))
  defor <- dplyr::summarise(panel, km2 = sum(defor_km2), .by = year)
  a <- ggplot2::ggplot(money, ggplot2::aes(year, value, fill = coverage)) +
    ggplot2::geom_col(width = 0.75, colour = "#fcfcfb", linewidth = 0.3) +
    ggplot2::scale_fill_manual(values = COVERAGE_COLORS, name = NULL) +
    policy_lines() +
    ggplot2::labs(title = "Amazon Fund disbursements by where the project works",
                  subtitle = "R$ million (2025 prices), by PRODES year (Aug–Jul)", x = NULL, y = NULL) +
    theme_report()
  b <- ggplot2::ggplot(defor, ggplot2::aes(year, km2)) +
    ggplot2::geom_line(colour = INK, linewidth = 0.7) + ggplot2::geom_point(colour = INK, size = 1.4) +
    policy_lines() +
    ggplot2::scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA)) +
    ggplot2::labs(title = "Deforestation in the Legal Amazon", subtitle = "km² per PRODES year (INPE)", x = NULL, y = NULL) +
    theme_report()
  save_fig(a, "01_fund_disbursements", height = 4.2)
  save_fig(b, "02_deforestation", height = 3.4)
}

# Where municipality-specific money went before the freeze and after the restart.
fig_map <- function(panel) {
  geo <- get_ibge_mun()
  period <- function(years, label) dplyr::summarise(panel[panel$year %in% years, ], money = sum(money), .by = cd_mun) |>
    dplyr::mutate(period = label)
  m <- dplyr::bind_rows(period(2010:2018, "2010–2018 (before the freeze)"), period(2023:2026, "2023–2026 (after the restart)")) |>
    dplyr::mutate(cd_mun = as.character(cd_mun))
  shapes <- dplyr::inner_join(geo, m, by = "cd_mun", relationship = "one-to-many") |>
    dplyr::mutate(money = ifelse(money > 0.01, money, NA))
  states <- dplyr::summarise(dplyr::group_by(geo, uf))
  p <- ggplot2::ggplot(shapes) +
    ggplot2::geom_sf(ggplot2::aes(fill = money), colour = NA) +
    ggplot2::geom_sf(data = states, fill = NA, colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradient(low = "#b7d3f6", high = "#0d366b", trans = "log10", na.value = "#ecebe7",
                                 labels = scales::label_number(accuracy = 0.1), name = "R$ million") +
    ggplot2::facet_wrap(~period) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(title = "Where local Fund projects spent their money",
                  subtitle = "Disbursements of municipality-specific projects, 2025 prices; grey = none") +
    theme_report() + ggplot2::theme(axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
                                    legend.position = "right")
  save_fig(p, "03_map_money", width = 9, height = 4.6)
}

es_frame <- function(model) {
  ct <- fixest::coeftable(model)
  ct <- ct[grepl("^year::", rownames(ct)) & !grepl("stock|priority_base", rownames(ct)), , drop = FALSE]
  tibble::tibble(year = as.integer(sub("^year::([0-9]+):.*", "\\1", rownames(ct))), est = ct[, 1], se = ct[, 2])
}

fig_event <- function(d, ref, title, subtitle, name) {
  d <- dplyr::bind_rows(d, tibble::tibble(year = ref, est = 0, se = 0)) |>
    dplyr::mutate(pct = pct_effect(est), lo = pct_effect(est - 1.96 * se), hi = pct_effect(est + 1.96 * se))
  p <- ggplot2::ggplot(d, ggplot2::aes(year, pct)) +
    ggplot2::geom_hline(yintercept = 0, colour = INK2, linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = ref + 0.5, linetype = "dashed", colour = INK2, linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = lo, ymax = hi), colour = ACCENT, linewidth = 0.8, alpha = 0.6) +
    ggplot2::geom_point(colour = ACCENT, size = 2.2) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = "% change in deforestation") +
    theme_report()
  save_fig(p, name, height = 4.2)
}

# Headline: restart exposure from all contracts since 2023, next to the stricter
# version that only counts contracts signed before the 2025 outcome year.
fig_restart <- function(models) {
  series <- c("All contracts since 2023" = "restart_all", "Only contracts signed before Aug 2024" = "restart")
  colors <- setNames(c("#2a78d6", "#eb6834"), names(series))
  d <- dplyr::bind_rows(lapply(series, function(m) dplyr::bind_rows(es_frame(models[[m]]), tibble::tibble(year = 2022L, est = 0, se = 0))),
                        .id = "variant") |>
    dplyr::mutate(variant = factor(variant, names(series)), pct = pct_effect(est),
                  lo = pct_effect(est - 1.96 * se), hi = pct_effect(est + 1.96 * se),
                  x = year + ifelse(variant == names(series)[1], -0.15, 0.15))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, pct, colour = variant)) +
    ggplot2::geom_hline(yintercept = 0, colour = INK2, linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = 2022.5, linetype = "dashed", colour = INK2, linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = lo, ymax = hi), linewidth = 0.8, alpha = 0.6) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_colour_manual(values = colors, name = NULL) +
    ggplot2::labs(title = "After the restart, deforestation fell where more new money was contracted",
                  subtitle = "Effect of R$ 1 million contracted since 2023, by year (relative to 2022; 95% CI).\nThe fall comes from contracts signed from Aug 2024 on, i.e. during the years it is measured in.",
                  x = NULL, y = "% change in deforestation") +
    theme_report()
  save_fig(p, "04_restart_event_study", height = 4.6)
}

# The headline in raw data: deforestation of three groups, each relative to its own
# 2019-2022 level (sums of km², sample municipalities).
fig_trajectories <- function(panel) {
  uniao <- "Uniao-com-Municipios-pela-Reducao-de-Desmatamento-e-Incendios-Florestais"
  links <- readr::read_csv("data/derived/fund_links.csv", col_types = "ccd")
  groups <- c("União com Municípios (signed Sep 2025)", "Other municipalities with local projects", "No local project")
  colors <- setNames(c("#2a78d6", "#eb6834", "#1baf7a"), groups)
  d <- panel[panel$in_sample, ] |>
    dplyr::mutate(group = dplyr::case_when(cd_mun %in% links$cd_mun[links$slug == uniao] ~ groups[1],
                                           !is.na(first_treat_year) ~ groups[2], TRUE ~ groups[3])) |>
    dplyr::summarise(km2 = sum(defor_km2), n = dplyr::n_distinct(cd_mun), .by = c(group, year)) |>
    dplyr::mutate(index = 100 * km2 / mean(km2[year %in% 2019:2022]), .by = group)
  n <- dplyr::distinct(d, group, n)
  legend <- setNames(paste0(n$group, " (", n$n, ")"), n$group)[groups]
  d <- dplyr::mutate(d, group = factor(legend[group], legend))
  colors <- setNames(colors, legend)
  labels <- dplyr::filter(d, year == max(year)) |> dplyr::mutate(label = round(index))
  p <- ggplot2::ggplot(d, ggplot2::aes(year, index, colour = group)) +
    ggplot2::geom_hline(yintercept = 100, colour = GRID, linewidth = 0.5) +
    policy_lines() +
    ggplot2::geom_line(linewidth = 0.8) + ggplot2::geom_point(size = 1.3) +
    ggplot2::geom_text(data = labels, ggplot2::aes(label = label), hjust = 0, nudge_x = 0.35, size = 3.2, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = colors, name = NULL) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = c(0.5, 2.2))) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::labs(title = "Deforestation by group, relative to each group's 2019–2022 level",
                  subtitle = "Index, 2019–2022 average = 100; sum of PRODES deforestation in each group (number of municipalities)",
                  x = NULL, y = NULL) +
    theme_report()
  save_fig(p, "08_raw_trajectories", height = 4.8)
}

# Targeting: municipalities with more deforestation before most projects began got more local money.
fig_targeting <- function(panel) {
  d <- panel[panel$in_sample, ] |>
    dplyr::summarise(baseline = mean(defor_km2[year %in% 2008:2012]), money = sum(money),
                     priority = any(priority[year <= 2022] == 1), .by = cd_mun) |>
    dplyr::mutate(priority = factor(ifelse(priority, "On the priority blacklist (2008–2022)", "Not on the blacklist"),
                                    c("On the priority blacklist (2008–2022)", "Not on the blacklist")))
  funded <- dplyr::filter(d, money > 0.01)
  p <- ggplot2::ggplot(funded, ggplot2::aes(baseline, money)) +
    ggplot2::geom_point(ggplot2::aes(colour = priority), size = 2, alpha = 0.8) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, colour = INK, linewidth = 0.6, se = TRUE, fill = GRID) +
    ggplot2::scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) +
    ggplot2::scale_y_log10(labels = scales::label_number(accuracy = 0.1)) +
    ggplot2::scale_colour_manual(values = c("#2a78d6", "#a3a29c"), name = NULL) +
    ggplot2::labs(title = "Local Fund money went where deforestation already was",
                  subtitle = sprintf("Each dot is a municipality with local Fund money (%d of %d in the sample); log scales",
                                     nrow(funded), nrow(d)),
                  x = "Average yearly deforestation 2008–2012 (km²)", y = "Local Fund money 2010–2026 (R$ million, 2025 prices)") +
    theme_report()
  save_fig(p, "09_money_followed_deforestation", height = 5)
}

fig_staggered <- function(staggered) {
  d <- staggered$dynamic
  e <- tibble::tibble(e = d$egt, pct = pct_effect(d$att.egt), lo = pct_effect(d$att.egt - 1.96 * d$se.egt),
                      hi = pct_effect(d$att.egt + 1.96 * d$se.egt))
  p <- ggplot2::ggplot(e, ggplot2::aes(e, pct)) +
    ggplot2::geom_hline(yintercept = 0, colour = INK2, linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed", colour = INK2, linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = lo, ymax = hi), colour = ACCENT, linewidth = 0.8, alpha = 0.6) +
    ggplot2::geom_point(colour = ACCENT, size = 2.2) +
    ggplot2::labs(title = "After a first local project, deforestation kept rising relative to other municipalities",
                  subtitle = "Deforestation relative to not-yet-treated municipalities, by years since the first local project\n(Callaway & Sant'Anna 2021; 95% CI)",
                  x = "Years since first contract", y = "% difference in deforestation") +
    theme_report()
  save_fig(p, "05_first_project_event_study", height = 4.2)
}

fig_robustness <- function(rob) {
  d <- dplyr::mutate(rob, spec = factor(spec, rev(spec)), lo = estimate - 1.96 * std.error, hi = estimate + 1.96 * std.error)
  p <- ggplot2::ggplot(d, ggplot2::aes(estimate, spec)) +
    ggplot2::geom_vline(xintercept = 0, colour = INK2, linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lo, xmax = hi), colour = ACCENT, linewidth = 0.8, alpha = 0.6) +
    ggplot2::geom_point(colour = ACCENT, size = 2) +
    ggplot2::labs(title = "Money and next year's deforestation: no robust effect",
                  subtitle = "Coefficient on Fund money (R$ million, lagged one year), 95% CI", x = NULL, y = NULL) +
    theme_report()
  save_fig(p, "07_dose_response_robustness", height = 6)
}

write_results <- function(models) {
  dir.create("results", showWarnings = FALSE)
  fixest::etable(models$main, models$mechanism, tex = FALSE) |> utils::write.csv("results/main_and_mechanism.csv")
  readr::write_csv(models$robustness, "results/robustness.csv")
  restart <- dplyr::bind_rows(lapply(c(all_contracts = "restart_all", signed_before_aug2024 = "restart",
                                         signed_before_aug2024_without_uniao = "restart_no_uniao",
                                         signed_before_aug2024_asinh = "restart_asinh"),
                                       function(m) es_frame(models[[m]])), .id = "variant")
  readr::write_csv(restart, "results/restart_event_study.csv")
  readr::write_csv(es_frame(models$freeze), "results/freeze_event_study.csv")
  d <- models$staggered$dynamic
  readr::write_csv(tibble::tibble(e = d$egt, att = d$att.egt, se = d$se.egt), "results/first_project_event_study.csv")
}

report_all <- function(panel, models) {
  fig_overview(panel)
  fig_map(panel)
  fig_restart(models)
  fig_trajectories(panel)
  fig_targeting(panel)
  fig_staggered(models$staggered)
  fig_event(es_frame(models$freeze), 2018L, "The 2019 freeze: no rise where Fund money dried up",
            "Effect of R$ 1 million/year of lost local Fund money, by year (relative to 2018; 95% CI)", "06_freeze_event_study")
  fig_robustness(models$robustness)
  write_results(models)
}
