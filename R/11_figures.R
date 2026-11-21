# ============================================================================
# 11_figures.R — all figures, generated in the order they appear in the paper
#
# Structure of this file:
#   Part A  Provenance notes, carried over from the original script
#   Part B  Figure construction: every builder function and plot object.
#           Extracted verbatim from plotting_volatilities_091126.R
#           lines 66-4023 and 4605-4813 (9 Nov 2026 version).
#   Part C  All writes to disk, in paper figure order, signposted
#           Figure 1 ... Figure 22, then unnumbered and slide figures.
#
# Why saving is separated from construction: in the original script the
# ggsave calls were spread across two blocks in the order the plots were
# written rather than the order they appear in the paper. Part B therefore
# runs with .vline_defer_saves = TRUE, which turns safe_save into a no-op,
# so the save calls still embedded in the extracted code do nothing. Every
# file is then written once, in order, in Part C. No plotting code was
# altered to achieve this.
#
# Figure numbers follow HapIneq-45 (17 Nov 2026). FIGURES.md holds the
# figure-to-filename mapping. Filenames are unchanged from the original.
# ============================================================================

if (!exists("VLINE_ROOT")) {
  stop("Source R/00_config.R first, or run the pipeline through run_all.R.")
}

# ============================================================================
# PART A — provenance notes from the original script
# ============================================================================

# > # ============================================================
# > # ============================================================
# > # Charlie Harrison
# > # Last Updated: 9th November 2026
# > # (Previous version: 15th August 2026 -- kept unchanged as its own file)
# > #
# > # Changes in this version (9 Nov 2026):
# > #  - Within-panel income Gini (income_gini.csv) no longer used.
# > #    Life satisfaction comparison plots use WID Top 10 / Bottom 50
# > #    ratios (income and wealth) from wid_inequality.csv, built by
# > #    cleaning Section 5b (wid_cleaning_091126.R).
# > #  - Elite polarisation plots include CHES general left-right
# > #    (lrgen), the main series, alongside lrecon and galtan.
# > #  - Comparison plots come in several versions:
# > #      Version 1  = paradox plots (Section 10, saved in 11.10)
# > #      Version 2a = WID income and wealth, separate panels (10c)
# > #      Version 2b = WID income and wealth, same panel (10c)
# > #      Elite E1-E3 standalone versions + singles (10d)
# > #  - New: volatility with No Controls and Model 5 on one panel (10e).
# > #  - Points removed from all volatility plots and WID plots (kept for
# > #    elite polarisation). WID wealth: Germany on its own y-axis.
# > #    Swiss political x-axis runs to 2023 (was clipped at 2020).
# > #  - WID 2a (indexed): country-specific secondary axes with the raw ratio.
# > #  - Swiss paradox plots stop at the last CHES wave (2019); Swiss
# > #    hidden-divergence (V-LINE) plots run to the end of the SHP data.
# > #  - Left-right elite line back to normal width (darker colour only).
# > #
# > # Structure:
# > # 0)  Paths, Load Pre-Computed Data
# > # 1)  Setup & Specifications
# > # 2)  Core Plotting Functions
# > # 3)  Volatility Plots (Life Satisfaction)
# > # 4)  Volatility Plots (Political)
# > # 5)  V-LINE Plots (Life Satisfaction, incl. COVID V-LINE)
# > # 6)  V-LINE Plots (Political)
# > # 7a)  Robustness Comparison Plots (UK: pre/across-election vs yearly)
# > # 7b) Nominal Stats and Distributions
# > # 8)  Income Category Validation
# > # 9)  Linearity/Non-Linearity Robustness
# > # 10) Paradox Plots -- Comparison Version 1 (WID + lrgen)
# > # 10b) Placebo Test (Health Status)
# > # 10c) WID Standalone -- Comparison Versions 2a / 2b
# > # 10d) Elite Polarisation Standalone (E1-E3, singles)
# > # 10e) Volatility Overlay (No Controls vs Model 5)
# > # 11) Saving All Plots
# > # ============================================================
# > # ============================================================
#
# > # remotes::install_version("ggplot2", version = "3.5.2")

# ============================================================================
# PART B — figure construction (verbatim extract; no writes)
# ============================================================================

.vline_defer_saves <- TRUE   # safe_save() is inert for the rest of Part B


# All Model Outputs
vol_lsat <- read_csv(file.path(modeloutput_path, "volatility_life_satisfaction.csv"), show_col_types = FALSE)
vol_pol <- read_csv(file.path(modeloutput_path, "volatility_political.csv"), show_col_types = FALSE)
vline_lsat <- read_csv(file.path(modeloutput_path, "vline_life_satisfaction.csv"), show_col_types = FALSE)
vline_pol <- read_csv(file.path(modeloutput_path, "vline_political.csv"), show_col_types = FALSE)
lincat_vline <- read_csv(file.path(modeloutput_path, "income_lincat_vline.csv"), show_col_types = FALSE)
all_vline_robustness <- read_csv(file.path(modeloutput_path, "v_line_nonlinear.csv"), show_col_types = FALSE)
vol_inputs <- read_csv(file.path(modeloutput_path, "volatility_inputs.csv"), show_col_types = FALSE)
all_distributions <- read_csv(file.path(modeloutput_path, "distribution_first_vs_last.csv"), show_col_types = FALSE)
# income_gini.csv (within-panel Gini) retired 9 Nov 2026 -- replaced by WID:
wid_ineq <- read_csv(file.path(modeloutput_path, "wid_inequality.csv"), show_col_types = FALSE)
elite_polarisation_ches <- read_csv(file.path(modeloutput_path, "elite_polarisation_ches.csv"), show_col_types = FALSE)
# vparty_swi_spread <- read_csv(file.path(modeloutput_path, "elite_polarisation_vparty_switzerland.csv"), show_col_types = FALSE)  # V-Party retired; never used below
# vparty_uk_spread <- read_csv(file.path(modeloutput_path, "elite_polarisation_vparty_uk.csv"), show_col_types = FALSE)  # V-Party retired; never used below



# ============================================================
# ============================================================
# 1) SETUP & SPECIFICATIONS
# ============================================================
# ============================================================
plot_specs <- tribble(
  ~country, ~domain, ~variable, ~time_var, ~data_source,
  ~x_breaks, ~x_limits, ~title,
  ~vol_y_breaks, ~vol_y_limits,
  ~vline_sd_y_breaks, ~vline_sd_y_limits,
  ~vline_mean_y_breaks, ~vline_mean_y_limits,
  ~base_year_natural, ~base_year_alternate,
  ~panel_width,
  
  # LIFE SATISFACTION - Main Analysis  [y-axes harmonised 10 Nov 2026: 60-120 in 20s, top and bottom panels]
  "Germany", "Life Satisfaction", "lfsato", "year", "volatility_life_satisfaction.csv",
  seq(1985, 2020, 5), c(1984, 2020), "Germany",
  seq(60, 120, 20), c(60, 120),
  seq(60, 120, 20), c(60, 120),
  seq(90, 150, 10), c(90, 150),
  1984, 1985,
  3,
  
  "Australia", "Life Satisfaction", "lfsato", "year", "volatility_life_satisfaction.csv",
  seq(2000, 2020, 5), c(2000, 2020), "Australia",
  seq(60, 120, 20), c(60, 120),
  seq(60, 120, 20), c(60, 120),
  seq(90, 150, 10), c(90, 150),
  2001, 2002,
  2,
  
  "Switzerland", "Life Satisfaction", "sat_life", "year", "volatility_life_satisfaction.csv",
  seq(2000, 2020, 5), c(1999, 2020), "Switzerland",
  seq(60, 120, 20), c(60, 120),
  seq(60, 120, 20), c(60, 120),
  seq(90, 150, 10), c(90, 150),
  2000, 2001,
  2,
  
  # LIFE SATISFACTION - COVID Era
  "Germany (COVID Era)", "Life Satisfaction", "lfsato", "year", "volatility_life_satisfaction.csv",
  seq(2017, 2022, 1), c(2016.5, 2022.5), "Germany",
  seq(90, 110, 5), c(90, 110),
  seq(90, 110, 5), c(90, 110),
  seq(90, 110, 5), c(90, 110),
  2018, 2019,
  1,
  
  "Switzerland (COVID Era)", "Life Satisfaction", "sat_life", "year", "volatility_life_satisfaction.csv",
  seq(2017, 2022, 1), c(2016.5, 2022.5), "Switzerland",
  seq(90, 110, 4), c(90, 110),
  seq(90, 110, 5), c(90, 110),
  seq(90, 110, 5), c(90, 110),
  2018, 2019,
  1,
  
  # POLITICAL - Switzerland (SHP)  [x-axis extended to 2023, 9 Nov 2026: SHP runs past 2020]
  #                                [y-axes harmonised 10 Nov 2026: 50-125 in 25s, limit 130]
  "Switzerland (SHP)", "Political", "lrself", "year", "volatility_political.csv",
  seq(2000, 2020, 5), c(1999, 2023), "Switzerland",
  seq(50, 125, 25), c(50, 130),
  seq(50, 125, 25), c(50, 130),
  NA, NA,
  1999, 2000,
  2,
  
  # POLITICAL - UK (BES) - By Election  [y-axes harmonised 10 Nov 2026: 50-125 in 25s, limit 130]
  "UK (BES Elections)", "Political", "lr", "election", "volatility_political.csv",
  1:5, c(0.5, 5.5), "Britain",
  seq(50, 125, 25), c(50, 130),
  seq(50, 125, 25), c(50, 130),
  NA, NA,
  1, 1,
  2,
  
  # POLITICAL - UK (BES) - Pre-Election Only (Robustness)
  "UK (BES Elections, Pre-Election)", "Political", "lr", "election", "volatility_political.csv",
  1:5, c(0.5, 5.5), "UK (Campaign Volatility)",
  seq(50, 125, 25), c(50, 125),
  NA, NA,
  NA, NA,
  1, 1,
  1,
  
  # POLITICAL - UK (BES) - Election Swing (Robustness)
  "UK (BES Elections, Election Swing)", "Political", "lr", "election", "volatility_political.csv",
  1:5, c(0.5, 5.5), "UK (Across Election Volatility)",
  seq(50, 125, 25), c(50, 125),
  NA, NA,
  NA, NA,
  1, 1,
  1,
  
  # POLITICAL - UK (BES) - Yearly (Robustness)
  "UK (BES Yearly)", "Political", "lr", "year", "volatility_political.csv",
  seq(2014, 2020, 2), c(2014, 2020), "UK (Quasi-Yearly Volatility)",
  seq(50, 125, 25), c(50, 125),
  NA, NA,
  NA, NA,
  2014, 2015,
  1
)

# ============================================================
# MODEL SPECIFICATIONS
# ============================================================

model_labels <- c(
  "model_1" = "No Controls",
  "model_2" = "Demographic Controls",
  "model_3" = "Linear Panel Tenure",
  "model_4" = "Panel Tenure (Factor)",
  "model_5" = "Panel Tenure (Factor) + Demographics",
  "model_6" = "New Respondents Only",
  "model_7" = "Balanced Panel",
  "model_8" = "Demographics + Tenure + Days Between Surveys"
)

# ============================================================
# COLOR PALETTES
# ============================================================

# UK Election labels
uk_election_labels <- c("GE '15", "Brexit '16", "GE '17", "GE '19", "GE '24")

# ============================================================
# HELPER FUNCTIONS
# ============================================================

get_plot_specs <- function(country_name, variable_name = NULL) {
  
  if (!is.null(variable_name)) {
    specs <- plot_specs %>%
      filter(country == country_name, variable == variable_name)
  } else {
    specs <- plot_specs %>%
      filter(country == country_name)
  }
  
  if (nrow(specs) == 0) {
    stop("No specs found for: ", country_name, " ", variable_name)
  }
  
  result <- as.list(specs[1, ])
  
  vector_cols <- c("x_breaks", "x_limits", "vol_y_breaks", "vol_y_limits",
                   "vline_sd_y_breaks", "vline_sd_y_limits",
                   "vline_mean_y_breaks", "vline_mean_y_limits")
  
  for (col in vector_cols) {
    if (!is.null(result[[col]])) {
      result[[col]] <- unlist(result[[col]])
    }
  }
  
  return(result)
}


# ============================================================
# ============================================================
# 2) CORE PLOTTING FUNCTIONS
# ============================================================
# ============================================================

create_volatility_plot <- function(data,
                                   country_name,
                                   variable_name,
                                   model_id = "model_5",
                                   show_primary_label = TRUE,
                                   show_secondary_label = TRUE,
                                   show_secondary_axis = TRUE,
                                   use_interpolated = TRUE,
                                   show_legend = TRUE,
                                   custom_x_breaks = NULL,
                                   custom_x_labels = NULL,
                                   custom_x_limits = NULL) {
  
  specs <- get_plot_specs(country_name, variable_name)
  
  plot_data <- data %>%
    filter(country == country_name,
           variable == variable_name,
           model_id == !!model_id)
  
  # Filter for non-overlapping windows (only for year-based data, not elections or COVID)
  if (specs$time_var == "year" && !grepl("COVID", country_name)) {
    plot_data <- plot_data %>%
      mutate(years_since_start = time_period - min(time_period)) %>%
      filter(years_since_start %% 2 == 0)
  }
  
  if (use_interpolated && "indexed_within_var_interpolated" %in% names(plot_data)) {
    indexed_var <- "indexed_within_var_interpolated"
  } else {
    indexed_var <- "indexed_within_var"
  }
  raw_var <- "mean_within_var"
  
  # NEW: Base-period anchored scaling
  # Get the raw value at base period (where indexed = 100)
  base_raw <- plot_data %>%
    filter(time_period == specs$base_year_natural) %>%
    pull(!!sym(raw_var)) %>%
    first()
  
  # Scale factor: since indexed = 100 * (raw / base_raw), then raw = indexed * (base_raw / 100)
  scale_factor <- base_raw / 100
  
  # Secondary axis breaks = primary breaks * scale_factor
  secondary_values <- specs$vol_y_breaks * scale_factor
  
  plot_title <- specs$title
  legend_label <- if (specs$domain == "Political") "Left-Right Ideology" else "Life Satisfaction"
  line_color <- if (specs$domain == "Political") "steelblue" else "darkslateblue"
  
  plot_data <- plot_data %>%
    mutate(series_label = legend_label)
  
  x_breaks <- if (!is.null(custom_x_breaks)) custom_x_breaks else specs$x_breaks
  x_limits <- if (!is.null(custom_x_limits)) custom_x_limits else specs$x_limits
  
  p <- ggplot(plot_data, aes(x = time_period)) +
    geom_line(aes(y = .data[[indexed_var]], color = series_label),
              linewidth = 1, alpha = 0.9) +
    scale_color_manual(
      name = "",
      values = setNames(line_color, legend_label)
    ) +
    labs(title = plot_title, x = "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = if (show_legend) "bottom" else "none"
    )
  
  if (!is.null(custom_x_labels)) {
    p <- p + scale_x_continuous(breaks = x_breaks, labels = custom_x_labels, limits = x_limits)
  } else {
    p <- p + scale_x_continuous(breaks = x_breaks, limits = x_limits)
  }
  
  if (show_secondary_axis) {
    p <- p + scale_y_continuous(
      name = if (show_primary_label) "Indexed Volatility" else "",
      breaks = specs$vol_y_breaks,
      limits = specs$vol_y_limits,
      sec.axis = sec_axis(
        trans = ~ . * scale_factor,
        name = if (show_secondary_label) "Absolute Volatility" else "",
        breaks = secondary_values,
        labels = number_format(accuracy = 0.001)(secondary_values)
      )
    )
  } else {
    p <- p + scale_y_continuous(
      name = if (show_primary_label) "Indexed Volatility" else "",
      breaks = specs$vol_y_breaks,
      limits = specs$vol_y_limits
    )
  }
  
  return(p)
}



# ============================================================
# V-LINE ADJUSTMENT PLOT (Nominal vs Adjusted)
# ============================================================

create_vline_plot <- function(data,
                              country_name,
                              variable_name,
                              metric_type = c("sd", "mean"),
                              grouping_type = "natural",
                              show_primary_label = TRUE,
                              show_secondary_label = TRUE,
                              show_secondary_axis = TRUE,
                              custom_x_breaks = NULL,
                              custom_x_labels = NULL,
                              custom_x_limits = NULL) {
  
  metric_type <- match.arg(metric_type)
  specs <- get_plot_specs(country_name, variable_name)
  
  plot_data <- data %>%
    filter(country == country_name,
           variable == variable_name)
  
  if ("grouping" %in% names(plot_data)) {
    plot_data <- plot_data %>% filter(grouping == grouping_type)
  }
  
  if (nrow(plot_data) == 0) {
    warning(paste("No V-LINE data for:", country_name, variable_name, grouping_type))
    return(ggplot() + theme_void() + labs(title = paste("No data:", country_name)))
  }
  
  if (metric_type == "sd") {
    indexed_nom <- "indexed_sd"
    indexed_adj <- "indexed_adjusted_sd"
    raw_nom <- "sd_level"
    raw_adj <- "adjusted_sd"
    label_nom <- if (specs$domain == "Political") "sd(Left-Right)" else "sd(Life Satisfaction)"
    label_adj <- "sd(V-LINE)"
    y_label <- "Indexed Inequality (SD)"
    y_label_sec <- "Absolute Inequality (SD)"
    y_breaks <- specs$vline_sd_y_breaks
    y_limits <- specs$vline_sd_y_limits
  } else {
    indexed_nom <- "indexed_mean"
    indexed_adj <- "indexed_adjusted_mean"
    raw_nom <- "mean_level"
    raw_adj <- "adjusted_mean"
    label_nom <- if (specs$domain == "Political") "Mean Position" else "Life Satisfaction"
    label_adj <- "V-LINE"
    y_label <- "Indexed Happiness"
    y_label_sec <- "Absolute Happiness"
    y_breaks <- specs$vline_mean_y_breaks
    y_limits <- specs$vline_mean_y_limits
  }
  
  if (any(is.na(y_breaks)) || any(is.na(y_limits))) {
    warning(paste("NA y-axis specs for:", country_name, variable_name, metric_type))
    return(ggplot() + theme_void() + labs(title = paste("No specs:", country_name)))
  }
  
  # NEW: Base-period anchored scaling
  # Get the raw nominal value at base period (where indexed = 100)
  base_year <- if (grouping_type == "alternate") specs$base_year_alternate else specs$base_year_natural
  
  base_raw <- plot_data %>%
    filter(period_numeric == base_year) %>%
    pull(!!sym(raw_nom)) %>%
    first()
  
  # Scale factor: indexed = 100 * (raw / base_raw), so raw = indexed * (base_raw / 100)
  scale_factor <- base_raw / 100
  secondary_values <- y_breaks * scale_factor
  
  plot_long <- plot_data %>%
    select(period_numeric, all_of(c(indexed_nom, indexed_adj))) %>%
    pivot_longer(cols = c(all_of(indexed_nom), all_of(indexed_adj)),
                 names_to = "type", values_to = "value") %>%
    mutate(
      label = factor(
        ifelse(type == indexed_nom, label_nom, label_adj),
        levels = c(label_nom, label_adj)
      )
    )
  
  title_suffix <- if (grouping_type == "alternate") " (Alternate Grouping)" else ""
  plot_title <- paste0(specs$title, title_suffix)
  
  x_breaks <- if (!is.null(custom_x_breaks)) custom_x_breaks else specs$x_breaks
  x_limits <- if (!is.null(custom_x_limits)) custom_x_limits else specs$x_limits
  
  p <- ggplot(plot_long, aes(x = period_numeric)) +
    geom_line(aes(y = value, color = label), linewidth = 1, alpha = 0.9) +
    # geom_point(aes(y = value, color = label, shape = label), size = 2.5, alpha = 0.9) +
    scale_color_manual(
      name = "",
      values = setNames(c("blue", "red"), c(label_nom, label_adj)) 
    ) +
    # scale_shape_manual(
    #   name = "",
    #   values = setNames(c(16, 18), c(label_nom, label_adj))
    # ) +
    labs(title = plot_title, x = "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "bottom"
    )
  
  if (!is.null(custom_x_labels)) {
    p <- p + scale_x_continuous(breaks = x_breaks, labels = custom_x_labels, limits = x_limits)
  } else {
    p <- p + scale_x_continuous(breaks = x_breaks, limits = x_limits)
  }
  
  if (show_secondary_axis) {
    p <- p + scale_y_continuous(
      name = if (show_primary_label) y_label else "",
      breaks = y_breaks,
      limits = y_limits,
      sec.axis = sec_axis(
        trans = ~ . * scale_factor,
        name = if (show_secondary_label) y_label_sec else "",
        breaks = secondary_values,
        labels = number_format(accuracy = 0.01)(secondary_values)
      )
    )
  } else {
    p <- p + scale_y_continuous(
      name = if (show_primary_label) y_label else "",
      breaks = y_breaks,
      limits = y_limits
    )
  }
  
  return(p)
}

# ============================================================
# GROUPING COMPARISON PLOT (Natural vs Alternate)
# ============================================================

create_grouping_comparison_plot <- function(data,
                                            country_name,
                                            variable_name,
                                            metric_type = c("sd", "mean"),
                                            show_primary_label = TRUE,
                                            show_secondary_label = TRUE,
                                            show_secondary_axis = TRUE) {

  metric_type <- match.arg(metric_type)
  specs <- get_plot_specs(country_name, variable_name)

  plot_data <- data %>%
    filter(country == country_name, variable == variable_name)

  if (metric_type == "sd") {
    indexed_nom <- "indexed_sd"
    indexed_adj <- "indexed_adjusted_sd"
    raw_nom <- "sd_level"
    label_nom <- "sd(Nominal)"
    label_adj <- "sd(V-LINE)"
    y_label <- "Indexed Inequality (SD)"
    y_label_sec <- "Absolute Inequality (SD)"
    y_breaks <- specs$vline_sd_y_breaks
    y_limits <- specs$vline_sd_y_limits
  } else {
    indexed_nom <- "indexed_mean"
    indexed_adj <- "indexed_adjusted_mean"
    raw_nom <- "mean_level"
    label_nom <- "Nominal"
    label_adj <- "V-LINE"
    y_label <- "Indexed Happiness"
    y_label_sec <- "Absolute Happiness"
    y_breaks <- specs$vline_mean_y_breaks
    y_limits <- specs$vline_mean_y_limits
  }

  # NEW: Base-period anchored scaling (use natural grouping for base)
  base_raw <- plot_data %>%
    filter(grouping == "natural", period_numeric == specs$base_year_natural) %>%
    pull(!!sym(raw_nom)) %>%
    first()

  scale_factor <- base_raw / 100
  secondary_values <- y_breaks * scale_factor

  plot_long <- plot_data %>%
    select(period_numeric, grouping, all_of(c(indexed_nom, indexed_adj))) %>%
    pivot_longer(cols = c(all_of(indexed_nom), all_of(indexed_adj)),
                 names_to = "adjustment", values_to = "value") %>%
    mutate(
      adj_label = ifelse(adjustment == indexed_nom, label_nom, label_adj),
      group_label = ifelse(grouping == "natural", "t", "t+1"),
      line_id = paste(adj_label, "-", group_label)
    )

  line_ids <- c(
    paste(label_nom, "- t"), paste(label_nom, "- t+1"),
    paste(label_adj, "- t"), paste(label_adj, "- t+1")
  )
  colors <- setNames(c("blue", "blue", "red", "red"), line_ids)
  linetypes <- setNames(c("solid", "dashed", "solid", "dashed"), line_ids)

  plot_title <- specs$title

  p <- ggplot(plot_long, aes(x = period_numeric, y = value,
                             color = line_id, linetype = line_id)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_color_manual(name = "", values = colors, breaks = line_ids) +
    scale_linetype_manual(name = "", values = linetypes, breaks = line_ids) +
    scale_x_continuous(breaks = specs$x_breaks, limits = specs$x_limits) +
    labs(title = plot_title, x = "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "bottom",
      legend.key.width = unit(1.5, "cm")
    ) +
    guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))

  if (show_secondary_axis) {
    p <- p + scale_y_continuous(
      name = if (show_primary_label) y_label else "",
      breaks = y_breaks,
      limits = y_limits,
      sec.axis = sec_axis(
        trans = ~ . * scale_factor,
        name = if (show_secondary_label) y_label_sec else "",
        breaks = secondary_values,
        labels = number_format(accuracy = 0.01)(secondary_values)
      )
    )
  } else {
    p <- p + scale_y_continuous(
      name = if (show_primary_label) y_label else "",
      breaks = y_breaks,
      limits = y_limits
    )
  }

  return(p)
}
#



# ============================================================
# ============================================================
# 3) VOLATILITY PLOTS - LIFE SATISFACTION
# ============================================================
# ============================================================

# Combined: 3 countries, secondary axis ticks on all, titles only on edges
create_publication_lsat_volatility <- function(vol_data, model_id = "model_5") {
  
  # Germany: primary label YES, secondary label NO (left panel)
  p_ger <- create_volatility_plot(vol_data, "Germany", "lfsato", model_id,
                                  show_primary_label = TRUE,
                                  show_secondary_label = FALSE,
                                  show_secondary_axis = TRUE)
  
  # Australia: no labels (middle panel)
  p_aus <- create_volatility_plot(vol_data, "Australia", "lfsato", model_id,
                                  show_primary_label = FALSE,
                                  show_secondary_label = FALSE,
                                  show_secondary_axis = TRUE)
  
  # Switzerland: primary label NO, secondary label YES (right panel)
  p_che <- create_volatility_plot(vol_data, "Switzerland", "sat_life", model_id,
                                  show_primary_label = FALSE,
                                  show_secondary_label = TRUE,
                                  show_secondary_axis = TRUE)
  
  combined <- p_ger + p_aus + p_che +
    plot_layout(widths = c(3, 2, 2), guides = "collect") &
    theme(legend.position = "bottom")
  
  # combined <- combined +
  #   plot_annotation(
  #     title = "Trends in Life Satisfaction Volatility",
  #     subtitle = paste0("Life Satisfaction (0-10). ", model_labels[model_id]),
  #     theme = theme(
  #       plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  #       plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5)
  #     )
  #   )
  
  return(combined)
}

# Individual country plots for slides (both axes labeled)
create_slide_lsat_volatility <- function(vol_data, country_name, variable_name, model_id = "model_5") {
  
  p <- create_volatility_plot(vol_data, country_name, variable_name, model_id,
                              show_primary_label = TRUE,
                              show_secondary_label = TRUE,
                              show_secondary_axis = TRUE)
  return(p)
}


# ============================================================
# ============================================================
# 4) VOLATILITY PLOTS - POLITICAL
# ============================================================
# ============================================================

create_publication_political_volatility <- function(vol_data, model_id = "model_5") {
  
  # UK: primary label YES, secondary label NO (left panel)
  p_uk <- create_volatility_plot(vol_data, "UK (BES Elections)", "lr", model_id,
                                 show_primary_label = TRUE,
                                 show_secondary_label = FALSE,
                                 show_secondary_axis = TRUE,
                                 custom_x_breaks = 1:5,
                                 custom_x_labels = uk_election_labels,
                                 custom_x_limits = c(0.5, 5.5))
  
  # Switzerland: primary label NO, secondary label YES (right panel)
  p_che <- create_volatility_plot(vol_data, "Switzerland (SHP)", "lrself", model_id,
                                  show_primary_label = FALSE,
                                  show_secondary_label = TRUE,
                                  show_secondary_axis = TRUE)
  
  combined <- p_uk + p_che +
    plot_layout(widths = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom")
  
  # combined <- combined +
  #   plot_annotation(
  #     title = "Trends in Political Ideology Self-Placement Volatility",
  #     subtitle = paste0("Left-Right Scale (0-10). ", model_labels[model_id]),
  #     theme = theme(
  #       plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  #       plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5)
  #     )
  #   )
  
  return(combined)
}

# Individual political plots for slides
create_slide_political_volatility <- function(vol_data, country_name, variable_name, model_id = "model_5",
                                              custom_x_breaks = NULL, custom_x_labels = NULL, custom_x_limits = NULL) {
  
  p <- create_volatility_plot(vol_data, country_name, variable_name, model_id,
                              show_primary_label = TRUE,
                              show_secondary_label = TRUE,
                              show_secondary_axis = TRUE,
                              custom_x_breaks = custom_x_breaks,
                              custom_x_labels = custom_x_labels,
                              custom_x_limits = custom_x_limits)
  return(p)
}


# ============================================================
# ============================================================
# 5) V-LINE PLOTS - LIFE SATISFACTION
# ============================================================
# ============================================================

# ============================================================
# COVID V-LINE Plot
# ============================================================

create_covid_vline_plot <- function(vline_data, metric_type = "sd") {
  
  covid_x_labels <- c("2018/2019", "2021/2022")
  
  p_ger <- create_vline_plot(vline_data, "Germany (COVID Era)", "lfsato", metric_type,
                             grouping_type = "natural",
                             show_primary_label = TRUE,
                             show_secondary_label = FALSE,
                             show_secondary_axis = TRUE,
                             custom_x_breaks = c(2018, 2021),
                             custom_x_labels = covid_x_labels,
                             custom_x_limits = c(2017.5, 2021.5))
  
  p_che <- create_vline_plot(vline_data, "Switzerland (COVID Era)", "sat_life", metric_type,
                             grouping_type = "natural",
                             show_primary_label = FALSE,
                             show_secondary_label = TRUE,
                             show_secondary_axis = TRUE,
                             custom_x_breaks = c(2018, 2021),
                             custom_x_labels = covid_x_labels,
                             custom_x_limits = c(2017.5, 2021.5))
  
  combined <- p_ger + p_che +
    plot_layout(widths = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom")
  
  title_text <- if (metric_type == "sd") {
    "Happiness Inequality: COVID Era"
  } else {
    "National Happiness: COVID Era"
  }
  
  subtitle_text <- if (metric_type == "sd") {
    "SD(Life Satisfaction) - Before/During Pandemic"
  } else {
    "Mean Life Satisfaction - Before/During Pandemic"
  }
  
  # combined <- combined +
  #   plot_annotation(
  #     title = title_text,
  #     subtitle = subtitle_text,
  #     theme = theme(
  #       plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  #       plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5)
  #     )
  #   )
  
  return(combined)
}


# V-LINE SD - 3 Countries
create_publication_lsat_vline_sd <- function(vline_data) {
  
  p_ger <- create_vline_plot(vline_data, "Germany", "lfsato", "sd",
                             grouping_type = "natural",
                             show_primary_label = TRUE,
                             show_secondary_label = FALSE,
                             show_secondary_axis = TRUE)
  p_aus <- create_vline_plot(vline_data, "Australia", "lfsato", "sd",
                             grouping_type = "natural",
                             show_primary_label = FALSE,
                             show_secondary_label = FALSE,
                             show_secondary_axis = TRUE)
  p_che <- create_vline_plot(vline_data, "Switzerland", "sat_life", "sd",
                             grouping_type = "natural",
                             show_primary_label = FALSE,
                             show_secondary_label = TRUE,
                             show_secondary_axis = TRUE)
  
  combined <- p_ger + p_aus + p_che +
    plot_layout(widths = c(3, 2, 2), guides = "collect") &
    theme(legend.position = "bottom")
  
  # combined <- combined +
  #   plot_annotation(
  #     title = "Trends in Happiness Inequality Adjusting for Stretching Effects",
  #     subtitle = "SD(Life Satisfaction)",
  #     theme = theme(
  #       plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  #       plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5)
  #     )
  #   )
  
  return(combined)
}

# V-LINE Mean - 3 Countries
create_publication_lsat_vline_mean <- function(vline_data) {
  
  p_ger <- create_vline_plot(vline_data, "Germany", "lfsato", "mean",
                             grouping_type = "natural",
                             show_primary_label = TRUE,
                             show_secondary_label = FALSE,
                             show_secondary_axis = TRUE)
  p_aus <- create_vline_plot(vline_data, "Australia", "lfsato", "mean",
                             grouping_type = "natural",
                             show_primary_label = FALSE,
                             show_secondary_label = FALSE,
                             show_secondary_axis = TRUE)
  p_che <- create_vline_plot(vline_data, "Switzerland", "sat_life", "mean",
                             grouping_type = "natural",
                             show_primary_label = FALSE,
                             show_secondary_label = TRUE,
                             show_secondary_axis = TRUE)
  
  combined <- p_ger + p_aus + p_che +
    plot_layout(widths = c(3, 2, 2), guides = "collect") &
    theme(legend.position = "bottom")
  
  # combined <- combined +
  #   plot_annotation(
  #     title = "Trends in National Happiness Adjusting for Stretching Effects",
  #     subtitle = "Mean Life Satisfaction",
  #     theme = theme(
  #       plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  #       plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5)
  #     )
  #   )
  
  return(combined)
}

# Individual V-LINE plots for slides
create_slide_lsat_vline <- function(vline_data, country_name, variable_name, metric_type = "sd") {
  
  p <- create_vline_plot(vline_data, country_name, variable_name, metric_type,
                         grouping_type = "natural",
                         show_primary_label = TRUE,
                         show_secondary_label = TRUE,
                         show_secondary_axis = TRUE)
  return(p)
}


# ============================================================
# ============================================================
# 6) V-LINE PLOTS - POLITICAL
# ============================================================
# ============================================================

create_publication_political_vline <- function(vline_data) {
  
  # UK with grouping_type = "election_cycle"
  p_uk <- create_vline_plot(vline_data, "UK (BES Elections)", "lr", "sd",
                            grouping_type = "election_cycle",
                            show_primary_label = TRUE,
                            show_secondary_label = FALSE,
                            show_secondary_axis = TRUE,
                            custom_x_breaks = 1:5,
                            custom_x_labels = uk_election_labels,
                            custom_x_limits = c(0.5, 5.5))
  
  p_che <- create_vline_plot(vline_data, "Switzerland (SHP)", "lrself", "sd",
                             grouping_type = "natural",
                             show_primary_label = FALSE,
                             show_secondary_label = TRUE,
                             show_secondary_axis = TRUE)
  
  combined <- p_uk + p_che +
    plot_layout(widths = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom")
  #
  # combined <- combined +
  #   plot_annotation(
  #     title = "Trends in Political Polarisation Adjusting for Stretching Effects",
  #     subtitle = "SD(Left-Right). Self-Assessed Political Ideology (0-10)",
  #     theme = theme(
  #       plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  #       plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5)
  #     )
  #   )
  
  return(combined)
}


# ============================================================
# ============================================================
# 7a) BES Robustness
# ============================================================
# ============================================================


create_bes_robustness_comparison <- function(vol_data) {
  
  # ============================================================
  # LEFT PANEL: Pre-election + Across-election on same plot
  # ============================================================
  
  # Get pre-election data
  pre_data <- vol_data %>%
    filter(country == "UK (BES Elections, Pre-Election)",
           variable == "lr",
           model_id == "model_5") %>%
    mutate(series = "Pre-Election (LR)")
  
  # Get across-election data
  swing_data <- vol_data %>%
    filter(country == "UK (BES Elections, Election Swing)",
           variable == "lr",
           model_id == "model_5") %>%
    mutate(series = "Across-Election (LR)")
  
  # Combine
  left_data <- bind_rows(pre_data, swing_data)
  
  # Determine y variable
  if ("indexed_within_var_interpolated" %in% names(left_data)) {
    indexed_var <- "indexed_within_var_interpolated"
  } else {
    indexed_var <- "indexed_within_var"
  }
  
  p_left <- ggplot(left_data, aes(x = time_period,
                                  y = .data[[indexed_var]],
                                  color = series)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_color_manual(
      name = "",
      values = c("Pre-Election (LR)" = "purple",
                 "Across-Election (LR)" = "darkorange")
    ) +
    scale_x_continuous(breaks = 1:5,
                       labels = uk_election_labels,
                       limits = c(0.5, 5.5)) +
    scale_y_continuous(breaks = seq(50, 125, 25),
                       limits = c(50, 125)) +
    labs(title = "Election Volatility",
         x = "",
         y = "Indexed Volatility") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "bottom"
    )
  
  # ============================================================
  # RIGHT PANEL: Quasi-Yearly Volatility (with legend and secondary axis title)
  # ============================================================
  
  # Get yearly data and relabel for legend
  yearly_data <- vol_data %>%
    filter(country == "UK (BES Yearly)",
           variable == "lr",
           model_id == "model_8") %>%
    mutate(series = "Yearly (LR)")
  
  if ("indexed_within_var_interpolated" %in% names(yearly_data)) {
    indexed_var_yearly <- "indexed_within_var_interpolated"
  } else {
    indexed_var_yearly <- "indexed_within_var"
  }
  
  # Get base raw value for secondary axis scaling
  base_raw <- yearly_data %>%
    filter(time_period == min(time_period)) %>%
    pull(mean_within_var) %>%
    first()
  
  scale_factor <- base_raw / 100
  y_breaks <- seq(50, 125, 25)
  secondary_values <- y_breaks * scale_factor
  
  p_right <- ggplot(yearly_data, aes(x = time_period,
                                     y = .data[[indexed_var_yearly]],
                                     color = series)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_color_manual(
      name = "",
      values = c("Yearly (LR)" = "steelblue")
    ) +
    scale_x_continuous(breaks = seq(2014, 2024, 2),
                       limits = c(2014, 2024)) +
    scale_y_continuous(
      name = "",
      breaks = y_breaks,
      limits = c(50, 125),
      sec.axis = sec_axis(
        trans = ~ . * scale_factor,
        name = "Absolute Volatility",
        breaks = secondary_values,
        labels = number_format(accuracy = 0.001)(secondary_values)
      )
    ) +
    labs(title = "Quasi-Yearly Volatility", x = "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "bottom"
    )
  
  # ============================================================
  # COMBINE
  # ============================================================
  
  combined <- p_left + p_right +
    plot_layout(widths = c(1, 1))
  
  # combined <- combined +
  #   plot_annotation(
  #     title = "UK Political Volatility: Alternative Measurement Windows",
  #     subtitle = "Left: Within-election (Model 5). Right: Year-to-year (Model 8, controls for days between surveys).",
  #     theme = theme(
  #       plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  #       plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5)
  #     )
  #   )
  
  return(combined)
}



# ============================================================
# ============================================================
# 7b) Nominal Stats, and Distributions
# ============================================================
# ============================================================



create_nominal_stats_plot <- function(data,
                                      country_name,
                                      variable_name,
                                      metric_type = c("sd", "mean"),
                                      grouping_type = "natural",
                                      show_primary_label = TRUE,
                                      show_secondary_label = TRUE,
                                      show_secondary_axis = TRUE,
                                      custom_x_breaks = NULL,
                                      custom_x_labels = NULL,
                                      custom_x_limits = NULL) {
  
  metric_type <- match.arg(metric_type)
  specs <- get_plot_specs(country_name, variable_name)
  
  plot_data <- data %>%
    filter(country == country_name,
           variable == variable_name)
  
  if ("grouping" %in% names(plot_data)) {
    plot_data <- plot_data %>% filter(grouping == grouping_type)
  }
  
  if (nrow(plot_data) == 0) {
    warning(paste("No nominal data for:", country_name, variable_name, grouping_type))
    return(ggplot() + theme_void() + labs(title = paste("No data:", country_name)))
  }
  
  # Set up variables and labels based on metric type and domain
  if (metric_type == "sd") {
    indexed_var <- "indexed_sd"
    raw_var <- "sd_level"
    if (specs$domain == "Political") {
      legend_label <- "sd(Left-Right)"
    } else {
      legend_label <- "sd(Life Satisfaction)"
    }
    y_label <- "Indexed Inequality (SD)"
    y_label_sec <- "Absolute Inequality (SD)"
    y_breaks <- specs$vline_sd_y_breaks
    y_limits <- specs$vline_sd_y_limits
  } else {
    indexed_var <- "indexed_mean"
    raw_var <- "mean_level"
    if (specs$domain == "Political") {
      legend_label <- "Mean Position"
    } else {
      legend_label <- "Life Satisfaction"
    }
    y_label <- "Indexed Happiness"
    y_label_sec <- "Absolute Happiness"
    y_breaks <- specs$vline_mean_y_breaks
    y_limits <- specs$vline_mean_y_limits
  }
  
  if (any(is.na(y_breaks)) || any(is.na(y_limits))) {
    warning(paste("NA y-axis specs for:", country_name, variable_name, metric_type))
    return(ggplot() + theme_void() + labs(title = paste("No specs:", country_name)))
  }
  
  # NEW: Base-period anchored scaling
  base_year <- if (grouping_type == "alternate") specs$base_year_alternate else specs$base_year_natural
  
  base_raw <- plot_data %>%
    filter(period_numeric == base_year) %>%
    pull(!!sym(raw_var)) %>%
    first()
  
  scale_factor <- base_raw / 100
  secondary_values <- y_breaks * scale_factor
  
  title_suffix <- if (grouping_type == "alternate") " (Alternate Grouping)" else ""
  plot_title <- paste0(specs$title, title_suffix)
  
  x_breaks <- if (!is.null(custom_x_breaks)) custom_x_breaks else specs$x_breaks
  x_limits <- if (!is.null(custom_x_limits)) custom_x_limits else specs$x_limits
  
  line_color <- if (specs$domain == "Political") "steelblue" else "blue"
  
  p <- ggplot(plot_data, aes(x = period_numeric)) +
    geom_line(aes(y = .data[[indexed_var]], color = legend_label),
              linewidth = 1, alpha = 0.9) +
    # geom_point(aes(y = .data[[indexed_var]], color = legend_label),
    #            size = 2.5, alpha = 0.9) +
    scale_color_manual(
      name = "",
      values = setNames(line_color, legend_label)
    ) +
    labs(title = plot_title, x = "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "bottom"
    )
  
  if (!is.null(custom_x_labels)) {
    p <- p + scale_x_continuous(breaks = x_breaks, labels = custom_x_labels, limits = x_limits)
  } else {
    p <- p + scale_x_continuous(breaks = x_breaks, limits = x_limits)
  }
  
  if (show_secondary_axis) {
    p <- p + scale_y_continuous(
      name = if (show_primary_label) y_label else "",
      breaks = y_breaks,
      limits = y_limits,
      sec.axis = sec_axis(
        trans = ~ . * scale_factor,
        name = if (show_secondary_label) y_label_sec else "",
        breaks = secondary_values,
        labels = number_format(accuracy = 0.01)(secondary_values)
      )
    )
  } else {
    p <- p + scale_y_continuous(
      name = if (show_primary_label) y_label else "",
      breaks = y_breaks,
      limits = y_limits
    )
  }
  
  return(p)
}


create_publication_lsat_nominal_sd <- function(vline_data) {
  
  p <- create_nominal_stats_plot(vline_data, "Germany", "lfsato", "sd",
                                 grouping_type = "natural",
                                 show_primary_label = TRUE,
                                 show_secondary_label = TRUE,
                                 show_secondary_axis = TRUE)
  
  p <- p +
    labs(
      subtitle = "SD(Life Satisfaction)") +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5)
    )
  
  return(p)
}

create_publication_lsat_nominal_mean <- function(vline_data) {
  
  p <- create_nominal_stats_plot(vline_data, "Germany", "lfsato", "mean",
                                 grouping_type = "natural",
                                 show_primary_label = TRUE,
                                 show_secondary_label = TRUE,
                                 show_secondary_axis = TRUE)
  
  p <- p +
    labs(
      subtitle = "Mean Life Satisfaction") +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5)
    )
  
  return(p)
}


create_publication_political_nominal_sd <- function(vline_data) {
  
  p <- create_nominal_stats_plot(vline_data, "UK (BES Elections)", "lr", "sd",
                                 grouping_type = "election_cycle",
                                 show_primary_label = TRUE,
                                 show_secondary_label = TRUE,
                                 show_secondary_axis = TRUE,
                                 custom_x_breaks = 1:5,
                                 custom_x_labels = uk_election_labels,
                                 custom_x_limits = c(0.5, 5.5))
  
  p <- p +
    labs(
      subtitle = "SD(Left-Right Self-Placement)") +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5)
    )
  
  return(p)
}


create_publication_lsat_nominal_sd_combined <- function(vline_data) {
  
  p_ger <- create_nominal_stats_plot(vline_data, "Germany", "lfsato", "sd",
                                     grouping_type = "natural",
                                     show_primary_label = TRUE,
                                     show_secondary_label = FALSE,
                                     show_secondary_axis = TRUE)
  p_aus <- create_nominal_stats_plot(vline_data, "Australia", "lfsato", "sd",
                                     grouping_type = "natural",
                                     show_primary_label = FALSE,
                                     show_secondary_label = FALSE,
                                     show_secondary_axis = TRUE)
  p_che <- create_nominal_stats_plot(vline_data, "Switzerland", "sat_life", "sd",
                                     grouping_type = "natural",
                                     show_primary_label = FALSE,
                                     show_secondary_label = TRUE,
                                     show_secondary_axis = TRUE)
  
  combined <- p_ger + p_aus + p_che +
    plot_layout(widths = c(3, 2, 2), guides = "collect") &
    theme(legend.position = "bottom")
  
  # combined <- combined +
  #   plot_annotation(
  #     title = "Nominal Trends in Happiness Inequality",
  #     subtitle = "SD(Life Satisfaction) - No Adjustment for Scale Stretching",
  #     theme = theme(
  #       plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  #       plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5)
  #     )
  #   )
  
  return(combined)
}


create_publication_political_nominal_sd_combined <- function(vline_data) {
  
  p_uk <- create_nominal_stats_plot(vline_data, "UK (BES Elections)", "lr", "sd",
                                    grouping_type = "election_cycle",
                                    show_primary_label = TRUE,
                                    show_secondary_label = FALSE,
                                    show_secondary_axis = TRUE,
                                    custom_x_breaks = 1:5,
                                    custom_x_labels = uk_election_labels,
                                    custom_x_limits = c(0.5, 5.5))
  
  p_che <- create_nominal_stats_plot(vline_data, "Switzerland (SHP)", "lrself", "sd",
                                     grouping_type = "natural",
                                     show_primary_label = FALSE,
                                     show_secondary_label = TRUE,
                                     show_secondary_axis = TRUE)
  
  combined <- p_uk + p_che +
    plot_layout(widths = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom")
  
  # combined <- combined +
  #   plot_annotation(
  #     title = "Nominal Trends in Political Polarisation",
  #     subtitle = "SD(Left-Right) - No Adjustment for Scale Stretching",
  #     theme = theme(
  #       plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  #       plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5)
  #     )
  #   )
  
  return(combined)
}

# ============================================================
# 10f) MECHANISM: NOMINAL SD + VOLATILITY (Model 5, No Controls)
# No benchmark data, no points.
# ============================================================

create_mechanism_panel <- function(vol_data, vline_data, country_name, variable_name,
                                   model_ids = c("model_5", "model_1"),
                                   grouping_type = "natural",
                                   show_primary_label = TRUE,
                                   show_secondary_label = FALSE,
                                   custom_x_breaks = NULL, custom_x_labels = NULL,
                                   custom_x_limits = NULL) {
  
  specs  <- get_plot_specs(country_name, variable_name)
  prefix <- if (specs$domain == "Political") "LR" else "LS"
  nom_label  <- paste0("Nominal SD(", prefix, ")")
  vol_labels <- paste0(prefix, " Volatility (", model_display[model_ids], ")")
  level_order <- c(nom_label, vol_labels)
  
  # Nominal SD (same source as the V-LINE plots)
  nominal <- vline_data %>%
    filter(country == country_name, variable == variable_name)
  if ("grouping" %in% names(nominal)) nominal <- nominal %>% filter(grouping == grouping_type)
  nominal <- nominal %>%
    transmute(x = period_numeric, value = indexed_sd, series = nom_label)
  
  # Volatility lines (same filtering as the overlay plots)
  vol_lines <- lapply(model_ids, function(mid) {
    d <- vol_data %>%
      filter(country == country_name, variable == variable_name, model_id == mid)
    if (specs$time_var == "year" && !grepl("COVID", country_name)) {
      d <- d %>%
        mutate(years_since_start = time_period - min(time_period)) %>%
        filter(years_since_start %% 2 == 0)
    }
    d %>% transmute(x = time_period, value = indexed_within_var_interpolated,
                    series = paste0(prefix, " Volatility (", model_display[[mid]], ")"))
  }) %>% bind_rows()
  
  plot_data <- bind_rows(nominal, vol_lines) %>%
    mutate(series = factor(series, levels = level_order))
  
  cols <- setNames(c("blue",  ifelse(model_ids == "model_5", "darkslategrey", "grey55")), level_order)
  ltys <- setNames(c("solid", ifelse(model_ids == "model_5", "dashed",        "dotted")), level_order)
  
  x_breaks <- if (!is.null(custom_x_breaks)) custom_x_breaks else specs$x_breaks
  x_limits <- if (!is.null(custom_x_limits)) custom_x_limits else specs$x_limits
  x_labels <- if (!is.null(custom_x_labels)) custom_x_labels else waiver()
  
  # Same y-axis as the V-LINE plots underneath
  y_breaks <- specs$vline_sd_y_breaks
  y_limits <- specs$vline_sd_y_limits
  
  ggplot(plot_data, aes(x = x, y = value, colour = series, linetype = series)) +
    geom_hline(yintercept = 100, colour = "grey70", linewidth = 0.4) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_colour_manual(name = "", values = cols, limits = level_order) +
    scale_linetype_manual(name = "", values = ltys, limits = level_order) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = x_limits) +
    scale_y_continuous(
      breaks = y_breaks, limits = y_limits,
      # Dummy secondary axis, printed in white, so panels line up with the V-LINE plots
      sec.axis = sec_axis(~ ., breaks = y_breaks,
                          labels = rep("0.00", length(y_breaks)),
                          name = if (show_secondary_label) "Absolute Inequality (SD)" else "")
    ) +
    labs(title = specs$title, x = "",
         y = if (show_primary_label) "Index (First Period = 100)" else "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "bottom",
      legend.key.width = unit(1.5, "cm"),
      axis.text.y.right  = element_text(colour = "white"),
      axis.title.y.right = element_text(colour = "white")
    )
}


create_publication_lsat_mechanism <- function(vol_data, vline_data,
                                              model_ids = c("model_5", "model_1")) {
  p_ger <- create_mechanism_panel(vol_data, vline_data, "Germany", "lfsato",
                                  model_ids = model_ids,
                                  show_primary_label = TRUE,
                                  show_secondary_label = FALSE)
  p_aus <- create_mechanism_panel(vol_data, vline_data, "Australia", "lfsato",
                                  model_ids = model_ids,
                                  show_primary_label = FALSE,
                                  show_secondary_label = FALSE)
  p_che <- create_mechanism_panel(vol_data, vline_data, "Switzerland", "sat_life",
                                  model_ids = model_ids,
                                  show_primary_label = FALSE,
                                  show_secondary_label = TRUE)
  p_ger + p_aus + p_che +
    plot_layout(widths = c(3, 2, 2), guides = "collect") &
    theme(legend.position = "bottom")
}

create_publication_political_mechanism <- function(vol_data, vline_data,
                                                   model_ids = c("model_5", "model_1")) {
  p_uk  <- create_mechanism_panel(vol_data, vline_data, "UK (BES Elections)", "lr",
                                  model_ids = model_ids,
                                  grouping_type = "election_cycle",
                                  show_primary_label = TRUE,
                                  show_secondary_label = FALSE,
                                  custom_x_breaks = 1:5,
                                  custom_x_labels = unname(uk_election_labels),
                                  custom_x_limits = c(0.5, 5.5))
  p_che <- create_mechanism_panel(vol_data, vline_data, "Switzerland (SHP)", "lrself",
                                  model_ids = model_ids,
                                  show_primary_label = FALSE,
                                  show_secondary_label = TRUE)
  p_uk + p_che +
    plot_layout(widths = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom")
}


# ============================================================
# DISPLAY SPECIFICATION TABLE (title, panel width per panel)
# ============================================================

distribution_plot_specs <- tribble(
  ~country,             ~domain,             ~title,        ~panel_width,
  
  "Germany",             "Life Satisfaction", "Germany",     3,
  "Australia",           "Life Satisfaction", "Australia",   2,
  "Switzerland",         "Life Satisfaction", "Switzerland", 2,
  
  "UK (BES Elections)",  "Political",         "Britain",     2,
  "Switzerland (SHP)",   "Political",         "Switzerland", 2
)

# ============================================================
# COLOR PALETTE
# ============================================================

period_colors <- c("First" = "#2c7bb6", "Last" = "#d7191c")

# UK election index -> display label (election is coded 1-5, not a year)
uk_election_labels <- c(
  "1" = "GE '15", "2" = "Brexit '16", "3" = "GE '17", "4" = "GE '19", "5" = "GE '24"
)

# ============================================================
# HELPER FUNCTION: pull a single spec row as a list
# ============================================================

get_distribution_specs <- function(country_name, domain_name) {
  
  specs <- distribution_plot_specs %>%
    filter(country == country_name, domain == domain_name)
  
  if (nrow(specs) == 0) {
    stop("No specs found for: ", country_name, " / ", domain_name)
  }
  
  as.list(specs[1, ])
}

create_distribution_plot <- function(dist_data,
                                     country_name,
                                     domain_name,
                                     show_primary_label = TRUE,
                                     show_legend = TRUE,
                                     colors = period_colors) {
  
  specs <- get_distribution_specs(country_name, domain_name)
  
  plot_data <- dist_data %>%
    filter(country == country_name, domain == domain_name)
  
  if (nrow(plot_data) == 0) {
    warning(paste("No distribution data for:", country_name, domain_name))
    return(ggplot() + theme_void() + labs(title = paste("No data:", country_name)))
  }
  
  first_period <- unique(plot_data$first_period)
  last_period  <- unique(plot_data$last_period)
  
  # UK's time_var is an election-cycle index (1-5), not a year -
  # display via uk_election_labels rather than the raw integer
  if (country_name == "UK (BES Elections)") {
    first_display <- uk_election_labels[as.character(first_period)]
    last_display  <- uk_election_labels[as.character(last_period)]
  } else {
    first_display <- first_period
    last_display  <- last_period
  }
  
  plot_title <- paste0(specs$title, " (", first_display, " vs ", last_display, ")")
  
  ggplot(plot_data, aes(x = category, y = proportion, fill = period)) +
    geom_col(position = "dodge", width = 0.8) +
    scale_x_continuous(breaks = 0:10) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(name = "", values = colors) +
    labs(
      title = plot_title,
      x = "",
      y = if (show_primary_label) "Weighted Share of Respondents" else ""
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = if (show_legend) "bottom" else "none"
    )
}


create_publication_lsat_distribution <- function(dist_data) {
  
  p_ger <- create_distribution_plot(dist_data, "Germany", "Life Satisfaction",
                                    show_primary_label = TRUE)
  p_aus <- create_distribution_plot(dist_data, "Australia", "Life Satisfaction",
                                    show_primary_label = FALSE)
  p_che <- create_distribution_plot(dist_data, "Switzerland", "Life Satisfaction",
                                    show_primary_label = FALSE)
  
  specs_ger <- get_distribution_specs("Germany", "Life Satisfaction")
  specs_aus <- get_distribution_specs("Australia", "Life Satisfaction")
  specs_che <- get_distribution_specs("Switzerland", "Life Satisfaction")
  
  combined <- p_ger + p_aus + p_che +
    plot_layout(widths = c(specs_ger$panel_width, specs_aus$panel_width, specs_che$panel_width),
                guides = "collect") &
    theme(legend.position = "bottom")
  
  # combined +
  #   plot_annotation(
  #     title = "Distribution of Responses: First vs Last Period",
  #     subtitle = "Life Satisfaction (0-10)",
  #     theme = theme(
  #       plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  #       plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5)
  #     )
  #   )
}



create_publication_political_distribution <- function(dist_data) {
  
  p_uk <- create_distribution_plot(dist_data, "UK (BES Elections)", "Political",
                                   show_primary_label = TRUE)
  p_che <- create_distribution_plot(dist_data, "Switzerland (SHP)", "Political",
                                    show_primary_label = FALSE)
  
  specs_uk  <- get_distribution_specs("UK (BES Elections)", "Political")
  specs_che <- get_distribution_specs("Switzerland (SHP)", "Political")
  
  combined <- p_uk + p_che +
    plot_layout(widths = c(specs_uk$panel_width, specs_che$panel_width),
                guides = "collect") &
    theme(legend.position = "bottom")
  
  # combined +
  #   plot_annotation(
  #     title = "Distribution of Responses: First vs Last Period",
  #     subtitle = "Left-Right Self-Placement (0-10)",
  #     theme = theme(
  #       plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  #       plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5)
  #     )
  #   )
}








# ============================================================
# ============================================================
# Main Sections:
# 8) Income Validation
# ============================================================
# ============================================================


get_lincat_axis_params <- function(country_name) {
  if (country_name == "Germany") {
    list(breaks = seq(40, 140, by = 20), limits = c(60, 150))
  } else if (country_name == "Switzerland") {
    list(breaks = seq(60, 140, by = 20), limits = c(60, 150))
  } else {
    list(breaks = seq(60, 140, by = 20), limits = c(60, 150))
  }
}

get_lincat_plot_layout <- function(country_name) {
  if (country_name == "Germany") {
    list(x_breaks = seq(1985, 2020, by = 5), x_limits = c(1984, 2019),
         plot_title = "Germany", show_y_label = TRUE, panel_width = 3)
  } else if (country_name == "Switzerland") {
    list(x_breaks = seq(2000, 2020, by = 5), x_limits = c(1999, 2019),
         plot_title = "Switzerland", show_y_label = FALSE, panel_width = 2)
  } else {
    list(x_breaks = seq(2000, 2020, by = 5), x_limits = c(2001, 2019),
         plot_title = "Australia", show_y_label = FALSE, panel_width = 2)
  }
}


plot_lincat_volatilities <- function(vline_data, country_name, add_stretching = FALSE) {
  
  layout <- get_lincat_plot_layout(country_name)
  axis_params <- get_lincat_axis_params(country_name)
  
  plot_data <- vline_data %>%
    filter(country == country_name, !is.na(indexed_mean_income_level)) %>%
    rename(year = time_period)
  
  if (add_stretching) {
    plot_long <- plot_data %>%
      mutate(vline = stretching_factor * 100) %>%
      select(year,
             `Volatility: Linear Income Category` = indexed_lincat_volatility,
             `Volatility: Absolute Income` = indexed_income_volatility,
             `Estimated Stretching` = vline) %>%
      pivot_longer(-year, names_to = "series", values_to = "value")
    
    color_vals <- c("Volatility: Linear Income Category" = "darkslategrey",
                    "Volatility: Absolute Income" = "#00BFFF",
                    "Estimated Stretching" = "red")
    linetype_vals <- c("Volatility: Linear Income Category" = "solid",
                       "Volatility: Absolute Income" = "solid",
                       "Estimated Stretching" = "twodash")
  } else {
    plot_long <- plot_data %>%
      select(year,
             `Volatility: Linear Income Category` = indexed_lincat_volatility,
             `Volatility: Absolute Income` = indexed_income_volatility) %>%
      pivot_longer(-year, names_to = "series", values_to = "value")
    
    color_vals <- c("Volatility: Linear Income Category" = "darkslategrey",
                    "Volatility: Absolute Income" = "#00BFFF")
    linetype_vals <- c("Volatility: Linear Income Category" = "solid",
                       "Volatility: Absolute Income" = "solid")
  }
  
  ggplot(plot_long, aes(x = year, y = value, color = series, linetype = series)) +
    geom_line(linewidth = 1.0) +
    scale_color_manual(values = color_vals, name = NULL) +
    scale_linetype_manual(values = linetype_vals, name = NULL) +
    scale_x_continuous(breaks = layout$x_breaks, limits = layout$x_limits) +
    scale_y_continuous(breaks = axis_params$breaks, limits = axis_params$limits) +
    labs(title = layout$plot_title, x = "", y = if (layout$show_y_label) "Indexed Volatility" else "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title.y = element_text(size = 14)
    )
}


plot_lincat_inequality_validation <- function(vline_data, country_name, show_vline = TRUE) {
  
  axis_params <- get_lincat_axis_params(country_name)
  layout <- get_lincat_plot_layout(country_name)
  
  plot_data <- vline_data %>%
    filter(country == country_name, !is.na(indexed_mean_income_level)) %>%
    rename(year = time_period)
  
  if (show_vline) {
    plot_long <- plot_data %>%
      select(year,
             `Nominal Inequality (SD of Linear Category)` = indexed_sd_lincat_level,
             `Actual Inequality (SD of Income)` = indexed_sd_income_level,
             `SD Latent Income (V-LINE)` = adjusted_sd_lincat) %>%
      pivot_longer(-year, names_to = "series", values_to = "value")
    
    color_vals <- c("Nominal Inequality (SD of Linear Category)" = "blue",
                    "Actual Inequality (SD of Income)" = "limegreen",
                    "SD Latent Income (V-LINE)" = "red")
  } else {
    plot_long <- plot_data %>%
      select(year,
             `Nominal Inequality (SD of Linear Category)` = indexed_sd_lincat_level,
             `Actual Inequality (SD of Income)` = indexed_sd_income_level) %>%
      pivot_longer(-year, names_to = "series", values_to = "value")
    
    color_vals <- c("Nominal Inequality (SD of Linear Category)" = "blue",
                    "Actual Inequality (SD of Income)" = "limegreen")
  }
  
  ggplot(plot_long, aes(x = year, y = value, color = series)) +
    geom_line(linewidth = 1.0) +
    scale_color_manual(values = color_vals, name = NULL) +
    scale_x_continuous(breaks = layout$x_breaks, limits = layout$x_limits) +
    scale_y_continuous(breaks = axis_params$breaks, limits = axis_params$limits) +
    labs(title = layout$plot_title, x = "", y = if (layout$show_y_label) "Indexed Income Inequality" else "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title.y = element_text(size = 14)
    )
}





get_income_homoskedasticity_axis_params <- function(country_name) {
  list(breaks = seq(80, 140, by = 20), limits = c(80, 150))
}

create_income_homoskedasticity_plot <- function(country_name, show_primary_label = TRUE, show_legend = TRUE) {
  
  axis_params <- get_income_homoskedasticity_axis_params(country_name)
  layout <- get_lincat_plot_layout(country_name)
  
  lab_nominal  <- "Nominal (SD of income category)"
  lab_const    <- "V-LINE, constant latent volatility"
  lab_observed <- "V-LINE, observed latent volatility"
  lab_actual   <- "Actual (SD of household income)"
  
  plot_data <- lincat_vline %>%
    filter(country == country_name, !is.na(indexed_sd_lincat_level)) %>%
    mutate(
      homoskedastic_stretching_factor = 100 / indexed_lincat_volatility,
      homoskedastic_sd_lincat = indexed_sd_lincat_level * homoskedastic_stretching_factor
    ) %>%
    transmute(
      year = time_period,
      !!lab_nominal  := indexed_sd_lincat_level,
      !!lab_const    := homoskedastic_sd_lincat,
      !!lab_observed := adjusted_sd_lincat,
      !!lab_actual   := indexed_sd_income_level
    ) %>%
    pivot_longer(-year, names_to = "series", values_to = "value")
  
  series_order <- c(lab_nominal, lab_const, lab_observed, lab_actual)
  
  income_homoskedasticity_colors <- setNames(c("blue", "red", "red", "limegreen"), series_order)
  income_homoskedasticity_linetypes <- setNames(c("solid", "dotted", "solid", "solid"), series_order)
  
  ggplot(plot_data, aes(x = year, y = value, color = series, linetype = series)) +
    geom_line(linewidth = 1.0) +
    scale_color_manual(values = income_homoskedasticity_colors, breaks = series_order, name = NULL) +
    scale_linetype_manual(values = income_homoskedasticity_linetypes, breaks = series_order, name = NULL) +
    scale_x_continuous(breaks = layout$x_breaks, limits = layout$x_limits) +
    scale_y_continuous(breaks = axis_params$breaks, limits = axis_params$limits) +
    labs(title = layout$plot_title, x = "", y = if (show_primary_label) "Indexed Income Inequality" else "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      legend.position = if (show_legend) "bottom" else "none",
      legend.text = element_text(size = 12),
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title.y = element_text(size = 14)
    )
}


# create_income_homoskedasticity_plot <- function(country_name, show_primary_label = TRUE, show_legend = TRUE) {
#
#   axis_params <- get_income_homoskedasticity_axis_params(country_name)
#   layout <- get_lincat_plot_layout(country_name)
#
#   plot_data <- lincat_vline %>%
#     filter(country == country_name, !is.na(indexed_sd_lincat_level)) %>%
#     mutate(
#       homoskedastic_stretching_factor = 100 / indexed_lincat_volatility,
#       homoskedastic_sd_lincat = indexed_sd_lincat_level * homoskedastic_stretching_factor
#     ) %>%
#     transmute(
#       year = time_period,
#       `Inequality Estimate (V-LINE, with Assumption 4)` = homoskedastic_sd_lincat,
#       `Inequality Estimate (V-LINE, no Assumption 4)` = adjusted_sd_lincat,
#       `Actual Inequality (SD of Income)` = indexed_sd_income_level
#     ) %>%
#     pivot_longer(-year, names_to = "series", values_to = "value")
#
#   income_homoskedasticity_colors <- c(
#     "Inequality Estimate (V-LINE, with Assumption 4)" = "red",
#     "Inequality Estimate (V-LINE, no Assumption 4)" = "red",
#     "Actual Inequality (SD of Income)" = "limegreen"
#   )
#   income_homoskedasticity_linetypes <- c(
#     "Inequality Estimate (V-LINE, with Assumption 4)" = "dotted",
#     "Inequality Estimate (V-LINE, no Assumption 4)" = "solid",
#     "Actual Inequality (SD of Income)" = "solid"
#   )
#
#   ggplot(plot_data, aes(x = year, y = value, color = series, linetype = series)) +
#     geom_line(linewidth = 1.0) +
#     scale_color_manual(values = income_homoskedasticity_colors, name = NULL) +
#     scale_linetype_manual(values = income_homoskedasticity_linetypes, name = NULL) +
#     scale_x_continuous(breaks = layout$x_breaks, limits = layout$x_limits) +
#     scale_y_continuous(breaks = axis_params$breaks, limits = axis_params$limits) +
#     labs(title = layout$plot_title, x = "", y = if (show_primary_label) "Indexed Income Inequality" else "") +
#     theme_minimal(base_size = 14) +
#     theme(
#       panel.border = element_rect(color = "black", fill = NA),
#       panel.grid.minor = element_blank(),
#       legend.position = if (show_legend) "bottom" else "none",
#       legend.text = element_text(size = 12),
#       plot.title = element_text(size = 16, hjust = 0.5),
#       axis.title.y = element_text(size = 14)
#     )
# }


plot_lincat_mean_validation <- function(vline_data, country_name, show_vline = TRUE) {
  
  axis_params <- get_lincat_axis_params(country_name)
  layout <- get_lincat_plot_layout(country_name)
  
  plot_data <- vline_data %>%
    filter(country == country_name, !is.na(indexed_mean_income_level)) %>%
    rename(year = time_period)
  
  if (show_vline) {
    plot_long <- plot_data %>%
      select(year,
             `Nominal Mean (Linear Category)` = indexed_mean_lincat_level,
             `Actual Mean Income` = indexed_mean_income_level,
             `V-LINE` = adjusted_mean_lincat) %>%
      pivot_longer(-year, names_to = "series", values_to = "value")
    
    color_vals <- c("Nominal Mean (Linear Category)" = "blue",
                    "Actual Mean Income" = "limegreen",
                    "V-LINE" = "red")
  } else {
    plot_long <- plot_data %>%
      select(year,
             `Nominal Mean (Linear Category)` = indexed_mean_lincat_level,
             `Actual Mean Income` = indexed_mean_income_level) %>%
      pivot_longer(-year, names_to = "series", values_to = "value")
    
    color_vals <- c("Nominal Mean (Linear Category)" = "blue",
                    "Actual Mean Income" = "limegreen")
  }
  
  ggplot(plot_long, aes(x = year, y = value, color = series)) +
    geom_line(linewidth = 1.0) +
    scale_color_manual(values = color_vals, name = NULL) +
    scale_x_continuous(breaks = layout$x_breaks, limits = layout$x_limits) +
    scale_y_continuous(breaks = axis_params$breaks, limits = axis_params$limits) +
    labs(title = layout$plot_title, x = "", y = if (layout$show_y_label) "Indexed Income Level" else "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title.y = element_text(size = 14)
    )
}





# ============================================================
# ============================================================
# Main Sections:
# 9) V-LINE with non-linearities
# ============================================================
# ============================================================


library(tidyverse)
library(patchwork)


# ============================================================
# PARSE TRANSFORMATION INFO
# ============================================================

all_vline_robustness <- all_vline_robustness %>%
  mutate(
    # Extract cost level
    cost = case_when(
      grepl("c0.00", variable) ~ 0.00,
      grepl("c0.05", variable) ~ 0.05,
      grepl("c0.10", variable) ~ 0.10,
      grepl("c0.20", variable) ~ 0.20
    ),
    # Extract transformation type
    transform_type = case_when(
      grepl("linear", variable) ~ "V-LINE: Linear Reporting",
      grepl("power_convex", variable) ~ "Convex",
      grepl("power_concave", variable) ~ "Concave",
      grepl("inv_s_curve", variable) ~ "Inverse-S"
    ),
    # Create display label
    cost_label = paste0("C = ", sprintf("%.2f", cost))
  )


# ============================================================
# COLOUR PALETTES (gradients by cost)
# ============================================================

# Convex: Blue gradient (lighter = lower cost)
convex_colours <- c(
  "Nominal LS" = "grey50",
  "V-LINE: Linear Reporting (C=0)" = "black",
  "C = 0.05" = "lightblue",
  "C = 0.10" = "#4292C6",
  "C = 0.20" = "#08519C"
)

# Concave: Red gradient (lighter = lower cost)
concave_colours <- c(
  "Nominal LS" = "grey50",
  "V-LINE: Linear Reporting (C=0)" = "black",
  "C = 0.05" = "#FCAE91",
  "C = 0.10" = "#FB6A4A",
  "C = 0.20" = "#CB181D"
)

# Inverse-S: Purple gradient (lighter = lower cost)
inv_s_colours <- c(
  "Nominal LS" = "grey50",
  "V-LINE: Linear Reporting (C=0)" = "black",
  "C = 0.05" = "#BCBDDC",
  "C = 0.10" = "#807DBA",
  "C = 0.20" = "#54278F"
)

# For summary plot at C = 0.1
summary_colours <- c(
  "Nominal LS" = "grey50",
  "V-LINE: Linear Reporting" = "#000000",
  "Convex" = "#4292C6",
  "Concave" = "#FB6A4A",
  "Inverse-S" = "#807DBA"
)

# ============================================================
# PLOTTING FUNCTION: Single transformation type with gradient
# ============================================================

create_transform_gradient_plot <- function(data,
                                           transform_type_filter,
                                           country_name,
                                           metric_type = c("mean", "sd"),
                                           colour_palette,
                                           y_limits = NULL) {
  
  metric_type <- match.arg(metric_type)
  
  # Get linear data
  linear_data <- data %>%
    filter(country == country_name,
           grouping == "natural",
           transform_type == "V-LINE: Linear Reporting") %>%
    mutate(cost_label = "V-LINE: Linear Reporting (C=0)")
  
  # Create nominal LS data (for legend)
  nominal_data <- linear_data %>%
    mutate(cost_label = "Nominal LS")
  
  # Get transformation data (exclude linear, filter to this type)
  transform_data <- data %>%
    filter(country == country_name,
           grouping == "natural",
           transform_type == transform_type_filter,
           cost > 0)  # Exclude C = 0 (that's linear)
  
  # Determine y variable
  if (metric_type == "mean") {
    y_var <- "indexed_adjusted_mean"
    nominal_var <- "indexed_mean"
    y_label <- "Indexed Happiness Level"
  } else {
    y_var <- "indexed_adjusted_sd"
    nominal_var <- "indexed_sd"
    y_label <- "Indexed Inequality (SD)"
  }
  
  # Set axis parameters by country
  if (country_name == "Germany") {
    x_breaks <- seq(1985, 2015, by = 5)
    x_limits <- c(1984, 2019)
    year_range <- "1984-2019"
  } else {
    x_breaks <- seq(2000, 2015, by = 5)
    x_limits <- c(2000, 2019)
    year_range <- "2001-2019"
  }
  
  # Default y-limits
  if (is.null(y_limits)) {
    y_limits <- if (metric_type == "mean") c(90, 140) else c(80, 120)
  }
  
  # Set factor levels for legend ordering
  level_order <- names(colour_palette)
  
  # Build plot
  p <- ggplot() +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "grey60", linewidth = 0.5)
  
  # Add nominal LS line (in legend)
  if (nrow(nominal_data) > 0) {
    p <- p +
      geom_line(data = nominal_data,
                aes(x = period_numeric, y = !!sym(nominal_var), colour = cost_label),
                linewidth = 0.8, linetype = "dashed") +
      geom_point(data = nominal_data,
                 aes(x = period_numeric, y = !!sym(nominal_var), colour = cost_label),
                 size = 2, shape = 1)
  }
  
  # Add linear V-LINE baseline (black line + points)
  if (nrow(linear_data) > 0) {
    p <- p +
      geom_line(data = linear_data,
                aes(x = period_numeric, y = !!sym(y_var), colour = cost_label),
                linewidth = 0.8) +
      geom_point(data = linear_data,
                 aes(x = period_numeric, y = !!sym(y_var), colour = cost_label),
                 size = 2)
  }
  
  # Add transformation POINTS ONLY (no lines) with gradient colours
  if (nrow(transform_data) > 0) {
    p <- p +
      geom_point(data = transform_data,
                 aes(x = period_numeric, y = !!sym(y_var),
                     colour = cost_label),
                 size = 2, alpha = 0.9)
  }
  
  p <- p +
    scale_colour_manual(name = "",
                        values = colour_palette,
                        limits = level_order) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits) +
    scale_y_continuous(limits = y_limits) +
    labs(
      title = paste0(country_name, " (", year_range, ")"),
      x = "Year",
      y = y_label
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      plot.title = element_text(hjust = 0.5, size = 16),
      legend.position = "bottom",
      legend.text = element_text(size = 10)
    )
  
  return(p)
}

# ============================================================
# COMBINED PLOT FUNCTION: Both countries side-by-side
# ============================================================

create_combined_transform_plot <- function(data,
                                           transform_type_filter,
                                           metric_type = c("mean", "sd"),
                                           colour_palette,
                                           y_limits = NULL,
                                           main_title = NULL) {
  
  metric_type <- match.arg(metric_type)
  
  p_germany <- create_transform_gradient_plot(
    data = data,
    transform_type_filter = transform_type_filter,
    country_name = "Germany",
    metric_type = metric_type,
    colour_palette = colour_palette,
    y_limits = y_limits
  )
  
  p_australia <- create_transform_gradient_plot(
    data = data,
    transform_type_filter = transform_type_filter,
    country_name = "Australia",
    metric_type = metric_type,
    colour_palette = colour_palette,
    y_limits = y_limits
  )
  
  combined <- p_germany + p_australia +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # if (!is.null(main_title)) {
  #   combined <- combined +
  #     plot_annotation(
  #       title = main_title,
  #       theme = theme(
  #         plot.title = element_text(size = 15, hjust = 0.5, face="bold")
  #       )
  #     )
  # }
  
  return(combined)
}

# ============================================================
# SUMMARY PLOT: All 3 types at C = 0.1
# ============================================================

create_summary_plot <- function(data,
                                country_name,
                                metric_type = c("mean", "sd"),
                                y_limits = NULL) {
  
  metric_type <- match.arg(metric_type)
  
  # Get linear data
  linear_data <- data %>%
    filter(country == country_name,
           grouping == "natural",
           transform_type == "V-LINE: Linear Reporting") %>%
    mutate(display_type = "V-LINE: Linear Reporting")
  
  # Create nominal LS data (for legend)
  nominal_data <- linear_data %>%
    mutate(display_type = "Nominal LS")
  
  # Get C = 0.1 data for all transformation types
  c01_data <- data %>%
    filter(country == country_name,
           grouping == "natural",
           cost == 0.10,
           transform_type != "V-LINE: Linear Reporting") %>%
    mutate(display_type = transform_type)
  
  plot_data <- bind_rows(linear_data, c01_data)
  
  # Determine y variable
  if (metric_type == "mean") {
    y_var <- "indexed_adjusted_mean"
    nominal_var <- "indexed_mean"
    y_label <- "Indexed Happiness Level"
  } else {
    y_var <- "indexed_adjusted_sd"
    nominal_var <- "indexed_sd"
    y_label <- "Indexed Inequality (SD)"
  }
  
  # Set axis parameters
  if (country_name == "Germany") {
    x_breaks <- seq(1985, 2020, by = 5)
    x_limits <- c(1984, 2019)
    year_range <- "1984-2019"
  } else {
    x_breaks <- seq(2000, 2020, by = 5)
    x_limits <- c(2000, 2019)
    year_range <- "2001-2019"
  }
  
  if (is.null(y_limits)) {
    y_limits <- if (metric_type == "mean") c(90, 140) else c(80, 120)
  }
  
  # Set factor levels for legend ordering
  level_order <- names(summary_colours)
  
  p <- ggplot() +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "grey60", linewidth = 0.5)
  
  # Add nominal LS line (in legend)
  if (nrow(nominal_data) > 0) {
    p <- p +
      geom_line(data = nominal_data,
                aes(x = period_numeric, y = !!sym(nominal_var), colour = display_type),
                linewidth = 0.8, linetype = "dashed") +
      geom_point(data = nominal_data,
                 aes(x = period_numeric, y = !!sym(nominal_var), colour = display_type),
                 size = 2, shape = 1)
  }
  
  # Add Linear with line + points
  p <- p +
    geom_line(data = filter(plot_data, display_type == "V-LINE: Linear Reporting"),
              aes(x = period_numeric, y = !!sym(y_var),
                  colour = display_type),
              linewidth = 0.8) +
    geom_point(data = filter(plot_data, display_type == "V-LINE: Linear Reporting"),
               aes(x = period_numeric, y = !!sym(y_var),
                   colour = display_type),
               size = 2)
  
  # Add non-linear transformations as points only (same shape)
  p <- p +
    geom_point(data = filter(plot_data, display_type != "V-LINE: Linear Reporting"),
               aes(x = period_numeric, y = !!sym(y_var),
                   colour = display_type),
               size = 2, alpha = 0.9)
  
  p <- p +
    scale_colour_manual(name = "",
                        values = summary_colours,
                        limits = level_order) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits) +
    scale_y_continuous(limits = y_limits) +
    labs(
      title = paste0(country_name, " (", year_range, ")"),
      x = "Year",
      y = y_label
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      plot.title = element_text(hjust = 0.5, size = 16),
      legend.position = "bottom"
    ) +
    guides(colour = guide_legend(nrow = 1))
  
  return(p)
}

create_combined_summary_plot <- function(data,
                                         metric_type = c("mean", "sd"),
                                         y_limits = NULL,
                                         main_title = NULL) {
  
  metric_type <- match.arg(metric_type)
  
  p_germany <- create_summary_plot(data, "Germany", metric_type, y_limits)
  p_australia <- create_summary_plot(data, "Australia", metric_type, y_limits)
  
  combined <- p_germany + p_australia +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # if (!is.null(main_title)) {
  #   combined <- combined +
  #     plot_annotation(
  #       title = main_title,
  #       theme = theme(
  #         plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  #       )
  #     )
  # }
  
  return(combined)
}


# ============================================================
# ============================================================
# 9b) V-LINE NON-LINEARITY ROBUSTNESS — UK
# Mirrors Section 9's Germany/Australia gradient plots, adapted
# for BES's structure: single "election_cycle" grouping (no
# natural/alternate distinction), election-index x-axis, and SD
# only — adjusted_mean is always NA for BES (see run_vline_
# analysis_bes_election), so there is no UK mean-robustness plot.
# Nominal series is Left-Right, so palettes are relabelled
# locally ("Nominal LS" -> "Nominal LR"); shared palettes untouched.
# ============================================================
# ============================================================

all_vline_robustness_uk <- read_csv(file.path(modeloutput_path, "v_line_nonlinear_uk.csv"), show_col_types = FALSE) %>%
  mutate(
    cost = case_when(
      grepl("c0.00", variable) ~ 0.00,
      grepl("c0.05", variable) ~ 0.05,
      grepl("c0.10", variable) ~ 0.10,
      grepl("c0.20", variable) ~ 0.20
    ),
    transform_type = case_when(
      grepl("linear", variable) ~ "V-LINE: Linear Reporting",
      grepl("power_convex", variable) ~ "Convex",
      grepl("power_concave", variable) ~ "Concave",
      grepl("inv_s_curve", variable) ~ "Inverse-S"
    ),
    cost_label = paste0("C = ", sprintf("%.2f", cost))
  )

UK_NOMINAL_LABEL <- "Nominal LR"

# Helper: swap the "Nominal LS" key for "Nominal LR" in a palette copy
relabel_nominal_lr <- function(palette) {
  names(palette)[names(palette) == "Nominal LS"] <- UK_NOMINAL_LABEL
  palette
}

create_transform_gradient_plot_uk <- function(data, transform_type_filter, colour_palette, y_limits = c(80, 130)) {
  
  colour_palette <- relabel_nominal_lr(colour_palette)
  
  linear_data <- data %>%
    filter(grouping == "election_cycle", transform_type == "V-LINE: Linear Reporting") %>%
    mutate(cost_label = "V-LINE: Linear Reporting (C=0)")
  
  nominal_data <- linear_data %>%
    mutate(cost_label = UK_NOMINAL_LABEL)
  
  transform_data <- data %>%
    filter(grouping == "election_cycle", transform_type == transform_type_filter, cost > 0)
  
  y_var <- "indexed_adjusted_sd"
  nominal_var <- "indexed_sd"
  y_label <- "Indexed Inequality (SD)"
  
  level_order <- names(colour_palette)
  
  p <- ggplot() +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "grey60", linewidth = 0.5)
  
  if (nrow(nominal_data) > 0) {
    p <- p +
      geom_line(data = nominal_data,
                aes(x = period_numeric, y = !!sym(nominal_var), colour = cost_label),
                linewidth = 0.8, linetype = "dashed") +
      geom_point(data = nominal_data,
                 aes(x = period_numeric, y = !!sym(nominal_var), colour = cost_label),
                 size = 2, shape = 1)
  }
  
  if (nrow(linear_data) > 0) {
    p <- p +
      geom_line(data = linear_data,
                aes(x = period_numeric, y = !!sym(y_var), colour = cost_label),
                linewidth = 0.8) +
      geom_point(data = linear_data,
                 aes(x = period_numeric, y = !!sym(y_var), colour = cost_label),
                 size = 2)
  }
  
  if (nrow(transform_data) > 0) {
    p <- p +
      geom_point(data = transform_data,
                 aes(x = period_numeric, y = !!sym(y_var), colour = cost_label),
                 size = 2, alpha = 0.9)
  }
  
  p +
    scale_colour_manual(name = "", values = colour_palette, limits = level_order) +
    scale_x_continuous(breaks = 1:5, labels = uk_election_labels, limits = c(0.5, 5.5)) +
    scale_y_continuous(limits = y_limits) +
    labs(title = "UK (BES Elections)", x = "", y = y_label) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      plot.title = element_text(hjust = 0.5, size = 16),
      legend.position = "bottom",
      legend.text = element_text(size = 10)
    )
}


create_summary_plot_uk <- function(data, y_limits = c(80, 130)) {
  
  uk_summary_colours <- relabel_nominal_lr(summary_colours)
  
  linear_data <- data %>%
    filter(grouping == "election_cycle", transform_type == "V-LINE: Linear Reporting") %>%
    mutate(display_type = "V-LINE: Linear Reporting")
  
  nominal_data <- linear_data %>% mutate(display_type = UK_NOMINAL_LABEL)
  
  c01_data <- data %>%
    filter(grouping == "election_cycle", cost == 0.10, transform_type != "V-LINE: Linear Reporting") %>%
    mutate(display_type = transform_type)
  
  plot_data <- bind_rows(linear_data, c01_data)
  
  y_var <- "indexed_adjusted_sd"
  nominal_var <- "indexed_sd"
  level_order <- names(uk_summary_colours)
  
  p <- ggplot() +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "grey60", linewidth = 0.5)
  
  if (nrow(nominal_data) > 0) {
    p <- p +
      geom_line(data = nominal_data,
                aes(x = period_numeric, y = !!sym(nominal_var), colour = display_type),
                linewidth = 0.8, linetype = "dashed") +
      geom_point(data = nominal_data,
                 aes(x = period_numeric, y = !!sym(nominal_var), colour = display_type),
                 size = 2, shape = 1)
  }
  
  p <- p +
    geom_line(data = filter(plot_data, display_type == "V-LINE: Linear Reporting"),
              aes(x = period_numeric, y = !!sym(y_var), colour = display_type),
              linewidth = 0.8) +
    geom_point(data = filter(plot_data, display_type == "V-LINE: Linear Reporting"),
               aes(x = period_numeric, y = !!sym(y_var), colour = display_type),
               size = 2)
  
  p <- p +
    geom_point(data = filter(plot_data, display_type != "V-LINE: Linear Reporting"),
               aes(x = period_numeric, y = !!sym(y_var), colour = display_type),
               size = 2, alpha = 0.9)
  
  p +
    scale_colour_manual(name = "", values = uk_summary_colours, limits = level_order) +
    scale_x_continuous(breaks = 1:5, labels = uk_election_labels, limits = c(0.5, 5.5)) +
    scale_y_continuous(limits = y_limits) +
    labs(title = "UK (BES Elections)", x = "", y = "Indexed Inequality (SD)") +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      plot.title = element_text(hjust = 0.5, size = 16),
      legend.position = "bottom"
    ) +
    guides(colour = guide_legend(nrow = 1))
}



#
#
#
#
# # ============================================================
# # ============================================================
# # Main Sections:
# # 10) "Paradox Plots"
# # ============================================================
# # ============================================================
#
# LS_PARADOX_Y_BREAKS <- seq(60, 120, 10)
# LS_PARADOX_Y_LIMITS <- c(60, 120)
#
# POL_PARADOX_Y_BREAKS <- seq(50, 150, 25)
# POL_PARADOX_Y_LIMITS <- c(50, 150)
#
# LS_PLOT2_LEVELS <- c("Latent Happiness Inequality (V-LINE)", "Household Income Gini")
# POL_PLOT2_LEVELS <- c("Latent Mass Polarisation (V-LINE)", "Elite Economic Polarisation", "Elite Cultural Polarisation")
#
# # Shared across LS and Political volatility lines wherever they appear --
# # model_5 = with controls (dashed, dark grey), model_1 = raw/no controls
# # (dotted, light grey). Same convention everywhere, incl. swiss_alltime.
# model_display <- c(model_1 = "No Controls", model_5 = "Model 5")
#
# paradox_colors <- c(
#   "Nominal SD(LS)" = "blue",
#   "Nominal SD(LR)" = "blue",
#   "Household Income Gini" = "limegreen",
#   "Elite Cultural Polarisation" = "#2E9E3F",   # bright mid green
#   "Elite Economic Polarisation" = "#6FCF73",   # bright pale green
#   "LS Volatility (Model 5)" = "darkslategrey",
#   "LS Volatility (No Controls)" = "grey45",
#   "LR Volatility (Model 5)" = "darkslategrey",
#   "LR Volatility (No Controls)" = "grey45",
#   "Latent Happiness Inequality (V-LINE)" = "red",
#   "Latent Mass Polarisation (V-LINE)" = "red"
# )
#
# paradox_linetypes <- c(
#   "Nominal SD(LS)" = "solid",
#   "Nominal SD(LR)" = "solid",
#   "Household Income Gini" = "solid",
#   "Elite Cultural Polarisation" = "solid",
#   "Elite Economic Polarisation" = "solid",
#   "LS Volatility (Model 5)" = "dashed",
#   "LS Volatility (No Controls)" = "dotted",
#   "LR Volatility (Model 5)" = "dashed",
#   "LR Volatility (No Controls)" = "dotted",
#   "Latent Happiness Inequality (V-LINE)" = "solid",
#   "Latent Mass Polarisation (V-LINE)" = "solid"
# )
#
# # ============================================================
# # CHES elite data for Switzerland (2010+) -- drives the live
# # Swiss political paradox plots (V-Party retired from these).
# # ============================================================
#
# elite_polarisation_ches_switzerland <- read_csv(
#   file.path(modeloutput_path, "elite_polarisation_ches_switzerland.csv"),
#   show_col_types = FALSE
# )
#
# SWISS_CHES_X_BREAKS <- seq(2010, 2018, 2)
# SWISS_CHES_X_LIMITS <- c(2009, 2019.5)
# SWISS_CHES_MAX_YEAR <- max(elite_polarisation_ches_switzerland$year)
#
# # ============================================================
# # ============================================================
# # LIFE SATISFACTION — PLOT 1: nominal, volatility, Gini
# # model_ids: "model_5" alone, or c("model_5", "model_1") for
# # the raw-data-vs-controls comparison variant.
# # ============================================================
# # ============================================================
#
# build_lsat_paradox_series <- function(country_name, variable_name, model_ids = "model_5") {
#
#   nominal <- vline_lsat %>%
#     filter(country == country_name, variable == variable_name, grouping == "natural") %>%
#     transmute(x = period_numeric, value = indexed_sd, series = "Nominal SD(LS)")
#
#   vol_lines <- lapply(model_ids, function(mid) {
#     label <- paste0("LS Volatility (", model_display[[mid]], ")")
#     vol_lsat %>%
#       filter(country == country_name, variable == variable_name, model_id == mid) %>%
#       mutate(years_since_start = time_period - min(time_period)) %>%
#       filter(years_since_start %% 2 == 0) %>%
#       transmute(x = time_period, value = indexed_within_var_interpolated, series = label)
#   }) %>% bind_rows()
#
#   # Gini truncated to the range the mass series (nominal SD + volatility) covers
#   mass_range <- range(c(nominal$x, vol_lines$x), na.rm = TRUE)
#
#   gini <- income_gini %>%
#     filter(country == country_name, time_period >= mass_range[1], time_period <= mass_range[2]) %>%
#     transmute(x = time_period, value = indexed_gini, series = "Household Income Gini")
#
#   level_order <- c("Nominal SD(LS)", paste0("LS Volatility (", model_display[model_ids], ")"), "Household Income Gini")
#
#   bind_rows(nominal, vol_lines, gini) %>%
#     mutate(series = factor(series, levels = level_order))
# }
#
# create_lsat_paradox_plot1 <- function(country_name, variable_name, model_ids = "model_5",
#                                       show_primary_label = TRUE, show_legend = TRUE) {
#
#   specs <- get_plot_specs(country_name, variable_name)
#   plot_data <- build_lsat_paradox_series(country_name, variable_name, model_ids)
#   level_order <- levels(plot_data$series)
#
#   x_data_range <- range(plot_data$x, na.rm = TRUE)
#   x_limits <- c(min(specs$x_limits[1], x_data_range[1]), max(specs$x_limits[2], x_data_range[2]))
#
#   ggplot(plot_data, aes(x = x, y = value, color = series, linetype = series)) +
#     geom_line(linewidth = 1, alpha = 0.9) +
#     scale_color_manual(name = "", values = paradox_colors, limits = level_order, drop = FALSE) +
#     scale_linetype_manual(name = "", values = paradox_linetypes, limits = level_order, drop = FALSE) +
#     scale_x_continuous(breaks = specs$x_breaks, limits = x_limits) +
#     scale_y_continuous(breaks = LS_PARADOX_Y_BREAKS, limits = LS_PARADOX_Y_LIMITS) +
#     labs(title = specs$title, x = "", y = if (show_primary_label) "Index (First Period = 100)" else "") +
#     theme_minimal(base_size = 14) +
#     theme(
#       panel.border = element_rect(color = "black", fill = NA),
#       panel.grid.minor = element_blank(),
#       plot.title = element_text(size = 16, hjust = 0.5),
#       legend.position = if (show_legend) "bottom" else "none"
#     )
# }
#
# create_publication_lsat_paradox_plot1 <- function(model_ids = "model_5") {
#
#   p_ger <- create_lsat_paradox_plot1("Germany", "lfsato", model_ids, show_primary_label = TRUE)
#   p_aus <- create_lsat_paradox_plot1("Australia", "lfsato", model_ids, show_primary_label = FALSE)
#   p_che <- create_lsat_paradox_plot1("Switzerland", "sat_life", model_ids, show_primary_label = FALSE)
#
#   p_ger + p_aus + p_che +
#     plot_layout(widths = c(3, 2, 2), guides = "collect") &
#     theme(legend.position = "bottom")
# }
#
# # ============================================================
# # ============================================================
# # LIFE SATISFACTION — PLOT 2: latent inequality vs Gini
# # No model variant -- no volatility line here.
# # ============================================================
# # ============================================================
#
# build_lsat_latent_series <- function(country_name, variable_name) {
#
#   latent <- vline_lsat %>%
#     filter(country == country_name, variable == variable_name, grouping == "natural") %>%
#     transmute(x = period_numeric, value = indexed_adjusted_sd, series = "Latent Happiness Inequality (V-LINE)")
#
#   latent_range <- range(latent$x, na.rm = TRUE)
#
#   gini <- income_gini %>%
#     filter(country == country_name, time_period >= latent_range[1], time_period <= latent_range[2]) %>%
#     transmute(x = time_period, value = indexed_gini, series = "Household Income Gini")
#
#   bind_rows(latent, gini) %>%
#     mutate(series = factor(series, levels = LS_PLOT2_LEVELS))
# }
#
# create_lsat_paradox_plot2 <- function(country_name, variable_name, show_primary_label = TRUE, show_legend = TRUE) {
#
#   specs <- get_plot_specs(country_name, variable_name)
#   plot_data <- build_lsat_latent_series(country_name, variable_name)
#
#   x_data_range <- range(plot_data$x, na.rm = TRUE)
#   x_limits <- c(min(specs$x_limits[1], x_data_range[1]), max(specs$x_limits[2], x_data_range[2]))
#
#   ggplot(plot_data, aes(x = x, y = value, color = series, linetype = series)) +
#     geom_line(linewidth = 1, alpha = 0.9) +
#     scale_color_manual(name = "", values = paradox_colors, limits = LS_PLOT2_LEVELS, drop = FALSE) +
#     scale_linetype_manual(name = "", values = paradox_linetypes, limits = LS_PLOT2_LEVELS, drop = FALSE) +
#     scale_x_continuous(breaks = specs$x_breaks, limits = x_limits) +
#     scale_y_continuous(breaks = LS_PARADOX_Y_BREAKS, limits = LS_PARADOX_Y_LIMITS) +
#     labs(title = specs$title, x = "", y = if (show_primary_label) "Index (First Period = 100)" else "") +
#     theme_minimal(base_size = 14) +
#     theme(
#       panel.border = element_rect(color = "black", fill = NA),
#       panel.grid.minor = element_blank(),
#       plot.title = element_text(size = 16, hjust = 0.5),
#       legend.position = if (show_legend) "bottom" else "none"
#     )
# }
#
# create_publication_lsat_paradox_plot2 <- function() {
#
#   p_ger <- create_lsat_paradox_plot2("Germany", "lfsato", show_primary_label = TRUE)
#   p_aus <- create_lsat_paradox_plot2("Australia", "lfsato", show_primary_label = FALSE)
#   p_che <- create_lsat_paradox_plot2("Switzerland", "sat_life", show_primary_label = FALSE)
#
#   p_ger + p_aus + p_che +
#     plot_layout(widths = c(3, 2, 2), guides = "collect") &
#     theme(legend.position = "bottom")
# }
#
# # ============================================================
# # ============================================================
# # POLITICAL PARADOX PLOTS — MAIN (Plot 1 and Plot 2)
# # UK: BES mass, CHES elite. Switzerland: SHP mass (2010+,
# # alternate grouping, reindexed to 2010=100), CHES elite
# # (2010-2019).
# # ============================================================
# # ============================================================
#
# uk_election_lookup <- tribble(
#   ~election, ~year, ~label,
#   1,         2015,  "GE '15",
#   2,         2016,  "Brexit '16",
#   3,         2017,  "GE '17",
#   4,         2019,  "GE '19",
#   5,         2024,  "GE '24"
# )
#
# map_ches_year_to_election <- function(year) {
#   sapply(year, function(y) uk_election_lookup$election[which.min(abs(y - uk_election_lookup$year))])
# }
#
# # ---- UK series builders ----
#
# build_uk_paradox_mass_series <- function(model_ids = "model_5") {
#
#   nominal <- vline_pol %>%
#     filter(country == "UK (BES Elections)", variable == "lr", grouping == "election_cycle") %>%
#     transmute(x = period_numeric, value = indexed_sd, series = "Nominal SD(LR)")
#
#   vol_lines <- lapply(model_ids, function(mid) {
#     label <- paste0("LR Volatility (", model_display[[mid]], ")")
#     vol_pol %>%
#       filter(country == "UK (BES Elections)", variable == "lr", model_id == mid) %>%
#       transmute(x = time_period, value = indexed_within_var_interpolated, series = label)
#   }) %>% bind_rows()
#
#   bind_rows(nominal, vol_lines)
# }
#
# build_uk_paradox_latent_series <- function() {
#   vline_pol %>%
#     filter(country == "UK (BES Elections)", variable == "lr", grouping == "election_cycle") %>%
#     transmute(x = period_numeric, value = indexed_adjusted_sd, series = "Latent Mass Polarisation (V-LINE)")
# }
#
# build_uk_paradox_elite_series <- function() {
#   bind_rows(
#     elite_polarisation_ches %>%
#       transmute(x = map_ches_year_to_election(year), value = indexed_lrecon_spread, series = "Elite Economic Polarisation"),
#     elite_polarisation_ches %>%
#       transmute(x = map_ches_year_to_election(year), value = indexed_galtan_spread, series = "Elite Cultural Polarisation")
#   )
# }
#
# # ---- Switzerland series builders (CHES elite, 2010+, alternate grouping) ----
#
# build_swiss_paradox_mass_series <- function(model_ids = "model_5") {
#
#   base <- vline_pol %>%
#     filter(country == "Switzerland (SHP)", variable == "lrself",
#            grouping == "alternate", period_numeric >= 2010, period_numeric <= SWISS_CHES_MAX_YEAR)
#
#   if (!(2010 %in% base$period_numeric)) {
#     stop("No 'alternate' grouping period_numeric == 2010 for Switzerland (SHP) -- check vline_pol.")
#   }
#
#   base_sd <- base$sd_level[base$period_numeric == 2010]
#
#   nominal <- base %>%
#     transmute(x = period_numeric, value = 100 * sd_level / base_sd, series = "Nominal SD(LR)")
#
#   vol_lines <- lapply(model_ids, function(mid) {
#     label <- paste0("LR Volatility (", model_display[[mid]], ")")
#
#     vol_base <- vol_pol %>%
#       filter(country == "Switzerland (SHP)", variable == "lrself",
#              model_id == mid, time_period >= 2010, time_period <= SWISS_CHES_MAX_YEAR) %>%
#       mutate(years_since_start = time_period - min(time_period)) %>%
#       filter(years_since_start %% 2 == 0)
#
#     if (!(2010 %in% vol_base$time_period)) {
#       stop("No ", mid, " volatility observation at time_period == 2010 for Switzerland (SHP).")
#     }
#
#     base_vol <- vol_base$mean_within_var[vol_base$time_period == 2010]
#
#     vol_base %>%
#       transmute(x = time_period, value = 100 * mean_within_var / base_vol, series = label)
#   }) %>% bind_rows()
#
#   bind_rows(nominal, vol_lines)
# }
#
# build_swiss_paradox_latent_series <- function() {
#
#   base <- vline_pol %>%
#     filter(country == "Switzerland (SHP)", variable == "lrself",
#            grouping == "alternate", period_numeric >= 2010, period_numeric <= SWISS_CHES_MAX_YEAR)
#
#   if (!(2010 %in% base$period_numeric)) {
#     stop("No 'alternate' grouping period_numeric == 2010 for Switzerland (SHP) -- check vline_pol.")
#   }
#
#   base_adj_sd <- base$adjusted_sd[base$period_numeric == 2010]
#
#   base %>%
#     transmute(x = period_numeric, value = 100 * adjusted_sd / base_adj_sd,
#               series = "Latent Mass Polarisation (V-LINE)")
# }
#
# build_swiss_paradox_elite_series <- function() {
#   bind_rows(
#     elite_polarisation_ches_switzerland %>%
#       transmute(x = year, value = indexed_lrecon_spread, series = "Elite Economic Polarisation"),
#     elite_polarisation_ches_switzerland %>%
#       transmute(x = year, value = indexed_galtan_spread, series = "Elite Cultural Polarisation")
#   )
# }
#
# # ---- Panel functions ----
#
# create_uk_political_paradox_panel <- function(mass_data, elite_data, level_order, y_breaks, y_limits, show_primary_label) {
#
#   plot_data <- bind_rows(mass_data, elite_data) %>%
#     mutate(series = factor(series, levels = level_order))
#
#   ggplot(plot_data, aes(x = x, y = value, color = series, linetype = series)) +
#     geom_line(linewidth = 1, alpha = 0.9) +
#     geom_point(data = mutate(elite_data, series = factor(series, levels = level_order)),
#                size = 1.5, alpha = 0.9) +
#     scale_color_manual(name = "", values = paradox_colors, limits = level_order, drop = FALSE) +
#     scale_linetype_manual(name = "", values = paradox_linetypes, limits = level_order, drop = FALSE) +
#     scale_x_continuous(breaks = uk_election_lookup$election, labels = uk_election_lookup$label, limits = c(0.5, 5.5)) +
#     scale_y_continuous(breaks = y_breaks, limits = y_limits) +
#     labs(title = "Britain", x = "", y = if (show_primary_label) "Index (First Period = 100)" else "") +
#     theme_minimal(base_size = 14) +
#     theme(
#       panel.border = element_rect(color = "black", fill = NA),
#       panel.grid.minor = element_blank(),
#       plot.title = element_text(size = 16, hjust = 0.5),
#       legend.position = "bottom"
#     )
# }
#
# create_swiss_political_paradox_panel <- function(mass_data, elite_data, level_order, y_breaks, y_limits, show_primary_label) {
#
#   plot_data <- bind_rows(mass_data, elite_data) %>%
#     mutate(series = factor(series, levels = level_order))
#
#   ggplot(plot_data, aes(x = x, y = value, color = series, linetype = series)) +
#     geom_line(linewidth = 1, alpha = 0.9) +
#     geom_point(data = mutate(elite_data, series = factor(series, levels = level_order)),
#                size = 1.5, alpha = 0.9) +
#     scale_color_manual(name = "", values = paradox_colors, limits = level_order, drop = FALSE) +
#     scale_linetype_manual(name = "", values = paradox_linetypes, limits = level_order, drop = FALSE) +
#     scale_x_continuous(breaks = SWISS_CHES_X_BREAKS, limits = SWISS_CHES_X_LIMITS) +
#     scale_y_continuous(breaks = y_breaks, limits = y_limits) +
#     labs(title = "Switzerland", x = "", y = if (show_primary_label) "Index (First Period = 100)" else "") +
#     theme_minimal(base_size = 14) +
#     theme(
#       panel.border = element_rect(color = "black", fill = NA),
#       panel.grid.minor = element_blank(),
#       plot.title = element_text(size = 16, hjust = 0.5),
#       legend.position = "bottom"
#     )
# }
#
# # ---- Caption ----
#
# political_paradox_caption <- paste0(
#   "Britain: mass polarisation from BES; elite polarisation from CHES\n",
#   "Switzerland: mass polarisation from SHP (2010 onwards); elite polarisation from CHES\n",
#   "Elite polarisation is the vote-share-weighted SD of expert-coded party positions. Britain indexed to first period = 100; Switzerland indexed to 2010 = 100."
# )
#
# # ---- Plot 1: nominal dispersion, volatility, elite polarisation ----
#
# create_publication_political_paradox_plot1 <- function(model_ids = "model_5") {
#
#   vol_labels <- paste0("LR Volatility (", model_display[model_ids], ")")
#   level_order <- c("Nominal SD(LR)", vol_labels, "Elite Economic Polarisation", "Elite Cultural Polarisation")
#
#   p_uk <- create_uk_political_paradox_panel(
#     mass_data = build_uk_paradox_mass_series(model_ids),
#     elite_data = build_uk_paradox_elite_series(),
#     level_order = level_order,
#     y_breaks = POL_PARADOX_Y_BREAKS,
#     y_limits = POL_PARADOX_Y_LIMITS,
#     show_primary_label = TRUE
#   )
#
#   p_che <- create_swiss_political_paradox_panel(
#     mass_data = build_swiss_paradox_mass_series(model_ids),
#     elite_data = build_swiss_paradox_elite_series(),
#     level_order = level_order,
#     y_breaks = POL_PARADOX_Y_BREAKS,
#     y_limits = POL_PARADOX_Y_LIMITS,
#     show_primary_label = FALSE
#   )
#
#   p_uk + p_che +
#     plot_layout(widths = c(1, 1), guides = "collect") &
#     theme(legend.position = "bottom")
# }
#
# # ---- Plot 2: latent (V-LINE-adjusted) polarisation vs elite polarisation ----
# # No model variant -- no volatility line here.
#
# create_publication_political_paradox_plot2 <- function() {
#
#   p_uk <- create_uk_political_paradox_panel(
#     mass_data = build_uk_paradox_latent_series(),
#     elite_data = build_uk_paradox_elite_series(),
#     level_order = POL_PLOT2_LEVELS,
#     y_breaks = POL_PARADOX_Y_BREAKS,
#     y_limits = POL_PARADOX_Y_LIMITS,
#     show_primary_label = TRUE
#   )
#
#   p_che <- create_swiss_political_paradox_panel(
#     mass_data = build_swiss_paradox_latent_series(),
#     elite_data = build_swiss_paradox_elite_series(),
#     level_order = POL_PLOT2_LEVELS,
#     y_breaks = POL_PARADOX_Y_BREAKS,
#     y_limits = POL_PARADOX_Y_LIMITS,
#     show_primary_label = FALSE
#   )
#
#   p_uk + p_che +
#     plot_layout(widths = c(1, 1), guides = "collect") &
#     theme(legend.position = "bottom")
# }
#
# # ============================================================
# # ============================================================
# # SWITZERLAND — ALL-TIME POLITICAL PLOT (2000+, no elite data)
# # Nominal SD, Latent V-LINE SD, and both volatility specs
# # (Model 5, No Controls) on one panel. Natural grouping, full
# # SHP range. Reuses the global paradox_colors/paradox_linetypes
# # maps -- same model_5/model_1 styling as everywhere else.
# # ============================================================
# # ============================================================
#
# SWISS_ALLTIME_LEVELS <- c(
#   "Nominal SD(LR)",
#   "Latent Mass Polarisation (V-LINE)",
#   "LR Volatility (Model 5)",
#   "LR Volatility (No Controls)"
# )
#
# build_swiss_alltime_political_series <- function() {
#
#   nominal <- vline_pol %>%
#     filter(country == "Switzerland (SHP)", variable == "lrself", grouping == "natural") %>%
#     transmute(x = period_numeric, value = indexed_sd, series = "Nominal SD(LR)")
#
#   latent <- vline_pol %>%
#     filter(country == "Switzerland (SHP)", variable == "lrself", grouping == "natural") %>%
#     transmute(x = period_numeric, value = indexed_adjusted_sd, series = "Latent Mass Polarisation (V-LINE)")
#
#   vol_lines <- lapply(c("model_5", "model_1"), function(mid) {
#     label <- paste0("LR Volatility (", model_display[[mid]], ")")
#     vol_pol %>%
#       filter(country == "Switzerland (SHP)", variable == "lrself", model_id == mid) %>%
#       mutate(years_since_start = time_period - min(time_period)) %>%
#       filter(years_since_start %% 2 == 0) %>%
#       transmute(x = time_period, value = indexed_within_var_interpolated, series = label)
#   }) %>% bind_rows()
#
#   bind_rows(nominal, latent, vol_lines) %>%
#     mutate(series = factor(series, levels = SWISS_ALLTIME_LEVELS))
# }
#
# create_swiss_alltime_political_plot <- function() {
#
#   specs <- get_plot_specs("Switzerland (SHP)", "lrself")
#   plot_data <- build_swiss_alltime_political_series()
#
#   ggplot(plot_data, aes(x = x, y = value, color = series, linetype = series)) +
#     geom_line(linewidth = 1, alpha = 0.9) +
#     scale_color_manual(name = "", values = paradox_colors, limits = SWISS_ALLTIME_LEVELS, drop = FALSE) +
#     scale_linetype_manual(name = "", values = paradox_linetypes, limits = SWISS_ALLTIME_LEVELS, drop = FALSE) +
#     scale_x_continuous(breaks = specs$x_breaks, limits = specs$x_limits) +
#     scale_y_continuous(breaks = POL_PARADOX_Y_BREAKS, limits = POL_PARADOX_Y_LIMITS) +
#     labs(title = "Switzerland (SHP, 2000-2020)", x = "", y = "Index (First Period = 100)") +
#     theme_minimal(base_size = 14) +
#     theme(
#       panel.border = element_rect(color = "black", fill = NA),
#       panel.grid.minor = element_blank(),
#       plot.title = element_text(size = 16, hjust = 0.5),
#       legend.position = "bottom"
#     ) +
#     guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))
# }
#
#
# # ============================================================
# # ============================================================
# # 10) Remaining Stuff
# # ============================================================
# # ============================================================
#
# # Placebo Volatility
#
# input_colors <- c(
#   "Health Status" = "#DA70D6"
# )
#
# create_input_volatility_plot <- function(vol_data, country_name,
#                                          x_breaks, x_limits,
#                                          y_breaks = seq(70, 140, 10),
#                                          y_limits = c(70, 140)) {
#
#   plot_data <- vol_data %>%
#     filter(country == country_name, model_id == "model_5",
#            variable %in% c("healthstatus"))
#
#   ggplot(plot_data, aes(x = time_period, y = indexed_within_var_interpolated,
#                         color = variable_label)) +
#     geom_line(linewidth = 1, alpha = 0.9) +
#     geom_point(size = 2) +
#     scale_x_continuous(breaks = x_breaks, limits = x_limits) +
#     scale_y_continuous(breaks = y_breaks, limits = y_limits) +
#     scale_color_manual(values = input_colors, name = "Input:") +
#     labs(title = country_name, x = "", y = "Indexed Volatility") +
#     theme_minimal(base_size = 14) +
#     theme(
#       panel.border = element_rect(color = "black", fill = NA),
#       panel.grid.minor = element_blank(),
#       plot.title = element_text(size = 16, hjust = 0.5),
#       legend.position = "bottom"
#     )
# }



# ============================================================
# ============================================================
# Main Sections:
# 10) "Paradox Plots" (Comparison plots: Version 1)
#
# Updated 9 Nov 2026:
#  - Life satisfaction: the within-panel household income Gini is
#    replaced by WID Top 10 / Bottom 50 ratios (income, and
#    optionally wealth). Annual WID series.
#  - Political: elite polarisation now includes CHES general
#    left-right (lrgen) as well as economic (lrecon) and cultural
#    (galtan). Which series appear is set by `elite_vars`.
#  - y-axes keep the old fixed ranges unless a series falls
#    outside them (e.g. WID wealth); then they widen to fit.
# ============================================================
# ============================================================

LS_PARADOX_Y_BREAKS <- seq(60, 120, 10)
LS_PARADOX_Y_LIMITS <- c(60, 120)

POL_PARADOX_Y_BREAKS <- seq(50, 150, 25)
POL_PARADOX_Y_LIMITS <- c(50, 150)

# Shared across LS and Political volatility lines wherever they appear --
# model_5 = with controls (dashed, dark grey), model_1 = raw/no controls
# (dotted, light grey). Same convention everywhere, incl. swiss_alltime.
model_display <- c(model_1 = "No Controls", model_5 = "Model 5")

# ---- Series labels ----
WID_LABELS <- c(
  income = "Top10/Bottom50 Income (WID)",
  wealth = "Top10/Bottom50 Wealth (WID)"
)

ELITE_LABELS <- c(
  lrgen  = "Elite Left-Right Polarisation",
  lrecon = "Elite Economic Polarisation",
  galtan = "Elite Cultural Polarisation"
)

ELITE_SHORT_LABELS <- c(lrgen = "General Left-Right", lrecon = "Economic", galtan = "Cultural")

# Named sets of elite series, used to generate the versions
ELITE_SETS <- list(
  lrgen     = "lrgen",                         # main specification
  all3      = c("lrgen", "lrecon", "galtan"),  # main + supplementary
  econ_cult = c("lrecon", "galtan")            # the previous version
)

paradox_colors <- c(
  "Nominal SD(LS)" = "blue",
  "Nominal SD(LR)" = "blue",
  "Top10/Bottom50 Income (WID)" = "limegreen",
  "Top10/Bottom50 Wealth (WID)" = "#006D2C",   # dark green
  "Elite Left-Right Polarisation" = "#0B3D0B", # very dark green (main series)
  "Elite Cultural Polarisation" = "#2E9E3F",   # bright mid green
  "Elite Economic Polarisation" = "#6FCF73",   # bright pale green
  "LS Volatility (Model 5)" = "darkslategrey",
  "LS Volatility (No Controls)" = "grey45",
  "LR Volatility (Model 5)" = "darkslategrey",
  "LR Volatility (No Controls)" = "grey45",
  "Latent Happiness Inequality (V-LINE)" = "red",
  "Latent Mass Polarisation (V-LINE)" = "red"
)

paradox_linetypes <- c(
  "Nominal SD(LS)" = "solid",
  "Nominal SD(LR)" = "solid",
  "Top10/Bottom50 Income (WID)" = "solid",
  "Top10/Bottom50 Wealth (WID)" = "solid",   # own panel row now; no need to dash
  "Elite Left-Right Polarisation" = "solid",
  "Elite Cultural Polarisation" = "solid",
  "Elite Economic Polarisation" = "solid",
  "LS Volatility (Model 5)" = "dashed",
  "LS Volatility (No Controls)" = "dotted",
  "LR Volatility (Model 5)" = "dashed",
  "LR Volatility (No Controls)" = "dotted",
  "Latent Happiness Inequality (V-LINE)" = "solid",
  "Latent Mass Polarisation (V-LINE)" = "solid"
)

# Line widths: all 1 (the thicker left-right line was dropped 9 Nov 2026;
# left-right stands out by its darker colour). Change a value here to
# emphasise one series again.
paradox_linewidths <- setNames(rep(1, length(paradox_colors)), names(paradox_colors))

# ============================================================
# HELPER: y-axis that keeps the default range unless data exceed it
# ============================================================

fit_y_scale <- function(values, default_breaks, default_limits, n_breaks = 6) {
  v <- values[is.finite(values)]
  if (length(v) == 0 || (min(v) >= default_limits[1] && max(v) <= default_limits[2])) {
    return(list(breaks = default_breaks, limits = default_limits))
  }
  br <- pretty(c(min(v, default_limits[1]), max(v, default_limits[2])), n = n_breaks)
  list(breaks = br, limits = range(br))
}

# ============================================================
# CHES elite data for Switzerland (2010, 2014, 2019 waves)
# ============================================================

elite_polarisation_ches_switzerland <- read_csv(
  file.path(modeloutput_path, "elite_polarisation_ches_switzerland.csv"),
  show_col_types = FALSE
)

SWISS_CHES_X_BREAKS <- seq(2010, 2018, 2)   # paradox plots: cut at the last CHES wave (2019)
SWISS_CHES_X_LIMITS <- c(2009, 2019.5)
SWISS_ELITE_X_BREAKS <- c(2010, 2014, 2019)  # CHES waves, standalone elite plots
SWISS_ELITE_X_LIMITS <- c(2009, 2019.5)
SWISS_CHES_MAX_YEAR <- max(elite_polarisation_ches_switzerland$year)

# Both CHES files must carry the lrgen columns (added in cleaning 5a)
for (df_name in c("elite_polarisation_ches", "elite_polarisation_ches_switzerland")) {
  missing_cols <- setdiff(c("lrgen_spread", "indexed_lrgen_spread"), names(get(df_name)))
  if (length(missing_cols) > 0) {
    stop(df_name, " has no ", paste(missing_cols, collapse = ", "),
         " -- re-run cleaning Section 5a to regenerate the CHES CSVs.")
  }
}

# ============================================================
# WID series builder (shared by Version 1 and Version 2 plots)
# Returns annual Top10/Bottom50 ratios for one country, trimmed
# to the panel window (and optionally to x_range), re-indexed so
# the first plotted year = 100.
# ============================================================

build_wid_series <- function(country_name, concepts = "income", x_range = NULL) {
  
  d <- wid_ineq %>%
    filter(country == country_name, concept %in% concepts, in_panel_window)
  
  if (!is.null(x_range)) {
    d <- d %>% filter(year >= x_range[1], year <= x_range[2])
  }
  
  if (nrow(d) == 0) {
    warning("No WID data for ", country_name, " (", paste(concepts, collapse = ", "), ")")
  }
  
  d %>%
    group_by(concept) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(indexed = 100 * t10b50_ratio / first(t10b50_ratio)) %>%
    ungroup() %>%
    transmute(x = year, value = indexed, level = t10b50_ratio, concept,
              is_core_estimate, series = unname(WID_LABELS[concept]))
}

# ============================================================
# ============================================================
# LIFE SATISFACTION — PLOT 1: nominal, volatility, WID
# model_ids: "model_5" alone, or c("model_5", "model_1") for
# the raw-data-vs-controls comparison variant.
# wid_concepts: "income", or c("income", "wealth").
# ============================================================
# ============================================================

build_lsat_paradox_series <- function(country_name, variable_name, model_ids = "model_5",
                                      wid_concepts = "income") {
  
  nominal <- vline_lsat %>%
    filter(country == country_name, variable == variable_name, grouping == "natural") %>%
    transmute(x = period_numeric, value = indexed_sd, series = "Nominal SD(LS)")
  
  vol_lines <- lapply(model_ids, function(mid) {
    label <- paste0("LS Volatility (", model_display[[mid]], ")")
    vol_lsat %>%
      filter(country == country_name, variable == variable_name, model_id == mid) %>%
      mutate(years_since_start = time_period - min(time_period)) %>%
      filter(years_since_start %% 2 == 0) %>%
      transmute(x = time_period, value = indexed_within_var_interpolated, series = label)
  }) %>% bind_rows()
  
  # WID truncated to the range the mass series (nominal SD + volatility) covers
  mass_range <- range(c(nominal$x, vol_lines$x), na.rm = TRUE)
  
  wid <- build_wid_series(country_name, wid_concepts, x_range = mass_range) %>%
    select(x, value, series)
  
  level_order <- c("Nominal SD(LS)", paste0("LS Volatility (", model_display[model_ids], ")"),
                   unname(WID_LABELS[wid_concepts]))
  
  bind_rows(nominal, vol_lines, wid) %>%
    mutate(series = factor(series, levels = level_order))
}

# ---- LIFE SATISFACTION — PLOT 2 builder: nominal + latent inequality vs WID ----
# No model variant -- no volatility line here.

build_lsat_latent_series <- function(country_name, variable_name, wid_concepts = "income") {
  
  vline_rows <- vline_lsat %>%
    filter(country == country_name, variable == variable_name, grouping == "natural")
  
  nominal <- vline_rows %>%
    transmute(x = period_numeric, value = indexed_sd, series = "Nominal SD(LS)")
  
  latent <- vline_rows %>%
    transmute(x = period_numeric, value = indexed_adjusted_sd, series = "Latent Happiness Inequality (V-LINE)")
  
  latent_range <- range(latent$x, na.rm = TRUE)
  
  wid <- build_wid_series(country_name, wid_concepts, x_range = latent_range) %>%
    select(x, value, series)
  
  level_order <- c("Nominal SD(LS)", "Latent Happiness Inequality (V-LINE)",
                   unname(WID_LABELS[wid_concepts]))
  
  bind_rows(nominal, latent, wid) %>%
    mutate(series = factor(series, levels = level_order))
}

# ---- Shared LS panel ----

create_lsat_paradox_panel <- function(plot_data, country_name, variable_name, y_scale,
                                      show_primary_label = TRUE, show_legend = TRUE) {
  
  specs <- get_plot_specs(country_name, variable_name)
  level_order <- levels(plot_data$series)
  
  x_data_range <- range(plot_data$x, na.rm = TRUE)
  x_limits <- c(min(specs$x_limits[1], x_data_range[1]), max(specs$x_limits[2], x_data_range[2]))
  
  ggplot(plot_data, aes(x = x, y = value, color = series, linetype = series)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_color_manual(name = "", values = paradox_colors, limits = level_order, drop = FALSE) +
    scale_linetype_manual(name = "", values = paradox_linetypes, limits = level_order, drop = FALSE) +
    scale_x_continuous(breaks = specs$x_breaks, limits = x_limits) +
    scale_y_continuous(breaks = y_scale$breaks, limits = y_scale$limits) +
    labs(title = specs$title, x = "", y = if (show_primary_label) "Index (First Period = 100)" else "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = if (show_legend) "bottom" else "none"
    )
}

LSAT_COUNTRIES <- tribble(
  ~country,      ~variable,
  "Germany",     "lfsato",
  "Australia",   "lfsato",
  "Switzerland", "sat_life"
)

# ---- 3-country wrapper: one common y-axis across the panels ----

combine_lsat_paradox <- function(series_list) {
  
  y_scale <- fit_y_scale(unlist(lapply(series_list, `[[`, "value")),
                         LS_PARADOX_Y_BREAKS, LS_PARADOX_Y_LIMITS)
  
  panels <- lapply(seq_len(nrow(LSAT_COUNTRIES)), function(i) {
    create_lsat_paradox_panel(series_list[[i]],
                              LSAT_COUNTRIES$country[i], LSAT_COUNTRIES$variable[i],
                              y_scale, show_primary_label = (i == 1))
  })
  
  panels[[1]] + panels[[2]] + panels[[3]] +
    plot_layout(widths = c(3, 2, 2), guides = "collect") &
    theme(legend.position = "bottom")
}

create_publication_lsat_paradox_plot1 <- function(model_ids = "model_5", wid_concepts = "income") {
  series_list <- lapply(seq_len(nrow(LSAT_COUNTRIES)), function(i) {
    build_lsat_paradox_series(LSAT_COUNTRIES$country[i], LSAT_COUNTRIES$variable[i],
                              model_ids, wid_concepts)
  })
  combine_lsat_paradox(series_list)
}

create_publication_lsat_paradox_plot2 <- function(wid_concepts = "income") {
  series_list <- lapply(seq_len(nrow(LSAT_COUNTRIES)), function(i) {
    build_lsat_latent_series(LSAT_COUNTRIES$country[i], LSAT_COUNTRIES$variable[i], wid_concepts)
  })
  combine_lsat_paradox(series_list)
}

# ============================================================
# ============================================================
# POLITICAL PARADOX PLOTS — MAIN (Plot 1 and Plot 2)
# UK: BES mass, CHES elite. Switzerland: SHP mass (2010+,
# alternate grouping, reindexed to 2010=100), CHES elite
# (2010, 2014, 2019).
# elite_vars: any of "lrgen", "lrecon", "galtan" (see ELITE_SETS).
# ============================================================
# ============================================================

uk_election_lookup <- tribble(
  ~election, ~year, ~label,
  1,         2015,  "GE '15",
  2,         2016,  "Brexit '16",
  3,         2017,  "GE '17",
  4,         2019,  "GE '19",
  5,         2024,  "GE '24"
)

map_ches_year_to_election <- function(year) {
  sapply(year, function(y) uk_election_lookup$election[which.min(abs(y - uk_election_lookup$year))])
}

# ---- Elite series builder (both countries) ----
# measure = "indexed" (first wave = 100) or "level" (vote-weighted SD, 0-10 scale)

build_elite_series <- function(country_name = c("UK", "Switzerland"),
                               elite_vars = ELITE_SETS$all3,
                               measure = c("indexed", "level")) {
  
  country_name <- match.arg(country_name)
  measure <- match.arg(measure)
  
  src <- if (country_name == "UK") elite_polarisation_ches else elite_polarisation_ches_switzerland
  
  value_cols <- if (measure == "indexed") {
    setNames(paste0("indexed_", elite_vars, "_spread"), elite_vars)
  } else {
    setNames(paste0(elite_vars, "_spread"), elite_vars)
  }
  
  out <- lapply(elite_vars, function(v) {
    src %>%
      transmute(year, value = .data[[value_cols[[v]]]],
                elite_var = v, series = unname(ELITE_LABELS[v]))
  }) %>% bind_rows()
  
  if (country_name == "UK") {
    out %>% mutate(x = map_ches_year_to_election(year))
  } else {
    out %>% mutate(x = year)
  }
}

build_uk_paradox_elite_series <- function(elite_vars = ELITE_SETS$all3) {
  build_elite_series("UK", elite_vars, "indexed") %>% select(x, value, series)
}

build_swiss_paradox_elite_series <- function(elite_vars = ELITE_SETS$all3) {
  build_elite_series("Switzerland", elite_vars, "indexed") %>% select(x, value, series)
}

# ---- UK mass series builders ----

build_uk_paradox_mass_series <- function(model_ids = "model_5") {
  
  nominal <- vline_pol %>%
    filter(country == "UK (BES Elections)", variable == "lr", grouping == "election_cycle") %>%
    transmute(x = period_numeric, value = indexed_sd, series = "Nominal SD(LR)")
  
  vol_lines <- lapply(model_ids, function(mid) {
    label <- paste0("LR Volatility (", model_display[[mid]], ")")
    vol_pol %>%
      filter(country == "UK (BES Elections)", variable == "lr", model_id == mid) %>%
      transmute(x = time_period, value = indexed_within_var_interpolated, series = label)
  }) %>% bind_rows()
  
  bind_rows(nominal, vol_lines)
}

build_uk_paradox_latent_series <- function() {
  
  vline_rows <- vline_pol %>%
    filter(country == "UK (BES Elections)", variable == "lr", grouping == "election_cycle")
  
  bind_rows(
    vline_rows %>%
      transmute(x = period_numeric, value = indexed_sd, series = "Nominal SD(LR)"),
    vline_rows %>%
      transmute(x = period_numeric, value = indexed_adjusted_sd, series = "Latent Mass Polarisation (V-LINE)")
  )
}

# ---- Switzerland mass series builders (2010+, alternate grouping) ----

build_swiss_paradox_mass_series <- function(model_ids = "model_5") {
  
  base <- vline_pol %>%
    filter(country == "Switzerland (SHP)", variable == "lrself",
           grouping == "alternate", period_numeric >= 2010, period_numeric <= SWISS_CHES_MAX_YEAR)
  
  if (!(2010 %in% base$period_numeric)) {
    stop("No 'alternate' grouping period_numeric == 2010 for Switzerland (SHP) -- check vline_pol.")
  }
  
  base_sd <- base$sd_level[base$period_numeric == 2010]
  
  nominal <- base %>%
    transmute(x = period_numeric, value = 100 * sd_level / base_sd, series = "Nominal SD(LR)")
  
  vol_lines <- lapply(model_ids, function(mid) {
    label <- paste0("LR Volatility (", model_display[[mid]], ")")
    
    vol_base <- vol_pol %>%
      filter(country == "Switzerland (SHP)", variable == "lrself",
             model_id == mid, time_period >= 2010, time_period <= SWISS_CHES_MAX_YEAR) %>%
      mutate(years_since_start = time_period - min(time_period)) %>%
      filter(years_since_start %% 2 == 0)
    
    if (!(2010 %in% vol_base$time_period)) {
      stop("No ", mid, " volatility observation at time_period == 2010 for Switzerland (SHP).")
    }
    
    base_vol <- vol_base$mean_within_var[vol_base$time_period == 2010]
    
    vol_base %>%
      transmute(x = time_period, value = 100 * mean_within_var / base_vol, series = label)
  }) %>% bind_rows()
  
  bind_rows(nominal, vol_lines)
}

build_swiss_paradox_latent_series <- function() {
  
  base <- vline_pol %>%
    filter(country == "Switzerland (SHP)", variable == "lrself",
           grouping == "alternate", period_numeric >= 2010, period_numeric <= SWISS_CHES_MAX_YEAR)
  
  if (!(2010 %in% base$period_numeric)) {
    stop("No 'alternate' grouping period_numeric == 2010 for Switzerland (SHP) -- check vline_pol.")
  }
  
  base_sd     <- base$sd_level[base$period_numeric == 2010]
  base_adj_sd <- base$adjusted_sd[base$period_numeric == 2010]
  
  bind_rows(
    base %>%
      transmute(x = period_numeric, value = 100 * sd_level / base_sd,
                series = "Nominal SD(LR)"),
    base %>%
      transmute(x = period_numeric, value = 100 * adjusted_sd / base_adj_sd,
                series = "Latent Mass Polarisation (V-LINE)")
  )
}

# ---- Panel functions ----

create_uk_political_paradox_panel <- function(mass_data, elite_data, level_order, y_breaks, y_limits, show_primary_label) {
  
  plot_data <- bind_rows(mass_data, elite_data) %>%
    mutate(series = factor(series, levels = level_order))
  
  ggplot(plot_data, aes(x = x, y = value, color = series, linetype = series, linewidth = series)) +
    geom_line(alpha = 0.9) +
    geom_point(data = mutate(elite_data, series = factor(series, levels = level_order)),
               size = 1.5, alpha = 0.9, show.legend = FALSE) +
    scale_color_manual(name = "", values = paradox_colors, limits = level_order, drop = FALSE) +
    scale_linetype_manual(name = "", values = paradox_linetypes, limits = level_order, drop = FALSE) +
    scale_linewidth_manual(name = "", values = paradox_linewidths, limits = level_order, drop = FALSE) +
    scale_x_continuous(breaks = uk_election_lookup$election, labels = uk_election_lookup$label, limits = c(0.5, 5.5)) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits) +
    labs(title = "Britain", x = "", y = if (show_primary_label) "Index (First Period = 100)" else "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "bottom"
    )
}

create_swiss_political_paradox_panel <- function(mass_data, elite_data, level_order, y_breaks, y_limits, show_primary_label) {
  
  plot_data <- bind_rows(mass_data, elite_data) %>%
    mutate(series = factor(series, levels = level_order))
  
  ggplot(plot_data, aes(x = x, y = value, color = series, linetype = series, linewidth = series)) +
    geom_line(alpha = 0.9) +
    geom_point(data = mutate(elite_data, series = factor(series, levels = level_order)),
               size = 1.5, alpha = 0.9, show.legend = FALSE) +
    scale_color_manual(name = "", values = paradox_colors, limits = level_order, drop = FALSE) +
    scale_linetype_manual(name = "", values = paradox_linetypes, limits = level_order, drop = FALSE) +
    scale_linewidth_manual(name = "", values = paradox_linewidths, limits = level_order, drop = FALSE) +
    scale_x_continuous(breaks = SWISS_CHES_X_BREAKS, limits = SWISS_CHES_X_LIMITS) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits) +
    labs(title = "Switzerland", x = "", y = if (show_primary_label) "Index (First Period = 100)" else "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "bottom"
    )
}

# ---- Caption ----

political_paradox_caption <- paste0(
  "Britain: mass polarisation from BES; elite polarisation from CHES\n",
  "Switzerland: mass polarisation from SHP (2010-2019); elite polarisation from CHES (2010, 2014, 2019)\n",
  "Elite polarisation is the vote-share-weighted SD of expert-coded party positions. Britain indexed to first period = 100; Switzerland indexed to 2010 = 100."
)

# ---- Shared wrapper: UK + Switzerland with a common y-axis ----

combine_political_paradox <- function(uk_mass, che_mass, elite_vars, level_order) {
  
  uk_elite  <- build_uk_paradox_elite_series(elite_vars)
  che_elite <- build_swiss_paradox_elite_series(elite_vars)
  
  y_scale <- fit_y_scale(c(uk_mass$value, che_mass$value, uk_elite$value, che_elite$value),
                         POL_PARADOX_Y_BREAKS, POL_PARADOX_Y_LIMITS)
  
  p_uk <- create_uk_political_paradox_panel(uk_mass, uk_elite, level_order,
                                            y_scale$breaks, y_scale$limits, show_primary_label = TRUE)
  p_che <- create_swiss_political_paradox_panel(che_mass, che_elite, level_order,
                                                y_scale$breaks, y_scale$limits, show_primary_label = FALSE)
  
  p_uk + p_che +
    plot_layout(widths = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom") &
    guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2),
           linewidth = guide_legend(nrow = 2))
}

# ---- Plot 1: nominal dispersion, volatility, elite polarisation ----

create_publication_political_paradox_plot1 <- function(model_ids = "model_5", elite_vars = ELITE_SETS$lrgen) {
  
  vol_labels <- paste0("LR Volatility (", model_display[model_ids], ")")
  level_order <- c("Nominal SD(LR)", vol_labels, unname(ELITE_LABELS[elite_vars]))
  
  combine_political_paradox(build_uk_paradox_mass_series(model_ids),
                            build_swiss_paradox_mass_series(model_ids),
                            elite_vars, level_order)
}

# ---- Plot 2: nominal + latent (V-LINE-adjusted) polarisation vs elite polarisation ----
# No model variant -- no volatility line here.

create_publication_political_paradox_plot2 <- function(elite_vars = ELITE_SETS$lrgen) {
  
  level_order <- c("Nominal SD(LR)", "Latent Mass Polarisation (V-LINE)", unname(ELITE_LABELS[elite_vars]))
  
  combine_political_paradox(build_uk_paradox_latent_series(),
                            build_swiss_paradox_latent_series(),
                            elite_vars, level_order)
}

# ============================================================
# ============================================================
# SWITZERLAND — ALL-TIME POLITICAL PLOT (2000+, no elite data)
# Nominal SD, Latent V-LINE SD, and both volatility specs
# (Model 5, No Controls) on one panel. Natural grouping, full
# SHP range. Reuses the global paradox_colors/paradox_linetypes
# maps -- same model_5/model_1 styling as everywhere else.
# ============================================================
# ============================================================

SWISS_ALLTIME_LEVELS <- c(
  "Nominal SD(LR)",
  "Latent Mass Polarisation (V-LINE)",
  "LR Volatility (Model 5)",
  "LR Volatility (No Controls)"
)

build_swiss_alltime_political_series <- function() {
  
  nominal <- vline_pol %>%
    filter(country == "Switzerland (SHP)", variable == "lrself", grouping == "natural") %>%
    transmute(x = period_numeric, value = indexed_sd, series = "Nominal SD(LR)")
  
  latent <- vline_pol %>%
    filter(country == "Switzerland (SHP)", variable == "lrself", grouping == "natural") %>%
    transmute(x = period_numeric, value = indexed_adjusted_sd, series = "Latent Mass Polarisation (V-LINE)")
  
  vol_lines <- lapply(c("model_5", "model_1"), function(mid) {
    label <- paste0("LR Volatility (", model_display[[mid]], ")")
    vol_pol %>%
      filter(country == "Switzerland (SHP)", variable == "lrself", model_id == mid) %>%
      mutate(years_since_start = time_period - min(time_period)) %>%
      filter(years_since_start %% 2 == 0) %>%
      transmute(x = time_period, value = indexed_within_var_interpolated, series = label)
  }) %>% bind_rows()
  
  bind_rows(nominal, latent, vol_lines) %>%
    mutate(series = factor(series, levels = SWISS_ALLTIME_LEVELS))
}

create_swiss_alltime_political_plot <- function() {
  
  specs <- get_plot_specs("Switzerland (SHP)", "lrself")
  plot_data <- build_swiss_alltime_political_series()
  
  ggplot(plot_data, aes(x = x, y = value, color = series, linetype = series)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_color_manual(name = "", values = paradox_colors, limits = SWISS_ALLTIME_LEVELS, drop = FALSE) +
    scale_linetype_manual(name = "", values = paradox_linetypes, limits = SWISS_ALLTIME_LEVELS, drop = FALSE) +
    scale_x_continuous(breaks = specs$x_breaks, limits = specs$x_limits) +
    scale_y_continuous(breaks = POL_PARADOX_Y_BREAKS, limits = POL_PARADOX_Y_LIMITS) +
    labs(title = paste0("Switzerland (SHP, ", min(plot_data$x), "-", max(plot_data$x), ")"),
         x = "", y = "Index (First Period = 100)") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "bottom"
    ) +
    guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))
}


# ============================================================
# ============================================================
# 10) Remaining Stuff
# ============================================================
# ============================================================

# Placebo Volatility

input_colors <- c(
  "Health Status" = "#DA70D6"
)

create_input_volatility_plot <- function(vol_data, country_name,
                                         x_breaks, x_limits,
                                         y_breaks = seq(70, 140, 10),
                                         y_limits = c(70, 140)) {
  
  plot_data <- vol_data %>%
    filter(country == country_name, model_id == "model_5",
           variable %in% c("healthstatus"))
  
  ggplot(plot_data, aes(x = time_period, y = indexed_within_var_interpolated,
                        color = variable_label)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits) +
    scale_color_manual(values = input_colors, guide = "none") +
    labs(title = country_name, x = "", y = "Indexed Volatility") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "none"
    )
}






# ============================================================
# ============================================================
# Main Sections (new, 9 Nov 2026):
# 10c) WID inequality on its own (Comparison plots: Version 2)
#      2a: income and wealth in separate panels
#      2b: income and wealth on the same panel
# 10d) Elite polarisation on its own (several versions)
# 10e) Volatility: No Controls vs Model 5 on one panel
# ============================================================
# ============================================================


# ============================================================
# 10c) WID INEQUALITY -- VERSION 2
# Annual Top10/Bottom50 ratios within each panel's window.
# "indexed": first panel year = 100. "level": the raw ratio.
# Points off by default (9 Nov 2026): WID returned the same years
# with and without extrapolations, so the core/interpolated flag
# carried no information. show_points = TRUE still draws them.
# ============================================================

WID_COUNTRY_LAYOUT <- tribble(
  ~country,      ~x_breaks,              ~x_limits,      ~panel_width,
  "Germany",     seq(1985, 2020, 5),     c(1984, 2020),  3,
  "Australia",   seq(2000, 2020, 5),     c(2000, 2020),  2,
  "Switzerland", seq(2000, 2020, 5),     c(1999, 2020),  2
)

get_wid_layout <- function(country_name) {
  row <- WID_COUNTRY_LAYOUT %>% filter(country == country_name)
  list(x_breaks = row$x_breaks[[1]], x_limits = row$x_limits[[1]], panel_width = row$panel_width)
}

wid_colors <- setNames(c("limegreen", "#006D2C"), unname(WID_LABELS[c("income", "wealth")]))
# The core/interpolated distinction was retired on 21 Nov 2026. WID returned
# identical year coverage with and without extrapolations in all six series,
# so is_core_estimate was TRUE everywhere and the second legend entry was an
# empty category that drop = FALSE forced into the key. Points now use one
# shape and carry no legend.

build_wid_plot_data <- function(country_name, concepts, measure = c("indexed", "level")) {
  measure <- match.arg(measure)
  d <- build_wid_series(country_name, concepts)
  d %>%
    mutate(
      y = if (measure == "indexed") value else level,
      series = factor(series, levels = unname(WID_LABELS[concepts]))
    )
}

create_wid_panel <- function(plot_data, country_name, y_scale, y_label = "",
                             title = NULL, show_points = FALSE,
                             sec_base = NULL, sec_label = "") {
  
  layout <- get_wid_layout(country_name)
  
  p <- ggplot(plot_data, aes(x = x, y = y, color = series, linetype = series)) +
    geom_line(linewidth = 1, alpha = 0.9)
  
  if (show_points) {
    p <- p + geom_point(size = 1.6, alpha = 0.9, show.legend = FALSE)
  }
  
  p +
    scale_color_manual(name = "", values = wid_colors, drop = FALSE) +
    scale_linetype_manual(name = "", values = paradox_linetypes[names(wid_colors)], drop = FALSE) +
    scale_x_continuous(breaks = layout$x_breaks, limits = layout$x_limits) +
    wid_y_axis(y_scale, sec_base, sec_label) +
    labs(title = if (is.null(title)) country_name else title, x = "", y = y_label) +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "bottom"
    )
}

wid_y_label <- function(measure, concept = NULL) {
  base <- if (measure == "indexed") "Index (First Year = 100)" else "Top 10 / Bottom 50 Ratio"
  if (is.null(concept)) base else paste0(tools::toTitleCase(concept), ": ", base)
}

# Primary y-axis; when sec_base (the ratio in the first year) is given,
# adds a country-specific secondary axis showing the raw ratio:
# ratio = index * sec_base / 100.
wid_y_axis <- function(y_scale, sec_base = NULL, sec_label = "") {
  if (is.null(sec_base) || !is.finite(sec_base)) {
    return(scale_y_continuous(breaks = y_scale$breaks, limits = y_scale$limits))
  }
  sec_values <- y_scale$breaks * sec_base / 100
  acc <- if (max(sec_values, na.rm = TRUE) < 20) 0.1 else 1
  scale_y_continuous(
    breaks = y_scale$breaks, limits = y_scale$limits,
    sec.axis = sec_axis(trans = ~ . * sec_base / 100, name = sec_label,
                        breaks = sec_values,
                        labels = number_format(accuracy = acc)(sec_values))
  )
}

wid_level_scale <- function(values) {
  br <- pretty(values[is.finite(values)], n = 5)
  list(breaks = br, limits = range(br))
}

# ---- 2a: separate panels. Rows = income / wealth; columns = countries ----
# Income row: one common y-axis. Wealth row: Germany on its own y-axis,
# Australia and Switzerland sharing one (German wealth inequality rises
# far more steeply and would otherwise flatten the other two).
# Indexed version: primary axis = index (first year = 100); each panel
# also has a country-specific secondary axis giving the raw ratio.

WID_SCALE_GROUPS <- list(
  income = list(c("Germany", "Australia", "Switzerland")),
  wealth = list("Germany", c("Australia", "Switzerland"))
)

create_wid_separate_panels <- function(measure = c("indexed", "level"), show_points = FALSE,
                                       secondary_axis = TRUE) {
  
  measure <- match.arg(measure)
  secondary_axis <- secondary_axis && measure == "indexed"  # levels already show the ratio
  countries <- WID_COUNTRY_LAYOUT$country
  
  rows <- lapply(c("income", "wealth"), function(concept) {
    
    data_list <- setNames(lapply(countries, build_wid_plot_data, concepts = concept, measure = measure),
                          countries)
    
    # One y-scale per group of countries that share an axis
    y_scales <- list()
    for (grp in WID_SCALE_GROUPS[[concept]]) {
      grp_y <- unlist(lapply(data_list[grp], `[[`, "y"))
      grp_scale <- if (measure == "indexed") fit_y_scale(grp_y, LS_PARADOX_Y_BREAKS, LS_PARADOX_Y_LIMITS) else wid_level_scale(grp_y)
      for (cn in grp) y_scales[[cn]] <- grp_scale
    }
    
    panels <- lapply(seq_along(countries), function(i) {
      d <- data_list[[i]]
      sec_base <- if (secondary_axis) d$level[which.min(d$x)] else NULL
      create_wid_panel(d, countries[i], y_scales[[countries[i]]],
                       y_label = if (i == 1) wid_y_label(measure, concept) else "",
                       title = if (concept == "income") countries[i] else "",
                       show_points = show_points,
                       sec_base = sec_base,
                       sec_label = if (i == length(countries)) "Top 10 / Bottom 50 Ratio" else "")
    })
    
    panels[[1]] + panels[[2]] + panels[[3]] +
      plot_layout(widths = WID_COUNTRY_LAYOUT$panel_width)
  })
  
  (rows[[1]] / rows[[2]]) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

# ---- 2b: same panel. One panel per country, income and wealth together ----
# Indexed only: the two ratios are an order of magnitude apart in levels.

create_wid_same_panel <- function(show_points = TRUE) {
  
  countries <- WID_COUNTRY_LAYOUT$country
  data_list <- lapply(countries, build_wid_plot_data,
                      concepts = c("income", "wealth"), measure = "indexed")
  y_scale <- fit_y_scale(unlist(lapply(data_list, `[[`, "y")), LS_PARADOX_Y_BREAKS, LS_PARADOX_Y_LIMITS)
  
  panels <- lapply(seq_along(countries), function(i) {
    create_wid_panel(data_list[[i]], countries[i], y_scale,
                     y_label = if (i == 1) wid_y_label("indexed") else "",
                     show_points = show_points)
  })
  
  panels[[1]] + panels[[2]] + panels[[3]] +
    plot_layout(widths = WID_COUNTRY_LAYOUT$panel_width, guides = "collect") &
    theme(legend.position = "bottom")
}

# ---- Single-country versions (slides) ----

# 2a for one country: income | wealth side by side, levels (as in the slides)
create_wid_country_separate <- function(country_name, measure = "level", show_points = FALSE) {
  panels <- lapply(c("income", "wealth"), function(concept) {
    d <- build_wid_plot_data(country_name, concept, measure)
    y_scale <- if (measure == "indexed") fit_y_scale(d$y, LS_PARADOX_Y_BREAKS, LS_PARADOX_Y_LIMITS) else wid_level_scale(d$y)
    create_wid_panel(d, country_name, y_scale, y_label = wid_y_label(measure),
                     title = tools::toTitleCase(concept), show_points = show_points)
  })
  panels[[1]] + panels[[2]] +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

# 2b for one country: income and wealth on one indexed panel
create_wid_country_same_panel <- function(country_name, show_points = TRUE) {
  d <- build_wid_plot_data(country_name, c("income", "wealth"), "indexed")
  y_scale <- fit_y_scale(d$y, LS_PARADOX_Y_BREAKS, LS_PARADOX_Y_LIMITS)
  create_wid_panel(d, country_name, y_scale, y_label = wid_y_label("indexed"),
                   show_points = show_points)
}


# ============================================================
# 10d) ELITE POLARISATION -- STANDALONE VERSIONS
# UK x-axis: CHES year mapped to the nearest BES election.
# Switzerland x-axis: CHES wave year (2010, 2014, 2019).
# ============================================================

elite_colors    <- paradox_colors[unname(ELITE_LABELS)]
elite_linetypes <- paradox_linetypes[unname(ELITE_LABELS)]

elite_y_label <- function(measure) {
  if (measure == "indexed") "Index (First Wave = 100)" else "Vote-Weighted SD of Party Positions (0-10)"
}

create_elite_panel <- function(country_name, elite_vars, measure, y_scale,
                               show_primary_label = TRUE, title = NULL,
                               single_colour = NULL) {
  
  d <- build_elite_series(country_name, elite_vars, measure) %>%
    mutate(series = factor(series, levels = unname(ELITE_LABELS[elite_vars])))
  
  p <- if (is.null(single_colour)) {
    ggplot(d, aes(x = x, y = value, color = series, linetype = series, linewidth = series)) +
      geom_line(alpha = 0.9) +
      geom_point(size = 2, alpha = 0.9, show.legend = FALSE) +
      scale_color_manual(name = "", values = elite_colors, drop = FALSE) +
      scale_linetype_manual(name = "", values = elite_linetypes, drop = FALSE) +
      scale_linewidth_manual(name = "", values = paradox_linewidths[unname(ELITE_LABELS)], drop = FALSE)
  } else {
    ggplot(d, aes(x = x, y = value)) +
      geom_line(colour = single_colour, linewidth = 1, alpha = 0.9) +
      geom_point(colour = single_colour, size = 2, alpha = 0.9)
  }
  
  p <- if (country_name == "UK") {
    p + scale_x_continuous(breaks = uk_election_lookup$election,
                           labels = uk_election_lookup$label, limits = c(0.5, 5.5))
  } else {
    p + scale_x_continuous(breaks = SWISS_ELITE_X_BREAKS, limits = SWISS_ELITE_X_LIMITS)
  }
  
  default_title <- if (country_name == "UK") "Britain" else "Switzerland"
  
  p +
    scale_y_continuous(breaks = y_scale$breaks, limits = y_scale$limits) +
    labs(title = if (is.null(title)) default_title else title, x = "",
         y = if (show_primary_label) elite_y_label(measure) else "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "bottom"
    )
}

elite_scale <- function(values, measure) {
  if (measure == "indexed") {
    fit_y_scale(values, POL_PARADOX_Y_BREAKS, POL_PARADOX_Y_LIMITS)
  } else {
    br <- pretty(c(0, values[is.finite(values)]), n = 5)
    list(breaks = br, limits = range(br))
  }
}

# ---- E1 / E2: UK + Switzerland, chosen series on the same panel ----
# elite_vars = ELITE_SETS$lrgen (E1) or ELITE_SETS$all3 (E2)

create_elite_two_country <- function(elite_vars = ELITE_SETS$lrgen, measure = c("indexed", "level")) {
  
  measure <- match.arg(measure)
  all_vals <- c(build_elite_series("UK", elite_vars, measure)$value,
                build_elite_series("Switzerland", elite_vars, measure)$value)
  y_scale <- elite_scale(all_vals, measure)
  
  p_uk  <- create_elite_panel("UK", elite_vars, measure, y_scale, show_primary_label = TRUE)
  p_che <- create_elite_panel("Switzerland", elite_vars, measure, y_scale, show_primary_label = FALSE)
  
  p_uk + p_che +
    plot_layout(widths = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom")
}

# ---- E3: separate panels per dimension. Rows = country; columns = series ----

create_elite_separate_panels <- function(elite_vars = ELITE_SETS$all3, measure = c("indexed", "level")) {
  
  measure <- match.arg(measure)
  all_vals <- c(build_elite_series("UK", elite_vars, measure)$value,
                build_elite_series("Switzerland", elite_vars, measure)$value)
  y_scale <- elite_scale(all_vals, measure)
  
  rows <- lapply(c("UK", "Switzerland"), function(cn) {
    row_name <- if (cn == "UK") "Britain" else "Switzerland"
    panels <- lapply(seq_along(elite_vars), function(j) {
      v <- elite_vars[j]
      create_elite_panel(cn, v, measure, y_scale,
                         show_primary_label = (j == 1),
                         title = paste0(row_name, ": ", ELITE_SHORT_LABELS[[v]]))
    })
    wrap_plots(panels, nrow = 1)
  })
  
  (rows[[1]] / rows[[2]]) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

# ---- Singles (slides): one country, one series, red, no title ----
# Same style as the old UK-only slide plots.

create_elite_single_plot <- function(country_name, elite_var, measure = "indexed") {
  vals <- build_elite_series(country_name, elite_var, measure)$value
  create_elite_panel(country_name, elite_var, measure, elite_scale(vals, measure),
                     show_primary_label = TRUE, title = "", single_colour = "red") +
    labs(title = NULL)
}


# ============================================================
# 10e) VOLATILITY: NO CONTROLS vs MODEL 5 ON ONE PANEL
# Same filtering as create_volatility_plot (non-overlapping
# windows for year-based series). No secondary axis: it is
# anchored to one base value, so it cannot label both lines.
# Solid lines, no points (9 Nov 2026). Model 5 takes the same
# colour as the single-model volatility plots (darkslateblue for
# LS, steelblue for LR); No Controls is light grey.
# ============================================================

VOL_OVERLAY_MODELS <- c("model_1", "model_5")

vol_overlay_colors <- c(
  "LS Volatility (Model 5)"     = "darkslateblue",
  "LS Volatility (No Controls)" = "grey65",
  "LR Volatility (Model 5)"     = "steelblue",
  "LR Volatility (No Controls)" = "grey65"
)

create_volatility_overlay_panel <- function(vol_data, country_name, variable_name,
                                            model_ids = VOL_OVERLAY_MODELS,
                                            show_primary_label = TRUE,
                                            custom_x_breaks = NULL, custom_x_labels = NULL,
                                            custom_x_limits = NULL) {
  
  specs <- get_plot_specs(country_name, variable_name)
  prefix <- if (specs$domain == "Political") "LR" else "LS"
  labels <- paste0(prefix, " Volatility (", model_display[model_ids], ")")
  
  plot_data <- lapply(model_ids, function(mid) {
    d <- vol_data %>%
      filter(country == country_name, variable == variable_name, model_id == mid)
    if (specs$time_var == "year" && !grepl("COVID", country_name)) {
      d <- d %>%
        mutate(years_since_start = time_period - min(time_period)) %>%
        filter(years_since_start %% 2 == 0)
    }
    d %>% transmute(x = time_period, value = indexed_within_var_interpolated,
                    series = paste0(prefix, " Volatility (", model_display[[mid]], ")"))
  }) %>%
    bind_rows() %>%
    mutate(series = factor(series, levels = labels))
  
  x_breaks <- if (!is.null(custom_x_breaks)) custom_x_breaks else specs$x_breaks
  x_limits <- if (!is.null(custom_x_limits)) custom_x_limits else specs$x_limits
  x_labels <- if (!is.null(custom_x_labels)) custom_x_labels else waiver()
  
  ggplot(plot_data, aes(x = x, y = value, color = series)) +
    geom_hline(yintercept = 100, colour = "grey70", linewidth = 0.4) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_color_manual(name = "", values = vol_overlay_colors, limits = labels) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = x_limits) +
    scale_y_continuous(breaks = specs$vol_y_breaks, limits = specs$vol_y_limits) +
    labs(title = specs$title, x = "", y = if (show_primary_label) "Indexed Volatility" else "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "bottom"
    )
}

create_publication_lsat_volatility_overlay <- function(vol_data, model_ids = VOL_OVERLAY_MODELS) {
  p_ger <- create_volatility_overlay_panel(vol_data, "Germany", "lfsato", model_ids, TRUE)
  p_aus <- create_volatility_overlay_panel(vol_data, "Australia", "lfsato", model_ids, FALSE)
  p_che <- create_volatility_overlay_panel(vol_data, "Switzerland", "sat_life", model_ids, FALSE)
  p_ger + p_aus + p_che +
    plot_layout(widths = c(3, 2, 2), guides = "collect") &
    theme(legend.position = "bottom")
}

create_publication_political_volatility_overlay <- function(vol_data, model_ids = VOL_OVERLAY_MODELS) {
  p_uk <- create_volatility_overlay_panel(vol_data, "UK (BES Elections)", "lr", model_ids, TRUE,
                                          custom_x_breaks = 1:5,
                                          custom_x_labels = unname(uk_election_labels),
                                          custom_x_limits = c(0.5, 5.5))
  p_che <- create_volatility_overlay_panel(vol_data, "Switzerland (SHP)", "lrself", model_ids, FALSE)
  p_uk + p_che +
    plot_layout(widths = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom")
}




# ----------------------------------------------------------------------------
# Part B continued: builders added after Section 10 in the original script
# (lines 4605-4813). Their embedded save calls are inert; the objects and
# functions they define are used in Part C.
# ----------------------------------------------------------------------------









# ============================================================
# NEW PLOTS  (paste after Section 10)
# Needs: vline_lsat, vline_pol, create_vline_plot, create_nominal_stats_plot,
# get_plot_specs, uk_election_labels, the paradox builders/panel function,
# safe_save and the folder paths. plot_specs is not modified.
# ============================================================


# ------------------------------------------------------------
# 1) V-LINE life satisfaction: Australia + Switzerland
#    Nominal vs V-LINE SD, natural grouping, no Gini line.
# ------------------------------------------------------------

create_lsat_vline_sd_aus_che <- function(vline_data) {
  
  p_aus <- create_vline_plot(vline_data, "Australia", "lfsato", "sd",
                             grouping_type = "natural",
                             show_primary_label = TRUE,
                             show_secondary_label = FALSE,
                             show_secondary_axis = TRUE)
  
  p_che <- create_vline_plot(vline_data, "Switzerland", "sat_life", "sd",
                             grouping_type = "natural",
                             show_primary_label = FALSE,
                             show_secondary_label = TRUE,
                             show_secondary_axis = TRUE)
  
  p_aus + p_che +
    plot_layout(widths = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom")
}

lsat_vline_aus_che <- create_lsat_vline_sd_aus_che(vline_lsat)
safe_save(lsat_vline_aus_che, "Rescaling_Natural_AUS_CHE_Between.png", 10, 5, Figures_for_Slides)


# ============================================================
# UK elite single plots: moved to Section 11.10c (9 Nov 2026),
# now generated by create_elite_single_plot() for both countries
# and all three CHES series, keeping the old UK filenames.
# ============================================================



# ------------------------------------------------------------
# 3) + 4) UK political SD plots with custom y-axes
#    Nominal-only: 80-120.  Nominal vs V-LINE: 90-130.  Steps of 10.
#    plot_specs is untouched; the y scale is replaced on these plots only.
# ------------------------------------------------------------

apply_uk_sd_y_axis <- function(p, vline_data, y_breaks, y_limits,
                               y_label     = "Indexed Inequality (SD)",
                               y_label_sec = "Absolute Inequality (SD)") {
  
  specs <- get_plot_specs("UK (BES Elections)", "lr")
  
  base_raw <- vline_data %>%
    filter(country == "UK (BES Elections)", variable == "lr",
           grouping == "election_cycle",
           period_numeric == specs$base_year_natural) %>%
    pull(sd_level) %>%
    first()
  
  scale_factor     <- base_raw / 100
  secondary_values <- y_breaks * scale_factor
  
  # A second y scale replaces the first (ggplot prints a harmless message)
  suppressMessages(
    p + scale_y_continuous(
      name   = y_label,
      breaks = y_breaks,
      limits = y_limits,
      sec.axis = sec_axis(
        trans  = ~ . * scale_factor,
        name   = y_label_sec,
        breaks = secondary_values,
        labels = number_format(accuracy = 0.01)(secondary_values)
      )
    )
  )
}

# 3) Nominal dispersion only: y-axis 80-120
uk_nominal_sd_80_120 <- create_nominal_stats_plot(
  vline_pol, "UK (BES Elections)", "lr", "sd",
  grouping_type = "election_cycle",
  show_primary_label = TRUE, show_secondary_label = TRUE, show_secondary_axis = TRUE,
  custom_x_breaks = 1:5, custom_x_labels = uk_election_labels, custom_x_limits = c(0.5, 5.5)
) %>% apply_uk_sd_y_axis(vline_pol, y_breaks = seq(80, 120, 10), y_limits = c(80, 120))

safe_save(uk_nominal_sd_80_120, "Nominal_UK_SD_Political_y80-120.png", 8, 6, Figures_for_Slides)

# 4) Nominal vs V-LINE: y-axis 90-130
uk_vline_sd_90_130 <- create_vline_plot(
  vline_pol, "UK (BES Elections)", "lr", "sd",
  grouping_type = "election_cycle",
  show_primary_label = TRUE, show_secondary_label = TRUE, show_secondary_axis = TRUE,
  custom_x_breaks = 1:5, custom_x_labels = uk_election_labels, custom_x_limits = c(0.5, 5.5)
) %>% apply_uk_sd_y_axis(vline_pol, y_breaks = seq(90, 130, 10), y_limits = c(90, 130))

safe_save(uk_vline_sd_90_130, "VLINE_Political_UK_Natural_SD_y90-130.png", 8, 6, Figures_for_Slides)




create_bes_election_vs_quasiyearly <- function(vol_data) {
  
  pick_index <- function(d) {
    if ("indexed_within_var_interpolated" %in% names(d)) "indexed_within_var_interpolated"
    else "indexed_within_var"
  }
  y_breaks <- seq(50, 125, 25)
  y_limits <- c(50, 125)
  
  base_theme <- theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "bottom"
    )
  
  # LEFT: main by-election measure (Model 5)
  left_data <- vol_data %>%
    filter(country == "UK (BES Elections)", variable == "lr", model_id == "model_5") %>%
    mutate(series = "By election (LR)")
  
  p_left <- ggplot(left_data, aes(x = time_period, y = .data[[pick_index(left_data)]], color = series)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_color_manual(name = "", values = c("By election (LR)" = "steelblue")) +
    scale_x_continuous(breaks = 1:5, labels = uk_election_labels, limits = c(0.5, 5.5)) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits) +
    labs(title = "By-Election Volatility", x = "", y = "Indexed Volatility") +
    base_theme
  
  # RIGHT: quasi-yearly measure (Model 8)
  right_data <- vol_data %>%
    filter(country == "UK (BES Yearly)", variable == "lr", model_id == "model_8") %>%
    mutate(series = "Quasi-yearly (LR)")
  
  p_right <- ggplot(right_data, aes(x = time_period, y = .data[[pick_index(right_data)]], color = series)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_color_manual(name = "", values = c("Quasi-yearly (LR)" = "darkorange")) +
    scale_x_continuous(breaks = seq(2014, 2024, 2), limits = c(2014, 2024)) +
    scale_y_continuous(name = "", breaks = y_breaks, limits = y_limits) +
    labs(title = "Quasi-Yearly Volatility", x = "") +
    base_theme
  
  p_left + p_right + plot_layout(widths = c(1, 1))
}

bes_election_vs_quasi <- create_bes_election_vs_quasiyearly(vol_pol)
safe_save(bes_election_vs_quasi, "POL_Robustness_UK_ElectionVsQuasiyearly.png", 10, 5, Figures_for_Publication)



# Germany + UK linearity summary, side by side (common y-axis 60-140)
p_lin_ger <- create_summary_plot(all_vline_robustness, "Germany", "sd", y_limits = c(60, 140)) +
  labs(x = "")
p_lin_uk  <- create_summary_plot_uk(all_vline_robustness_uk, y_limits = c(60, 140)) +
  labs(y = "")

plot_summary_sd_ger_uk <- p_lin_ger + p_lin_uk + plot_layout(widths = c(1, 1)) &
  guides(colour = guide_legend(nrow = 2))

safe_save(plot_summary_sd_ger_uk, "nonlinearity_summary_sd_ger_uk.png", 12, 5.5, Figures_for_Publication)



# Volatility (solid lines, no points)
create_publication_lsat_volatility_overlay(vol_lsat)
create_publication_political_volatility_overlay(vol_pol)

# Life satisfaction vs WID income (the one paradox plot, three variants)
create_publication_lsat_paradox_plot1(c("model_5", "model_1"), "income")
create_publication_lsat_paradox_plot1("model_5", "income")
create_publication_lsat_paradox_plot2("income")

# WID on its own: income and wealth in separate rows
create_wid_separate_panels("level")
create_wid_separate_panels("indexed")

# Political: left-right emphasised, legend in two rows
create_publication_political_paradox_plot2(ELITE_SETS$all3)
create_publication_political_paradox_plot2(ELITE_SETS$lrgen)
create_publication_political_paradox_plot1(c("model_5", "model_1"), ELITE_SETS$all3)

# Elite on its own
create_elite_two_country(ELITE_SETS$all3, "indexed")
create_elite_two_country(ELITE_SETS$lrgen, "level")
create_elite_separate_panels(ELITE_SETS$all3, "indexed")



cat("\n--- 11.10e: Mechanism (Nominal SD + Volatility) ---\n")

safe_save(create_publication_lsat_mechanism(vol_lsat, vline_lsat),
          "mechanism_lsat_nominal_vs_volatility.png", 14, 6, Figures_for_Publication)
safe_save(create_publication_political_mechanism(vol_pol, vline_pol),
          "mechanism_political_nominal_vs_volatility.png", 10, 6, Figures_for_Publication)



# ----------------------------------------------------------------------------
# Part B continued: the transformation curves themselves
#
# f(R) on the 0-10 scale, from nonlinear_transformation_curves.csv, which
# 06_robustness_nonlinearity.R writes.
#   By cost:  one panel per C, colours from summary_colours.
#   By shape: one panel per shape, colours from that shape's gradient.
# Linear reporting is the black identity line in both.
# ----------------------------------------------------------------------------

transform_curves <- read_csv(file.path(modeloutput_path, "nonlinear_transformation_curves.csv"),
                             show_col_types = FALSE)

TRANSFORM_LINEAR_LABEL <- "Linear Reporting (C = 0)"

TRANSFORM_PALETTES <- list(
  Convex      = convex_colours,
  Concave     = concave_colours,
  `Inverse-S` = inv_s_colours
)

create_transform_curve_panel <- function(curve_data, colour_var, colour_values, title,
                                         show_primary_label = TRUE) {

  identity_line <- tibble(R = c(0, 10), fR = c(0, 10), series = TRANSFORM_LINEAR_LABEL)

  plot_data <- curve_data %>%
    mutate(series = .data[[colour_var]]) %>%
    select(R, fR, series) %>%
    bind_rows(identity_line) %>%
    mutate(series = factor(series, levels = names(colour_values)))

  ggplot(plot_data, aes(x = R, y = fR, colour = series)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_colour_manual(name = "", values = colour_values, limits = names(colour_values)) +
    scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
    coord_equal() +
    labs(title = title, x = "Reported Value (R)",
         y = if (show_primary_label) "Transformed Value f(R)" else "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "bottom"
    )
}

# Shapes compared at equal C, one shared legend
create_transform_curves_by_cost <- function(curves = transform_curves) {

  colours <- c(setNames("black", TRANSFORM_LINEAR_LABEL),
               summary_colours[c("Convex", "Concave", "Inverse-S")])
  costs <- sort(unique(curves$cost))

  panels <- lapply(seq_along(costs), function(i) {
    create_transform_curve_panel(filter(curves, cost == costs[i]), "transform_type", colours,
                                 title = sprintf("C = %.2f", costs[i]),
                                 show_primary_label = (i == 1))
  })

  wrap_plots(panels, nrow = 1) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

# Each shape's cost gradient, one legend per panel
create_transform_curves_by_shape <- function(curves = transform_curves) {

  shapes <- names(TRANSFORM_PALETTES)

  panels <- lapply(seq_along(shapes), function(i) {
    pal <- TRANSFORM_PALETTES[[shapes[i]]]
    colours <- c(setNames("black", TRANSFORM_LINEAR_LABEL),
                 pal[c("C = 0.05", "C = 0.10", "C = 0.20")])
    create_transform_curve_panel(filter(curves, transform_type == shapes[i]), "cost_label", colours,
                                 title = shapes[i], show_primary_label = (i == 1)) +
      guides(colour = guide_legend(nrow = 2))
  })

  wrap_plots(panels, nrow = 1) &
    theme(legend.position = "bottom")
}


.vline_defer_saves <- FALSE  # from here on, safe_save() writes to disk

# ============================================================================
# PART C — writes to disk
#
# The five numbered figures of the main text come first, in the order they
# appear in it. Appendix figures follow, grouped by appendix; slide figures
# after that. Neither group is numbered: appendix numbering shifts whenever a
# float is added, and the slide decks are revised independently.
# ============================================================================

cat("\n========================================\n")
cat("MAIN TEXT FIGURES\n")
cat("========================================\n")

# ===== Figure 1 — Proof of Concept: income validation =======================
cat("\nFigure 1  income validation, two panels\n")

# Panel (a): nominal and actual dispersion diverge
ger_inc_cat_ineq_valid_minus_VLINE <- plot_lincat_inequality_validation(lincat_vline, "Germany", show_vline = FALSE)
aus_inc_cat_ineq_valid_minus_VLINE <- plot_lincat_inequality_validation(lincat_vline, "Australia", show_vline = FALSE)
che_inc_cat_ineq_valid_minus_VLINE <- plot_lincat_inequality_validation(lincat_vline, "Switzerland", show_vline = FALSE)
inc_cat_ineq_valid_minus_VLINE <- ger_inc_cat_ineq_valid_minus_VLINE + aus_inc_cat_ineq_valid_minus_VLINE + che_inc_cat_ineq_valid_minus_VLINE +
  plot_layout(widths = c(3, 2, 2), guides = "collect") &
  theme(legend.position = "bottom")
safe_save(inc_cat_ineq_valid_minus_VLINE, "inc_cat_ineq_valid_minus_VLINE.png", 14, 6, Figures_for_Publication)

# Panel (b): response volatility and the stretching parameter
ger_inc_cat_stretching <- plot_lincat_volatilities(lincat_vline, "Germany", add_stretching = TRUE)
aus_inc_cat_stretching <- plot_lincat_volatilities(lincat_vline, "Australia", add_stretching = TRUE)
che_inc_cat_stretching <- plot_lincat_volatilities(lincat_vline, "Switzerland", add_stretching = TRUE)
inc_cat_stretching <- ger_inc_cat_stretching + aus_inc_cat_stretching + che_inc_cat_stretching +
  plot_layout(widths = c(3, 2, 2), guides = "collect") &
  theme(legend.position = "bottom")
safe_save(inc_cat_stretching, "inc_cat_stretching.png", 14, 6, Figures_for_Publication)

# ===== Figure 2 — actual, nominal and V-LINE-adjusted income inequality =====
cat("\nFigure 2  inc_cat_ineq_valid.png\n")
ger_inc_cat_ineq_valid <- plot_lincat_inequality_validation(lincat_vline, "Germany", show_vline = TRUE)
aus_inc_cat_ineq_valid <- plot_lincat_inequality_validation(lincat_vline, "Australia", show_vline = TRUE)
che_inc_cat_ineq_valid <- plot_lincat_inequality_validation(lincat_vline, "Switzerland", show_vline = TRUE)
inc_cat_ineq_valid <- ger_inc_cat_ineq_valid + aus_inc_cat_ineq_valid + che_inc_cat_ineq_valid +
  plot_layout(widths = c(3, 2, 2), guides = "collect") &
  theme(legend.position = "bottom")
safe_save(inc_cat_ineq_valid, "inc_cat_ineq_valid.png", 14, 6, Figures_for_Publication)

# ===== Figure 3 — Results: life satisfaction ================================
cat("\nFigure 3  life satisfaction, two panels\n")

# Panel (a): nominal dispersion and response volatility
safe_save(create_publication_lsat_mechanism(vol_lsat, vline_lsat),
          "mechanism_lsat_nominal_vs_volatility.png", 14, 6, Figures_for_Publication)

# Panel (b): nominal and latent dispersion (V-LINE)
pub_lsat_vline_sd <- create_publication_lsat_vline_sd(vline_lsat)
safe_save(pub_lsat_vline_sd, "rescaling_between_natural.png", 14, 6, Figures_for_Publication)

# ===== Figure 4 — Results: left-right self-placement ========================
cat("\nFigure 4  political, two panels\n")

# Panel (a): nominal dispersion and response volatility
safe_save(create_publication_political_mechanism(vol_pol, vline_pol),
          "mechanism_political_nominal_vs_volatility.png", 10, 6, Figures_for_Publication)

# Panel (b): nominal and latent dispersion (V-LINE)
pub_pol_vline_sd <- create_publication_political_vline(vline_pol)
safe_save(pub_pol_vline_sd, "VLINE_political_combined_SD.png", 10, 6, Figures_for_Publication)

# ===== Figure 5 — Results: placebo, self-rated health =======================
cat("\nFigure 5  placebo_health_status_volatility.png\n")
plot_inputs_germany <- create_input_volatility_plot(vol_inputs, "Germany",
                                                    x_breaks = seq(1995, 2020, 5), x_limits = c(1994, 2020))
plot_inputs_australia <- create_input_volatility_plot(vol_inputs, "Australia",
                                                      x_breaks = seq(2000, 2020, 5), x_limits = c(2000, 2020))
plot_inputs_combined <- plot_inputs_germany + plot_inputs_australia +
  plot_layout(widths = c(3, 2), guides = "collect") &
  theme(legend.position = "bottom")
safe_save(plot_inputs_combined, "placebo_health_status_volatility.png", 12, 6, Figures_for_Publication)

cat("\n--- main text complete: 5 figures ---\n")


# ============================================================================
# APPENDIX FIGURES — grouped by appendix, not numbered
# ============================================================================

cat("\n========================================\n")
cat("APPENDIX FIGURES\n")
cat("========================================\n")

# ---- Appendix A: Additional Figures ----------------------------------------
cat("\nAppendix A\n")

# Within-person volatility, new respondents only (Model 6)
safe_save(create_publication_lsat_volatility(vol_lsat, "model_6"),
          "LS_overall_new_respondents.png", 12, 6, Figures_for_Publication)
safe_save(create_publication_political_volatility(vol_pol, "model_6"),
          "POL_overall_new_respondents.png", 10, 6, Figures_for_Publication)

# Within-person volatility, balanced panels (Model 7)
safe_save(create_publication_lsat_volatility(vol_lsat, "model_7"),
          "LS_overall_balanced_panel.png", 12, 6, Figures_for_Publication)
safe_save(create_publication_political_volatility(vol_pol, "model_7"),
          "POL_overall_balanced_panel.png", 10, 6, Figures_for_Publication)

# Sensitivity to the definition of the two-year periods: natural (t) against
# alternate (t+1) grouping, three countries. Both groupings come from
# vline_life_satisfaction.csv.
lsat_specs <- c(Germany = "lfsato", Australia = "lfsato", Switzerland = "sat_life")

make_grouping_panels <- function(metric) {
  map(names(lsat_specs), function(cn) {
    create_grouping_comparison_plot(
      vline_lsat, cn, lsat_specs[[cn]], metric,
      show_primary_label   = cn == "Germany",
      show_secondary_label = cn == "Switzerland",
      show_secondary_axis  = TRUE
    )
  })
}

pub_grouping_sd <- wrap_plots(make_grouping_panels("sd"), nrow = 1, guides = "collect") &
  theme(legend.position = "bottom")
safe_save(pub_grouping_sd, "comparison_between_groupings.png", 14, 6, Figures_for_Publication)

# Raw response distributions, first vs last period
all_distributions <- all_distributions %>%
  mutate(period = factor(period, levels = c("First", "Last")))
pub_lsat_distribution <- create_publication_lsat_distribution(all_distributions)
safe_save(pub_lsat_distribution, "Distribution_FirstLast_LifeSatisfaction.png", 14, 6, Figures_for_Publication)
pub_political_distribution <- create_publication_political_distribution(all_distributions)
safe_save(pub_political_distribution, "Distribution_FirstLast_Political.png", 10, 6, Figures_for_Publication)

# ---- Appendix B: External Benchmarks ---------------------------------------
cat("\nAppendix B\n")
safe_save(create_wid_separate_panels("level"),
          "wid_2a_separate_panels_levels.png", 14, 9, Figures_for_Publication)
safe_save(create_elite_separate_panels(ELITE_SETS$all3, "indexed"),
          "elite_E3_separate_panels_indexed.png", 14, 9, Figures_for_Publication)

# ---- Appendix C: BES with an alternative period definition -----------------
cat("\nAppendix C\n")
slide_uk_robustness <- create_bes_robustness_comparison(vol_pol)
safe_save(slide_uk_robustness, "POL_Robustness_UK_PreVsSwing.png", 10, 5, Figures_for_Publication)

# ---- Appendix D: Levels Analysis -------------------------------------------
cat("\nAppendix D\n")
ger_inc_cat_mean_valid <- plot_lincat_mean_validation(lincat_vline, "Germany", show_vline = TRUE)
aus_inc_cat_mean_valid <- plot_lincat_mean_validation(lincat_vline, "Australia", show_vline = TRUE)
che_inc_cat_mean_valid <- plot_lincat_mean_validation(lincat_vline, "Switzerland", show_vline = TRUE)
inc_cat_mean_valid <- ger_inc_cat_mean_valid + aus_inc_cat_mean_valid + che_inc_cat_mean_valid +
  plot_layout(widths = c(3, 2, 2), guides = "collect") &
  theme(legend.position = "bottom")
safe_save(inc_cat_mean_valid, "inc_cat_mean_valid.png", 14, 6, Figures_for_Publication)

pub_lsat_vline_mean <- create_publication_lsat_vline_mean(vline_lsat)
safe_save(pub_lsat_vline_mean, "rescaling_mean_natural.png", 14, 6, Figures_for_Publication)

covid_vline_mean <- create_covid_vline_plot(vline_lsat, "mean")
safe_save(covid_vline_mean, "COVID_VLINE_Mean_Combined.png", 10, 5, Figures_for_Publication)

covid_vline_sd <- create_covid_vline_plot(vline_lsat, "sd")
safe_save(covid_vline_sd, "COVID_VLINE_SD_Combined.png", 10, 5, Figures_for_Publication)

# ---- Appendix E: Robustness to linearity violations ------------------------
cat("\nAppendix E\n")

# The simulated transformations themselves, one panel per shape, each showing
# that shape's cost gradient against the linear identity.
safe_save(create_transform_curves_by_shape(),
          "nonlinearity_transformations_by_shape.png", 13, 6, Figures_for_Publication)

plot_convex_sd <- create_combined_transform_plot(all_vline_robustness, "Convex", "sd", convex_colours,
                                                 y_limits = c(60, 140), main_title = "V-LINE Inequality (SD): Convex Transformation")
plot_concave_sd <- create_combined_transform_plot(all_vline_robustness, "Concave", "sd", concave_colours,
                                                  y_limits = c(60, 140), main_title = "V-LINE Inequality (SD): Concave Transformation")
plot_inv_s_sd <- create_combined_transform_plot(all_vline_robustness, "Inverse-S", "sd", inv_s_colours,
                                                y_limits = c(60, 140), main_title = "V-LINE Inequality (SD): Inverse-S Transformation")
plot_summary_sd <- create_combined_summary_plot(all_vline_robustness, "sd", y_limits = c(60, 140),
                                                main_title = "V-LINE Inequality (SD): All Transformations at C = 0.10")
safe_save(plot_summary_sd, "nonlinearity_summary_sd.png", 10, 6, Figures_for_Publication)
safe_save(plot_convex_sd, "nonlinearity_convex_sd.png", 10, 6, Figures_for_Publication)
safe_save(plot_concave_sd, "nonlinearity_concave_sd.png", 10, 6, Figures_for_Publication)
safe_save(plot_inv_s_sd, "nonlinearity_inv_s_sd.png", 10, 6, Figures_for_Publication)

plot_convex_sd_uk <- create_transform_gradient_plot_uk(all_vline_robustness_uk, "Convex", convex_colours, y_limits = c(80, 140))
plot_concave_sd_uk <- create_transform_gradient_plot_uk(all_vline_robustness_uk, "Concave", concave_colours, y_limits = c(80, 140))
plot_inv_s_sd_uk <- create_transform_gradient_plot_uk(all_vline_robustness_uk, "Inverse-S", inv_s_colours, y_limits = c(80, 140))
plot_summary_sd_uk <- create_summary_plot_uk(all_vline_robustness_uk, y_limits = c(80, 140))
safe_save(plot_summary_sd_uk, "nonlinearity_summary_sd_uk.png", 7, 6, Figures_for_Publication)
safe_save(plot_convex_sd_uk, "nonlinearity_convex_sd_uk.png", 7, 6, Figures_for_Publication)
safe_save(plot_concave_sd_uk, "nonlinearity_concave_sd_uk.png", 7, 6, Figures_for_Publication)
safe_save(plot_inv_s_sd_uk, "nonlinearity_inv_s_sd_uk.png", 7, 6, Figures_for_Publication)

# ---- Appendix F: Robustness to smooth rescaling trends: no figures ---------

# ---- Appendix G: Robustness to violations of homoskedasticity --------------
cat("\nAppendix G\n")
ger_income_homoskedasticity <- create_income_homoskedasticity_plot("Germany", show_primary_label = TRUE)
aus_income_homoskedasticity <- create_income_homoskedasticity_plot("Australia", show_primary_label = FALSE)
che_income_homoskedasticity <- create_income_homoskedasticity_plot("Switzerland", show_primary_label = FALSE)
income_homoskedasticity <- ger_income_homoskedasticity + che_income_homoskedasticity + aus_income_homoskedasticity +
  plot_layout(widths = c(3, 2, 2), guides = "collect") &
  theme(legend.position = "bottom")
safe_save(income_homoskedasticity, "income_homoskedasticity_appendix.png", 14, 8, Figures_for_Publication)


# ============================================================================
# SLIDE FIGURES — not numbered
#
# Used by the presentation decks rather than the paper. Loops are kept whole,
# so a few members of each family are produced that no current deck uses;
# NOT_USED.md lists them.
# ============================================================================

cat("\n========================================\n")
cat("SLIDE FIGURES\n")
cat("========================================\n")

# Life satisfaction volatility by model, Germany
for (m in c("model_1", "model_2", "model_5", "model_6", "model_7")) {
  p <- create_slide_lsat_volatility(vol_lsat, "Germany", "lfsato", m)
  fname <- switch(m,
                  model_1 = "TS_Model_1_NoControls_ger_life.png",
                  model_2 = "TS_Model_2_Demographic_ger_life.png",
                  model_5 = "TS_Model_5_FactorPanelDemographic_ger_life.png",
                  model_6 = "TS_Model_6_NewRespondents_ger_life.png",
                  model_7 = "TS_Model_7_BalancedPanel_ger_life.png"
  )
  safe_save(p, fname, 8, 5, Figures_for_Slides)
}

# Political volatility by model, UK
for (m in c("model_1", "model_2", "model_5", "model_6", "model_7")) {
  p <- create_slide_political_volatility(vol_pol, "UK (BES Elections)", "lr", m,
                                         custom_x_breaks = 1:5,
                                         custom_x_labels = uk_election_labels,
                                         custom_x_limits = c(0.5, 5.5))
  fname <- switch(m,
                  model_1 = "POL_Model_1_NoControls_uk.png",
                  model_2 = "POL_Model_2_Demographic_uk.png",
                  model_5 = "POL_Model_5_FactorPanelDemographic_uk.png",
                  model_6 = "POL_Model_6_NewRespondents_uk.png",
                  model_7 = "POL_Model_7_BalancedPanel_uk.png"
  )
  safe_save(p, fname, 8, 5, Figures_for_Slides)
}

slide_uk_pol_m8 <- create_slide_political_volatility(vol_pol, "UK (BES Elections)", "lr", "model_8",
                                                     custom_x_breaks = 1:5,
                                                     custom_x_labels = uk_election_labels,
                                                     custom_x_limits = c(0.5, 5.5))
safe_save(slide_uk_pol_m8, "POL_Model_8_surveygaps_uk.png", 8, 5, Figures_for_Slides)
safe_save(slide_uk_pol_m8, "POL_Model_8_surveygaps_uk.png", 8, 5, Figures_for_Publication)

# V-LINE and nominal dispersion, single-country versions
slide_ger_vline_sd <- create_vline_plot(vline_lsat, "Germany", "lfsato", "sd",
                                        grouping_type = "natural",
                                        show_primary_label = TRUE, show_secondary_label = TRUE,
                                        show_secondary_axis = TRUE)
safe_save(slide_ger_vline_sd, "Rescaling_Natural_GER_Between.png", 8, 6, Figures_for_Slides)

slide_ger_vline_mean <- create_vline_plot(vline_lsat, "Germany", "lfsato", "mean",
                                          grouping_type = "natural",
                                          show_primary_label = TRUE, show_secondary_label = TRUE,
                                          show_secondary_axis = TRUE)
safe_save(slide_ger_vline_mean, "Rescaling_Natural_GER_Mean.png", 8, 6, Figures_for_Slides)

slide_aus_vline_sd <- create_vline_plot(vline_lsat, "Australia", "lfsato", "sd",
                                        grouping_type = "natural",
                                        show_primary_label = TRUE, show_secondary_label = TRUE,
                                        show_secondary_axis = TRUE)
safe_save(slide_aus_vline_sd, "Rescaling_Natural_AUS_Between.png", 8, 6, Figures_for_Slides)
safe_save(lsat_vline_aus_che, "Rescaling_Natural_AUS_CHE_Between.png", 10, 5, Figures_for_Slides)

slide_ger_nominal_sd <- create_nominal_stats_plot(vline_lsat, "Germany", "lfsato", "sd",
                                                  grouping_type = "natural",
                                                  show_primary_label = TRUE, show_secondary_label = TRUE,
                                                  show_secondary_axis = TRUE)
safe_save(slide_ger_nominal_sd, "Nominal_GER_Between_LS.png", 8, 6, Figures_for_Slides)

slide_uk_vline_sd <- create_vline_plot(vline_pol, "UK (BES Elections)", "lr", "sd",
                                       grouping_type = "election_cycle",
                                       show_primary_label = TRUE, show_secondary_label = TRUE,
                                       show_secondary_axis = TRUE,
                                       custom_x_breaks = 1:5, custom_x_labels = uk_election_labels,
                                       custom_x_limits = c(0.5, 5.5))
safe_save(slide_uk_vline_sd, "VLINE_Political_UK_Natural_SD.png", 8, 6, Figures_for_Slides)

slide_che_vline_sd <- create_vline_plot(vline_pol, "Switzerland (SHP)", "lrself", "sd",
                                        grouping_type = "natural",
                                        show_primary_label = TRUE, show_secondary_label = TRUE,
                                        show_secondary_axis = TRUE)
safe_save(slide_che_vline_sd, "VLINE_Political_CHE_Natural_SD.png", 8, 6, Figures_for_Slides)

slide_uk_nominal_sd <- create_nominal_stats_plot(vline_pol, "UK (BES Elections)", "lr", "sd",
                                                 grouping_type = "election_cycle",
                                                 show_primary_label = TRUE, show_secondary_label = TRUE,
                                                 show_secondary_axis = TRUE,
                                                 custom_x_breaks = 1:5, custom_x_labels = uk_election_labels,
                                                 custom_x_limits = c(0.5, 5.5))
safe_save(slide_uk_nominal_sd, "Nominal_UK_SD_Political.png", 8, 6, Figures_for_Slides)

# Alternative y-axis crops, built in Part B
safe_save(uk_nominal_sd_80_120, "Nominal_UK_SD_Political_y80-120.png", 8, 6, Figures_for_Slides)
safe_save(uk_vline_sd_90_130, "VLINE_Political_UK_Natural_SD_y90-130.png", 8, 6, Figures_for_Slides)

# Slide copies of figures that also appear in the paper
safe_save(covid_vline_sd, "COVID_VLINE_SD_Combined.png", 10, 5, Figures_for_Slides)
safe_save(covid_vline_mean, "COVID_VLINE_Mean_Combined.png", 10, 5, Figures_for_Slides)
safe_save(slide_uk_robustness, "POL_Robustness_UK_PreVsSwing.png", 10, 5, Figures_for_Slides)

# Income category, Germany-only versions
ger_inc_cat_volatility <- plot_lincat_volatilities(lincat_vline, "Germany", add_stretching = FALSE)
safe_save(ger_inc_cat_volatility, "Income_Cat_GER_Volatility_only.png", 9, 6, Figures_for_Slides)
safe_save(ger_inc_cat_stretching, "Income_Cat_GER_Stretching.png", 9, 6, Figures_for_Slides)
safe_save(ger_inc_cat_mean_valid, "Income_Cat_GER_Mean_Valid.png", 9, 6, Figures_for_Slides)
safe_save(ger_inc_cat_ineq_valid, "Income_Cat_GER_Ineq_Valid.png", 9, 6, Figures_for_Slides)
safe_save(ger_inc_cat_ineq_valid_minus_VLINE, "Income_Cat_GER_Ineq_Valid_minus_VLINE.png", 9, 6, Figures_for_Slides)

# Non-linearity, slide versions
plot_summary_sd_germany <- create_summary_plot(all_vline_robustness, "Germany", "sd", y_limits = c(60, 140))
safe_save(plot_summary_sd_germany, "Nonlinearity_Summary_SD_Germany.png", 9, 6, Figures_for_Slides)
safe_save(plot_summary_sd_ger_uk, "nonlinearity_summary_sd_ger_uk.png", 12, 5.5, Figures_for_Publication)

# BES election-cycle vs quasi-yearly volatility
safe_save(bes_election_vs_quasi, "POL_Robustness_UK_ElectionVsQuasiyearly.png", 10, 5, Figures_for_Publication)

# WID, per-country versions
for (cn in WID_COUNTRY_LAYOUT$country) {
  code <- switch(cn, Germany = "GER", Australia = "AUS", Switzerland = "CHE")
  safe_save(create_wid_country_separate(cn, "level"),
            paste0("WID_", code, "_2a_income_wealth_levels.png"), 12, 5.5, Figures_for_Slides)
  safe_save(create_wid_country_separate(cn, "indexed"),
            paste0("WID_", code, "_2a_income_wealth_indexed.png"), 12, 5.5, Figures_for_Slides)
}

# Elite polarisation, single-variable panels
for (cn in c("UK", "Switzerland")) {
  for (v in ELITE_SETS$all3) {
    fname <- paste0("paradox_political_", if (cn == "UK") "uk" else "che", "_elite_",
                    switch(v, lrgen = "lrgen", lrecon = "economic", galtan = "cultural"), ".png")
    safe_save(create_elite_single_plot(cn, v), fname, 6, 5, Figures_for_Publication)
  }
}


# ============================================================================
# FIGURES THE CODE CAN PRODUCE THAT NOTHING CURRENTLY USES
#
# Set GENERATE_UNUSED <- TRUE in R/00_config.R to produce these. NOT_USED.md
# describes each family.
# ============================================================================

if (isTRUE(GENERATE_UNUSED)) {

  cat("\n========================================\n")
  cat("UNUSED FIGURES (GENERATE_UNUSED is TRUE)\n")
  cat("========================================\n")

  # Volatility, model specifications not shown in the paper
  lsat_vol_pub_filenames <- c(
    model_1 = "LS_overall_no_controls.png",
    model_2 = "LS_overall_demographic_controls.png",
    model_3 = "LS_overall_panel_tenure_linear.png",
    model_4 = "LS_overall_panel_tenure_factor.png",
    model_5 = "LS_overall_panel_tenure_factor_demographic.png"
  )
  for (m in names(lsat_vol_pub_filenames)) {
    safe_save(create_publication_lsat_volatility(vol_lsat, m),
              lsat_vol_pub_filenames[[m]], 12, 6, Figures_for_Publication)
  }

  pol_vol_pub_filenames <- c(
    model_1 = "POL_overall_no_controls.png",
    model_2 = "POL_overall_demographic_controls.png",
    model_3 = "POL_overall_panel_tenure_linear.png",
    model_4 = "POL_overall_panel_tenure_factor.png",
    model_5 = "POL_overall_panel_tenure_factor_demographic.png"
  )
  for (m in names(pol_vol_pub_filenames)) {
    safe_save(create_publication_political_volatility(vol_pol, m),
              pol_vol_pub_filenames[[m]], 10, 6, Figures_for_Publication)
  }

  # Volatility overlay, no controls against Model 5
  safe_save(create_publication_lsat_volatility_overlay(vol_lsat),
            "LS_volatility_nc_vs_model5.png", 12, 6, Figures_for_Publication)
  safe_save(create_publication_political_volatility_overlay(vol_pol),
            "POL_volatility_nc_vs_model5.png", 10, 6, Figures_for_Publication)

  # Nominal dispersion on its own
  safe_save(create_publication_lsat_nominal_sd_combined(vline_lsat),
            "nominal_between_natural_combined.png", 14, 6, Figures_for_Publication)
  safe_save(create_publication_political_nominal_sd_combined(vline_pol),
            "nominal_political_combined_SD.png", 10, 6, Figures_for_Publication)

  # Income category volatility without the stretching parameter
  inc_cat_volatility <- (ger_inc_cat_volatility +
                         plot_lincat_volatilities(lincat_vline, "Australia", add_stretching = FALSE) +
                         plot_lincat_volatilities(lincat_vline, "Switzerland", add_stretching = FALSE)) +
    plot_layout(widths = c(3, 2, 2), guides = "collect") &
    theme(legend.position = "bottom")
  safe_save(inc_cat_volatility, "inc_cat_volatility_only.png", 14, 6, Figures_for_Publication)

  # Further single-country slide variants
  slide_aus_vline_mean <- create_vline_plot(vline_lsat, "Australia", "lfsato", "mean",
                                            grouping_type = "natural",
                                            show_primary_label = TRUE, show_secondary_label = TRUE,
                                            show_secondary_axis = TRUE)
  safe_save(slide_aus_vline_mean, "Rescaling_Natural_AUS_Mean.png", 8, 6, Figures_for_Slides)

  slide_ger_nominal_mean <- create_nominal_stats_plot(vline_lsat, "Germany", "lfsato", "mean",
                                                      grouping_type = "natural",
                                                      show_primary_label = TRUE, show_secondary_label = TRUE,
                                                      show_secondary_axis = TRUE)
  safe_save(slide_ger_nominal_mean, "Nominal_GER_Mean_LS.png", 8, 6, Figures_for_Slides)

  safe_save(create_summary_plot(all_vline_robustness, "Australia", "sd", y_limits = c(60, 140)),
            "Nonlinearity_Summary_SD_Australia.png", 9, 6, Figures_for_Slides)

  # Swiss mass polarisation over the full SHP panel
  safe_save(create_swiss_alltime_political_plot(),
            "swiss_alltime_political.png", 9, 6, Figures_for_Publication)

  # The paradox family: an earlier presentation of the central comparison
  safe_save(create_publication_lsat_paradox_plot1("model_5", "income"),
            "paradox_lsat_plot1_model5_wid_income.png", 14, 6, Figures_for_Publication)
  safe_save(create_publication_lsat_paradox_plot1(c("model_5", "model_1"), "income"),
            "paradox_lsat_plot1_model5_nc_wid_income.png", 14, 6, Figures_for_Publication)
  safe_save(create_publication_lsat_paradox_plot2("income"),
            "paradox_lsat_plot2_latent_vs_wid_income.png", 14, 6, Figures_for_Publication)

  for (es in names(ELITE_SETS)) {
    vars <- ELITE_SETS[[es]]
    safe_save(create_publication_political_paradox_plot1("model_5", vars),
              paste0("paradox_political_plot1_model5_", es, ".png"), 10, 6, Figures_for_Publication)
    safe_save(create_publication_political_paradox_plot1(c("model_5", "model_1"), vars),
              paste0("paradox_political_plot1_model5_nc_", es, ".png"), 10, 6, Figures_for_Publication)
    safe_save(create_publication_political_paradox_plot2(vars),
              paste0("paradox_political_plot2_latent_vs_elite_", es, ".png"), 10, 6, Figures_for_Publication)
  }

  # Mean counterpart of the period-grouping sensitivity figure
  pub_grouping_mean <- wrap_plots(make_grouping_panels("mean"), nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  safe_save(pub_grouping_mean, "comparison_mean_groupings.png", 14, 6, Figures_for_Publication)

  # Transformation curves arranged by cost rather than by shape
  safe_save(create_transform_curves_by_cost(),
            "nonlinearity_transformations_by_cost.png", 13, 5.5, Figures_for_Publication)

  # Alternative cuts of the two Appendix B figures
  safe_save(create_wid_separate_panels("indexed"),
            "wid_2a_separate_panels_indexed.png", 14, 9, Figures_for_Publication)

  for (m in c("indexed", "level")) {
    sfx <- if (m == "indexed") "indexed" else "levels"
    safe_save(create_elite_two_country(ELITE_SETS$lrgen, m),
              paste0("elite_E1_lrgen_", sfx, ".png"), 10, 5.5, Figures_for_Publication)
    safe_save(create_elite_two_country(ELITE_SETS$all3, m),
              paste0("elite_E2_all3_same_panel_", sfx, ".png"), 10, 6, Figures_for_Publication)
  }
  safe_save(create_elite_separate_panels(ELITE_SETS$all3, "level"),
            "elite_E3_separate_panels_levels.png", 14, 9, Figures_for_Publication)

} else {
  cat("\nSkipping unused figures (GENERATE_UNUSED is FALSE). See NOT_USED.md.\n")
}

cat("\n========================================\n")
cat("ALL FIGURES COMPLETE\n")
cat("========================================\n")
