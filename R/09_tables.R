# ============================================================================
# 09 - Tables A1, A2, B, C1, C2, C3, D, F, G (LaTeX bodies)
#
# V-LINE replication package. Extracted verbatim from
# model_volatilities_091126.R lines 3907-5162 (9 Nov 2026 version).
# Paths and libraries come from R/00_config.R; source via run_all.R.
# ============================================================================

# # ============================================================
# # ============================================================
# # Main Sections:
# # 9) Tables
# # ============================================================
# # ============================================================


# Load required libraries
library(gt)
library(dplyr)
library(tidyr)
library(webshot2)
# ============================================================
# ESTIMATION SAMPLE (MODEL 1) -- shared by Tables A1, A2 and B
# ============================================================
# Individuals and Observations in the descriptive tables are the people and person-periods that
# actually enter the Model 1 regressions. The filter mirrors the complete-case filter inside
# analyze_within_variance(): non-missing |dX|, weight, time variable and every Model 1 control,
# with a strictly positive weight. Control lists are read from the model specs themselves, so the
# table sample and the regression sample cannot drift apart.
# NB: Model 1 has no demographic controls (BES: only wave_pos), so age/sex/migration can be missing for some
# sampled people; the demographic means and shares in Table B are computed over non-missing values.

get_reg_sample <- function(data, sat_var, controls,
                              weight_var = "norm_weight", time_var = "year") {
  vars_needed <- c(paste0("abs_d_", sat_var), weight_var, time_var,
                   unname(vapply(controls, function(v) gsub("^.*\\((.*)\\).*$", "\\1", v), character(1))))
  data %>%
    filter(if_all(all_of(vars_needed), ~ !is.na(.))) %>%
    filter(!!sym(weight_var) > 0)
}

# Stop if the helper's N differs from the N that the Model 1 regressions actually used
check_sample_vs_model <- function(sample_df, results, variable, label) {
  r <- results[results$model_id == "model_1" & results$variable == variable, ]
  n_model <- sum(r$n_obs, na.rm = TRUE)
  cat(sprintf("  Sample check [%s]: helper N = %s | Model 1 N = %s\n", label,
              format(nrow(sample_df), big.mark = ","), format(n_model, big.mark = ",")))
  if (nrow(sample_df) != n_model) {
    stop(sprintf("Sample mismatch for %s: helper N = %d, Model 1 N = %d",
                 label, nrow(sample_df), as.integer(n_model)))
  }
  invisible(TRUE)
}


# ============================================================
# TABLE A1: Life Satisfaction Statistics (Germany, Australia, Switzerland)
# ============================================================

create_table_a1_life_satisfaction <- function() {
  
  calculate_ls_stats <- function(data, country_name, ls_var = "lfsato", 
                                 vol_var = "abs_d_lfsato", time_var = "year", reg_sample) {
    
    # Convert any haven_labelled variables to numeric
    data <- data %>%
      mutate(across(where(haven::is.labelled), as.numeric))
    
    # Get first and last time periods WITH LS DATA
    data_with_ls <- data %>% filter(!is.na(!!sym(ls_var)))
    first_period <- min(data_with_ls[[time_var]], na.rm = TRUE)
    last_period <- max(data_with_ls[[time_var]], na.rm = TRUE)
    last_period_m_1 <- last_period - 1  # For volatility (which is forward-looking)
    
    cat("Country:", country_name, "| First year with LS:", first_period, "| Last year with LS:", last_period, "\n")
    
    # Estimation sample (Model 1): Individuals, Observations and all |Δ| statistics use it
    vol_data <- reg_sample %>%
      mutate(across(where(haven::is.labelled), as.numeric))
    n_vol_obs <- nrow(vol_data)
    
    cat("  Model 1 sample observations:", n_vol_obs, "| individuals:", n_distinct(vol_data$pid), "\n")
    
    # Overall LS stats
    ls_overall <- data %>%
      filter(!is.na(!!sym(ls_var))) %>%
      summarise(
        mean_ls = mean(!!sym(ls_var), na.rm = TRUE),
        sd_ls = sd(!!sym(ls_var), na.rm = TRUE)
      )
    
    # First year LS stats
    ls_first <- data %>%
      filter(!is.na(!!sym(ls_var)), !!sym(time_var) == first_period) %>%
      summarise(
        mean_ls_first = mean(!!sym(ls_var), na.rm = TRUE),
        sd_ls_first = sd(!!sym(ls_var), na.rm = TRUE)
      )
    
    # Last year LS stats
    ls_last <- data %>%
      filter(!is.na(!!sym(ls_var)), !!sym(time_var) == last_period) %>%
      summarise(
        mean_ls_last = mean(!!sym(ls_var), na.rm = TRUE),
        sd_ls_last = sd(!!sym(ls_var), na.rm = TRUE)
      )
    
    # Overall volatility stats
    vol_overall <- vol_data %>%
      filter(!is.na(!!sym(vol_var))) %>%
      summarise(
        mean_vol = mean(!!sym(vol_var), na.rm = TRUE),
        sd_vol = sd(!!sym(vol_var), na.rm = TRUE)
      )
    
    # First year volatility stats
    vol_first <- vol_data %>%
      filter(!is.na(!!sym(vol_var)), !!sym(time_var) == first_period) %>%
      summarise(
        mean_vol_first = mean(!!sym(vol_var), na.rm = TRUE),
        sd_vol_first = sd(!!sym(vol_var), na.rm = TRUE)
      )
    
    # Last year volatility stats (use last_period - 1 since vol is forward-looking)
    vol_last <- vol_data %>%
      filter(!is.na(!!sym(vol_var)), !!sym(time_var) == last_period_m_1) %>%
      summarise(
        mean_vol_last = mean(!!sym(vol_var), na.rm = TRUE),
        sd_vol_last = sd(!!sym(vol_var), na.rm = TRUE)
      )
    
    # Combine all statistics
    tibble(
      Country = country_name,
      Individuals = scales::comma(n_distinct(vol_data$pid)),
      Observations = scales::comma(n_vol_obs),
      `Years with LS` = paste0(first_period, "–", last_period),
      `Life Satisfaction (LS)` = sprintf("%.2f (%.2f)", ls_overall$mean_ls, ls_overall$sd_ls),
      `First Year: LS` = sprintf("%.2f (%.2f)", ls_first$mean_ls_first, ls_first$sd_ls_first),
      `Last Year: LS` = sprintf("%.2f (%.2f)", ls_last$mean_ls_last, ls_last$sd_ls_last),
      `|ΔLS|` = sprintf("%.2f (%.2f)", vol_overall$mean_vol, vol_overall$sd_vol),
      `First Year: |ΔLS|` = sprintf("%.2f (%.2f)", vol_first$mean_vol_first, vol_first$sd_vol_first),
      `Last Year: |ΔLS|` = sprintf("%.2f (%.2f)", vol_last$mean_vol_last, vol_last$sd_vol_last)
    )
  }
  
  # Calculate for each country
  # Model 1 estimation samples, checked against the N each Model 1 regression actually used
  ger_m1 <- get_reg_sample(ger_final, "lfsato", model_specs$model_1$controls)
  aus_m1 <- get_reg_sample(aus_final, "lfsato", model_specs$model_1$controls)
  shp_ls_m1 <- get_reg_sample(shp_final, "sat_life", model_specs_shp$model_1$controls)
  check_sample_vs_model(ger_m1, results_ger_lsat, "lfsato", "Germany LS")
  check_sample_vs_model(aus_m1, results_aus_lsat, "lfsato", "Australia LS")
  check_sample_vs_model(shp_ls_m1, results_shp_lsat, "sat_life", "Switzerland LS")
  
  germany_stats <- calculate_ls_stats(ger_final, "Germany", 
                                      ls_var = "lfsato", vol_var = "abs_d_lfsato",
                                      reg_sample = ger_m1)
  australia_stats <- calculate_ls_stats(aus_final, "Australia", 
                                        ls_var = "lfsato", vol_var = "abs_d_lfsato",
                                        reg_sample = aus_m1)
  switzerland_stats <- calculate_ls_stats(shp_final, "Switzerland", 
                                          ls_var = "sat_life", vol_var = "abs_d_sat_life",
                                          reg_sample = shp_ls_m1)
  
  # Combine all countries
  combined_stats <- bind_rows(germany_stats, australia_stats, switzerland_stats)
  
  
  
  # Transpose: countries as columns, measures as rows
  table_a1 <- combined_stats %>%
    pivot_longer(cols = -Country, names_to = "Measure", values_to = "Value") %>%
    pivot_wider(names_from = Country, values_from = Value) %>%
    select(Measure, Germany, Australia, Switzerland)
  
  return(table_a1)
}


# ============================================================
# TABLE A2: Political Attitudes Statistics (Switzerland, UK)
# ============================================================

create_table_a2_political <- function() {
  
  # Switzerland political stats (yearly)
  calculate_pol_stats_yearly <- function(data, country_name, pol_var, vol_var, time_var = "year", reg_sample) {
    
    data <- data %>%
      mutate(across(where(haven::is.labelled), as.numeric))
    
    # Get first and last time periods WITH POLITICAL DATA
    data_with_pol <- data %>% filter(!is.na(!!sym(pol_var)))
    first_period <- min(data_with_pol[[time_var]], na.rm = TRUE)
    last_period <- max(data_with_pol[[time_var]], na.rm = TRUE)
    last_period_m_1 <- last_period - 1
    
    cat("Country:", country_name, "| First year with LR:", first_period, "| Last year with LR:", last_period, "\n")
    
    # Estimation sample (Model 1): Individuals, Observations and all |Δ| statistics use it
    vol_data <- reg_sample %>%
      mutate(across(where(haven::is.labelled), as.numeric))
    n_vol_obs <- nrow(vol_data)
    
    cat("  Model 1 sample observations:", n_vol_obs, "| individuals:", n_distinct(vol_data$pid), "\n")
    
    # Overall stats
    pol_overall <- data %>%
      filter(!is.na(!!sym(pol_var))) %>%
      summarise(
        mean_pol = mean(!!sym(pol_var), na.rm = TRUE),
        sd_pol = sd(!!sym(pol_var), na.rm = TRUE)
      )
    
    # First period stats
    pol_first <- data %>%
      filter(!is.na(!!sym(pol_var)), !!sym(time_var) == first_period) %>%
      summarise(
        mean_pol_first = mean(!!sym(pol_var), na.rm = TRUE),
        sd_pol_first = sd(!!sym(pol_var), na.rm = TRUE)
      )
    
    # Last period stats
    pol_last <- data %>%
      filter(!is.na(!!sym(pol_var)), !!sym(time_var) == last_period) %>%
      summarise(
        mean_pol_last = mean(!!sym(pol_var), na.rm = TRUE),
        sd_pol_last = sd(!!sym(pol_var), na.rm = TRUE)
      )
    
    # Volatility stats
    vol_overall <- vol_data %>%
      filter(!is.na(!!sym(vol_var))) %>%
      summarise(
        mean_vol = mean(!!sym(vol_var), na.rm = TRUE),
        sd_vol = sd(!!sym(vol_var), na.rm = TRUE)
      )
    
    vol_first <- vol_data %>%
      filter(!is.na(!!sym(vol_var)), !!sym(time_var) == first_period) %>%
      summarise(
        mean_vol_first = mean(!!sym(vol_var), na.rm = TRUE),
        sd_vol_first = sd(!!sym(vol_var), na.rm = TRUE)
      )
    
    vol_last <- vol_data %>%
      filter(!is.na(!!sym(vol_var)), !!sym(time_var) == last_period_m_1) %>%
      summarise(
        mean_vol_last = mean(!!sym(vol_var), na.rm = TRUE),
        sd_vol_last = sd(!!sym(vol_var), na.rm = TRUE)
      )
    
    tibble(
      Country = country_name,
      Individuals = scales::comma(n_distinct(vol_data$pid)),
      Observations = scales::comma(n_vol_obs),
      `Years with LR` = paste0(first_period, "–", last_period),
      `Left-Right (LR)` = sprintf("%.2f (%.2f)", pol_overall$mean_pol, pol_overall$sd_pol),
      `First Year: LR` = sprintf("%.2f (%.2f)", pol_first$mean_pol_first, pol_first$sd_pol_first),
      `Last Year: LR` = sprintf("%.2f (%.2f)", pol_last$mean_pol_last, pol_last$sd_pol_last),
      `|ΔLR|` = sprintf("%.2f (%.2f)", vol_overall$mean_vol, vol_overall$sd_vol),
      `First Year: |ΔLR|` = sprintf("%.2f (%.2f)", vol_first$mean_vol_first, vol_first$sd_vol_first),
      `Last Year: |ΔLR|` = sprintf("%.2f (%.2f)", vol_last$mean_vol_last, vol_last$sd_vol_last)
    )
  }
  
  # UK BES political stats (by election)
  calculate_pol_stats_election <- function(data, country_name, pol_var = "lr",
                                           vol_var = "abs_d_lr", time_var = "election",
                                           reg_sample) {
    
    data <- data %>%
      mutate(across(where(haven::is.labelled), as.numeric))
    
    # Estimation sample (Model 1): Individuals, Observations and all |d| statistics use it
    vol_data <- reg_sample %>%
      mutate(across(where(haven::is.labelled), as.numeric)) %>%
      filter(!is.na(!!sym(vol_var)))
    n_vol_obs <- nrow(vol_data)
    
    # First and last elections, defined separately for levels and for volatility
    lr_elections  <- sort(unique(data[[time_var]][!is.na(data[[pol_var]])]))
    vol_elections <- sort(unique(vol_data[[time_var]]))
    first_election <- min(lr_elections);  last_election <- max(lr_elections)
    first_vol      <- min(vol_elections); last_vol      <- max(vol_elections)
    
    # Check: volatility should cover the same elections as levels
    cat("Country:", country_name, "\n")
    cat("  Elections with LR:  ", paste(lr_elections,  collapse = ", "), "\n")
    cat("  Elections with |dLR|:", paste(vol_elections, collapse = ", "), "\n")
    print(table(vol_data[[time_var]]))   # |dLR| observations per election

    cat("  Model 1 sample observations:", n_vol_obs,
        "| individuals:", n_distinct(vol_data$pid), "\n")
    
    mean_sd <- function(d, var, period = NULL) {
      if (!is.null(period)) d <- d[which(d[[time_var]] == period), ]
      x <- d[[var]][!is.na(d[[var]])]
      stopifnot(length(x) > 1)
      sprintf("%.2f (%.2f)", mean(x), sd(x))
    }
    
    tibble(
      Country = country_name,
      Individuals  = scales::comma(n_distinct(vol_data$pid)),
      Observations = scales::comma(n_vol_obs),
      `Years with LR` = "2015–2024",   # General Elections 2015 to 2024 (elections coded 1-5)
      `Left-Right (LR)`       = mean_sd(data, pol_var),
      `First Election: LR`    = mean_sd(data, pol_var, first_election),
      `Last Election: LR`     = mean_sd(data, pol_var, last_election),
      `|ΔLR|`                 = mean_sd(vol_data, vol_var),
      `First Election: |ΔLR|` = mean_sd(vol_data, vol_var, first_vol),
      `Last Election: |ΔLR|`  = mean_sd(vol_data, vol_var, last_vol)
    )
  }
  
  # Calculate for each country
  # Model 1 estimation samples, checked against the N each Model 1 regression actually used
  shp_lr_m1 <- get_reg_sample(shp_politics, "lrself", model_specs_shp$model_1$controls)
  bes_m1 <- get_reg_sample(bes_elections_final, "lr", model_specs_bes$model_1$controls,
                              weight_var = "election_weight", time_var = "election")
  check_sample_vs_model(shp_lr_m1, results_shp_pol, "lrself", "Switzerland LR")
  check_sample_vs_model(bes_m1, results_bes_election, "lr", "UK (BES) LR")
  
  switzerland_stats <- calculate_pol_stats_yearly(shp_politics, "Switzerland (SHP)", 
                                                  pol_var = "lrself", vol_var = "abs_d_lrself",
                                                  reg_sample = shp_lr_m1)
  uk_stats <- calculate_pol_stats_election(bes_elections_final, "UK (BES)", 
                                           pol_var = "lr", vol_var = "abs_d_lr",
                                           reg_sample = bes_m1)
  
  # Harmonise row names for UK (First/Last Election -> First/Last Year for display consistency)
  # But keep them separate so it's clear what they mean
  switzerland_stats_renamed <- switzerland_stats %>%
    rename(
      `First Period: LR` = `First Year: LR`,
      `Last Period: LR` = `Last Year: LR`,
      `First Period: |ΔLR|` = `First Year: |ΔLR|`,
      `Last Period: |ΔLR|` = `Last Year: |ΔLR|`
    )
  
  uk_stats_renamed <- uk_stats %>%
    rename(
      `First Period: LR` = `First Election: LR`,
      `Last Period: LR` = `Last Election: LR`,
      `First Period: |ΔLR|` = `First Election: |ΔLR|`,
      `Last Period: |ΔLR|` = `Last Election: |ΔLR|`
    )
  
  # Combine
  combined_stats <- bind_rows(switzerland_stats_renamed, uk_stats_renamed)
  
  # Transpose: countries as columns, measures as rows
  table_a2 <- combined_stats %>%
    pivot_longer(cols = -Country, names_to = "Measure", values_to = "Value") %>%
    pivot_wider(names_from = Country, values_from = Value) %>%
    select(Measure, `Switzerland (SHP)`, `UK (BES)`)
  
  return(table_a2)
}


# ============================================================
# GENERATE AND DISPLAY TABLES A1 AND A2
# ============================================================

cat("\n")
cat("========================================\n")
cat("GENERATING TABLES A1 AND A2\n")
cat("========================================\n\n")

# ========== TABLE A1: Life Satisfaction ==========
cat("Generating Table A1: Life Satisfaction Statistics...\n")
table_a1_data <- create_table_a1_life_satisfaction()

cat("\n--- Table A1 Preview ---\n")
print(table_a1_data, n = 20)

# Create gt table for display
table_a1_gt <- table_a1_data %>%
  gt() %>%
  tab_header(title = "Table A1: Life Satisfaction Levels and Within-Person Volatility") %>%
  tab_source_note(source_note = md("*Note:* LS = Life Satisfaction (0–10 scale). |ΔLS| = absolute year-to-year change in life satisfaction. Individuals and Observations refer to the Model 1 estimation sample: person-years with a valid |ΔLS| and a positive weight. |ΔLS| statistics use this sample; levels of LS use all valid responses. Values shown as Mean (SD).")) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = 1)
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
    locations = cells_body(columns = everything(), rows = Measure %in% c("Years with LS", "Last Year: LS"))
  )

# Display
table_a1_gt


# ========== TABLE A2: Political Attitudes ==========
cat("\n\nGenerating Table A2: Political Attitudes Statistics...\n")
table_a2_data <- create_table_a2_political()

cat("\n--- Table A2 Preview ---\n")
print(table_a2_data, n = 20)

# Create gt table for display
table_a2_gt <- table_a2_data %>%
  gt() %>%
  tab_header(title = "Table A2: Political Attitudes Levels and Within-Person Volatility") %>%
  tab_source_note(source_note = md("*Note:* LR = Left-Right self-placement (0–10 scale). |ΔLR| = absolute change in left-right position. Individuals and Observations refer to the Model 1 estimation sample: person-periods with a valid |ΔLR| and a positive weight. |ΔLR| statistics use this sample; levels of LR use all valid responses. Switzerland: yearly observations. UK: within-election volatility across General Elections (2015, 2017, 2019, 2024) and Brexit (2016). Values shown as Mean (SD).")) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = 1)
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
    locations = cells_body(columns = everything(), rows = Measure %in% c("Years with LR", "Last Period: LR"))
  )

# Display
table_a2_gt



cat("\n========================================\n")
cat("TABLES A1 AND A2 COMPLETE\n")
cat("========================================\n")



# ============================================================
# TABLE B: Panel Structure and Demographics (All 4 Datasets)
# ============================================================

create_table_b_panel_demographics <- function() {
  
  # Model 1 estimation samples (life satisfaction for SOEP, HILDA and SHP; left-right for SHP and BES),
  # checked against the N each Model 1 regression actually used
  ger_m1    <- get_reg_sample(ger_final, "lfsato", model_specs$model_1$controls)
  aus_m1    <- get_reg_sample(aus_final, "lfsato", model_specs$model_1$controls)
  shp_ls_m1 <- get_reg_sample(shp_final, "sat_life", model_specs_shp$model_1$controls)
  shp_lr_m1 <- get_reg_sample(shp_politics, "lrself", model_specs_shp$model_1$controls)
  bes_m1    <- get_reg_sample(bes_elections_final, "lr", model_specs_bes$model_1$controls,
                              weight_var = "election_weight", time_var = "election")
  check_sample_vs_model(ger_m1,    results_ger_lsat,     "lfsato",   "Germany LS")
  check_sample_vs_model(aus_m1,    results_aus_lsat,     "lfsato",   "Australia LS")
  check_sample_vs_model(shp_ls_m1, results_shp_lsat,     "sat_life", "Switzerland LS")
  check_sample_vs_model(shp_lr_m1, results_shp_pol,      "lrself",   "Switzerland LR")
  check_sample_vs_model(bes_m1,    results_bes_election, "lr",       "UK (BES) LR")
  
  # Helper for standard yearly panels (Germany, Australia, Switzerland)
  calculate_stats_yearly <- function(data, country_name, 
                                     sex_var = "sex", time_var = "year",
                                     tenure_var, has_migback = TRUE, rvar, reg_sample) {
    
    data <- data %>%
      mutate(across(where(haven::is.labelled), as.numeric))
    
    # Years with a valid response (same definition as Tables A1 and A2)
    years_valid <- data[[time_var]][!is.na(data[[rvar]])]
    year_range  <- paste0(min(years_valid, na.rm = TRUE), "\u2013", max(years_valid, na.rm = TRUE))
    
    # Everything below describes the Model 1 estimation sample
    reg_sample <- reg_sample %>%
      mutate(across(where(haven::is.labelled), as.numeric))
    
    # Mean Waves = valid responses (non-missing outcome) per person in the estimation sample
    person_stats <- data %>%
      filter(!is.na(!!sym(rvar)), pid %in% reg_sample$pid) %>%
      group_by(pid) %>%
      summarise(n_waves = n(), .groups = "drop")
    
    # Total observations (rows) in the estimation sample
    n_observations <- nrow(reg_sample)
    
    # Get baseline (first observation per person) for demographics
    baseline_data <- reg_sample %>%
      arrange(pid, !!sym(time_var)) %>%
      group_by(pid) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        age = as.numeric(age),
        sex_numeric = as.numeric(!!sym(sex_var))
      )
    
    # Calculate migrant percentage if available
    if (has_migback && "migback_collapsed" %in% names(baseline_data)) {
      baseline_data <- baseline_data %>%
        mutate(migback_collapsed = as.numeric(as.character(migback_collapsed)))
      # migback_collapsed: 0 = native-born, 1 = migrant background
      migrant_pct <- sprintf("%.1f", mean(baseline_data$migback_collapsed == 1, na.rm = TRUE) * 100)
    } else {
      migrant_pct <- "—"
    }
    
    # Female percentage (sex == 1 typically means female in GSOEP/HILDA/SHP)
    female_pct <- mean(baseline_data$sex_numeric == 1, na.rm = TRUE) * 100
    
    tibble(
      Country = country_name,
      Years = year_range,
      Individuals = scales::comma(n_distinct(reg_sample$pid)),
      Observations = scales::comma(n_observations),
      `New Respondent Obs.` = NA_character_,   # filled from Model 6 below
      `Balanced Panel Obs.` = NA_character_,   # filled from Model 7 below
      `Mean Waves` = sprintf("%.1f", mean(person_stats$n_waves)),
      `Mean Age` = sprintf("%.1f", mean(baseline_data$age, na.rm = TRUE)),
      `Female (%)` = sprintf("%.1f", female_pct),
      `Migrant (%)` = migrant_pct
    )
  }
  
  # Helper for BES election-based panel
  calculate_stats_elections <- function(data, country_name, year_range,
                                        time_var = "election", wave_var, rvar = "lr", reg_sample) {
    
    data <- data %>%
      mutate(across(where(haven::is.labelled), as.numeric))
    reg_sample <- reg_sample %>%
      mutate(across(where(haven::is.labelled), as.numeric))
    
    # Mean Waves = waves with a valid left-right answer, per person in the estimation sample
    person_stats <- data %>%
      filter(!is.na(!!sym(rvar)), pid %in% reg_sample$pid) %>%
      group_by(pid) %>%
      summarise(n_waves = n_distinct(!!sym(wave_var)), .groups = "drop")
    
    # Total observations (rows) in the estimation sample
    n_observations <- nrow(reg_sample)
    
    # Get baseline (first observation per person) for demographics
    baseline_data <- reg_sample %>%
      arrange(pid, !!sym(time_var)) %>%
      group_by(pid) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        age = as.numeric(age),
        female_numeric = as.numeric(female)
      )
    
    tibble(
      Country = country_name,
      Years = year_range,
      Individuals = scales::comma(n_distinct(reg_sample$pid)),
      Observations = scales::comma(n_observations),
      `New Respondent Obs.` = NA_character_,   # filled from Model 6 below
      `Balanced Panel Obs.` = NA_character_,   # filled from Model 7 below
      `Mean Waves` = sprintf("%.1f", mean(person_stats$n_waves)),
      `Mean Age` = sprintf("%.1f", mean(baseline_data$age, na.rm = TRUE)),
      `Female (%)` = sprintf("%.1f", mean(baseline_data$female_numeric == 1, na.rm = TRUE) * 100),
      `Migrant (%)` = "—"
    )
  }
  
  # Calculate stats for each sample
  cat("\n--- Calculating Germany stats ---\n")
  germany_stats <- calculate_stats_yearly(
    data = ger_final, rvar = "lfsato", reg_sample = ger_m1,
    country_name = "Germany (GSOEP)", tenure_var = "panel_tenure"
  )
  
  cat("\n--- Calculating Australia stats ---\n")
  australia_stats <- calculate_stats_yearly(
    data = aus_final, rvar = "lfsato", reg_sample = aus_m1,
    country_name = "Australia (HILDA)", tenure_var = "panel_tenure"
  )
  
  cat("\n--- Calculating Switzerland (life satisfaction) stats ---\n")
  switzerland_ls_stats <- calculate_stats_yearly(
    data = shp_final, rvar = "sat_life", reg_sample = shp_ls_m1,
    country_name = "Switzerland (SHP, LS)", tenure_var = "panel_tenure"
  )
  
  cat("\n--- Calculating Switzerland (politics) stats ---\n")
  switzerland_lr_stats <- calculate_stats_yearly(
    data = shp_politics, rvar = "lrself", reg_sample = shp_lr_m1,
    country_name = "Switzerland (SHP, LR)", tenure_var = "panel_tenure"
  )
  
  cat("\n--- Calculating UK stats ---\n")
  uk_stats <- calculate_stats_elections(
    data = bes_elections_final, rvar = "lr", reg_sample = bes_m1,
    country_name = "UK (BES)", year_range = "2015–2024",
    time_var = "election", wave_var = "wave"
  )
  
  # Combine all samples
  combined_stats <- bind_rows(
    germany_stats,
    australia_stats,
    switzerland_ls_stats,
    switzerland_lr_stats,
    uk_stats
  )
  
  # New Respondent Obs. and Balanced Panel Obs. = observations used by the
  # Model 6 and Model 7 regressions (row order matches bind_rows above)
  model_n <- function(results, variable, model) {
    r <- results[results$model_id == model & results$variable == variable, ]
    sum(r$n_obs, na.rm = TRUE)
  }
  model_n_all <- function(model) {
    scales::comma(c(
      model_n(results_ger_lsat,     "lfsato",   model),
      model_n(results_aus_lsat,     "lfsato",   model),
      model_n(results_shp_lsat,     "sat_life", model),
      model_n(results_shp_pol,      "lrself",   model),
      model_n(results_bes_election, "lr",       model)
    ))
  }
  combined_stats$`New Respondent Obs.` <- model_n_all("model_6")
  combined_stats$`Balanced Panel Obs.` <- model_n_all("model_7")
  
  # Transpose: samples as columns, measures as rows
  table_b <- combined_stats %>%
    pivot_longer(cols = -Country, names_to = "Measure", values_to = "Value") %>%
    pivot_wider(names_from = Country, values_from = Value) %>%
    select(Measure, `Germany (GSOEP)`, `Australia (HILDA)`,
           `Switzerland (SHP, LS)`, `Switzerland (SHP, LR)`, `UK (BES)`)
  
  return(table_b)
}

# ============================================================
# GENERATE AND DISPLAY TABLE B
# ============================================================

cat("\n")
cat("========================================\n")
cat("GENERATING TABLE B\n")
cat("========================================\n\n")

cat("Generating Table B: Panel Structure and Demographics...\n")
table_b_data <- create_table_b_panel_demographics()

cat("\n--- Table B Preview ---\n")
print(table_b_data, n = 20)

# Create gt table for display
table_b_gt <- table_b_data %>%
  gt() %>%
  tab_header(title = "Table B: Panel Structure and Demographic Composition") %>%
  tab_source_note(source_note = md("*Note:* Individuals, Observations and New Respondent Obs. refer to the Model 1 estimation sample (person-periods with a valid volatility measure and a positive weight; life satisfaction for SOEP, HILDA and SHP, left-right for BES). New Respondent Obs. = sample observations from an individual's first or second valid response. Mean Waves = valid responses per sampled individual (BES: waves, out of 15, with a valid left-right answer). Balanced Panel = sampled individuals observed in every period of the sample (BES: at least two valid |ΔLR| in each of the five elections). Demographics are measured at each individual's first observation in the sample and are unweighted; means and shares exclude missing values. Migrant background not available for BES.")) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = 1)
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
    locations = cells_body(columns = everything(), rows = Measure %in% c("Years", "Mean Waves"))
  )

table_b_gt


cat("\n========================================\n")
cat("TABLE B COMPLETE\n")
cat("========================================\n")
table_b_data




# ============================================================
# ============================================================
# TABLES C1, C2 — AGGREGATED / UNWEIGHTED (with one
# deliberate exception) TIME-TREND REGRESSIONS
#
# Logic: collapse to one indexed-volatility data point per
# period (already computed via analyze_within_variance /
# run_all_models), then regress that period-level series on
# years_since_start via plain OLS.
#
# Weighting: OFF by default. The only row weighted by n_obs is
# "New Respondents Only", where per-period sample size is very
# spiky.
# Every other row treats each period as equally informative.
# ============================================================
# ============================================================

library(gt)

# ============================================================
# CORE FUNCTION: OLS trend on an already-indexed series
# ============================================================

fit_indexed_trend_aggregated <- function(period_data,
                                         time_var = "time_period",
                                         value_var = "indexed_within_var_interpolated",
                                         weight_var = "n_obs",
                                         weighted = FALSE) {
  
  d <- period_data %>%
    filter(!is.na(!!sym(value_var)))
  
  if (weighted) {
    d <- d %>% filter(!is.na(!!sym(weight_var)), !!sym(weight_var) > 0)
  }
  
  n_periods <- nrow(d)
  
  if (n_periods < 3) {
    warning(paste("Only", n_periods, "valid period(s) - trend not estimated"))
    return(tibble(
      n_periods = n_periods,
      total_n = if (weighted) sum(d[[weight_var]]) else NA_real_,
      intercept = NA_real_,
      time_coefficient = NA_real_, time_se = NA_real_, time_pvalue = NA_real_
    ))
  }
  
  first_period <- min(d[[time_var]], na.rm = TRUE)
  d <- d %>% mutate(years_since_start = !!sym(time_var) - first_period)
  
  formula_obj <- as.formula(paste(value_var, "~ years_since_start"))
  
  model <- if (weighted) {
    lm(formula_obj, data = d, weights = d[[weight_var]])
  } else {
    lm(formula_obj, data = d)
  }
  
  cs <- tidy(model)
  
  tibble(
    n_periods        = n_periods,
    total_n          = if (weighted) sum(d[[weight_var]]) else sum(!is.na(d[[value_var]])),
    intercept        = cs$estimate[cs$term == "(Intercept)"],
    time_coefficient = cs$estimate[cs$term == "years_since_start"],
    time_se          = cs$std.error[cs$term == "years_since_start"],
    time_pvalue      = cs$p.value[cs$term == "years_since_start"]
  )
}


# ============================================================
# WRAPPER: apply across every (country, model, variable) group
# Only "New Respondents Only" gets weighted = TRUE.
# ============================================================

run_all_models_aggregated <- function(period_results,
                                      group_vars = c("country", "model_id", "model_label", "variable"),
                                      time_var = "time_period",
                                      value_var = "indexed_within_var_interpolated",
                                      weight_var = "n_obs") {
  
  period_results %>%
    group_by(across(all_of(group_vars))) %>%
    group_modify(~ fit_indexed_trend_aggregated(
      period_data = .x,
      time_var = time_var,
      value_var = value_var,
      weight_var = weight_var,
      weighted = any(.x$model_label == "New Respondents Only")
    )) %>%
    ungroup()
}


# ============================================================
# FORMAT: Add significance stars (aggregated version)
# ============================================================

format_results_with_stars_agg <- function(results) {
  results %>%
    mutate(
      stars = case_when(
        time_pvalue < 0.001 ~ "***",
        time_pvalue < 0.01  ~ "**",
        time_pvalue < 0.05  ~ "*",
        time_pvalue < 0.10  ~ "+",
        TRUE ~ ""
      ),
      formatted = ifelse(
        is.na(time_coefficient),
        sprintf("— (n_periods=%d)", n_periods),
        sprintf("%.3f%s\n(%.3f)", time_coefficient, stars, time_se)
      )
    )
}


# ============================================================
# HELPER: robust numeric sort key for model_id, independent of
# whether upstream code used "model_1" or "Model 1" style, and
# handling R1/R2/R3 robustness rows explicitly.
# ============================================================

model_sort_key <- function(model_id) {
  case_when(
    model_id == "R1" ~ 9.1,
    model_id == "R2" ~ 9.2,
    model_id == "R3" ~ 9.3,
    model_id == "R4" ~ 9.4,
    TRUE ~ suppressWarnings(as.numeric(gsub("[^0-9]", "", model_id)))
  )
}


# ============================================================
# ============================================================
# TABLE C1 (AGGREGATED, UNWEIGHTED): LIFE SATISFACTION
# ============================================================
# ============================================================

cat("\n############################################################\n")
cat("# TABLE C1 (Aggregated): LIFE SATISFACTION TIME TRENDS\n")
cat("############################################################\n")

lsat_period_results <- bind_rows(results_ger_lsat, results_aus_lsat, results_shp_lsat) %>%
  filter(variable %in% c("lfsato", "sat_life"))

results_c1_agg <- run_all_models_aggregated(lsat_period_results) %>%
  format_results_with_stars_agg()

table_c1_data <- results_c1_agg %>%
  select(model_id, model_label, country, formatted) %>%
  pivot_wider(names_from = country, values_from = formatted, id_cols = c(model_id, model_label)) %>%
  mutate(sort_order = model_sort_key(model_id)) %>%
  arrange(sort_order) %>%
  select(-sort_order) %>%
  rename(Model = model_id, Specification = model_label) %>%
  select(Model, Specification, any_of(c("Germany", "Australia", "Switzerland")))

cat("\n--- Table C1 (Aggregated) Preview ---\n")
print(table_c1_data, n = 10)

# ============================================================
# GT TABLE: C1
# ============================================================

table_c1_gt <- table_c1_data %>%
  gt() %>%
  tab_header(title = "Table C1: Indexed Time Trends in Within-Person Variation \u2013 Life Satisfaction") %>%
  tab_source_note(source_note = md("*Note:* Coefficient from OLS regression of indexed within-person volatility on years since panel start, one observation per period. Standard errors in parentheses. + p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001.")) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = 1)
  )

# Display
table_c1_gt


# ============================================================
# ============================================================
# TABLE C2 (AGGREGATED, UNWEIGHTED): POLITICAL ATTITUDES
# ============================================================
# ============================================================
# Main table (C2): Models 1-7, both countries.
# UK robustness rows (C3):
#   R1 = election position only  (comes from results_bes_election, via model_specs_bes)
#   R2 = pre-election volatility only     (Model 5 controls)
#   R3 = across-election volatility only  (Model 5 controls)
#   R4 = quasi-yearly volatility          (Model 5 controls)

cat("\n############################################################\n")
cat("# TABLE C2 (Aggregated): POLITICAL ATTITUDES TIME TRENDS\n")
cat("############################################################\n")

# Sort key: models 1-8 by number, then R1-R4
model_sort_key <- function(model_id) {
  case_when(
    model_id == "R1" ~ 9.1,
    model_id == "R2" ~ 9.2,
    model_id == "R3" ~ 9.3,
    model_id == "R4" ~ 9.4,
    TRUE ~ suppressWarnings(as.numeric(gsub("[^0-9]", "", model_id)))
  )
}

pol_period_results <- bind_rows(
  results_shp_pol %>% mutate(country = "Switzerland (SHP)"),
  results_bes_election %>% mutate(country = "UK (BES)")
)

# Models 1-8 and R1 (R1: UK only)
results_c2_agg <- run_all_models_aggregated(pol_period_results) %>%
  format_results_with_stars_agg()

# --- BES robustness checks R2-R4: build period-level series first
# (analyze_within_variance), then aggregate-then-regress each.
# None of these are "New Respondents Only", so all unweighted.

bes_m5_controls_no_pos <- c("factor(panel_tenure)", "days_gap", demo_controls_bes)

cat("\n--- UK Robustness: Pre-election volatility (R2) ---\n")
results_bes_pre_period <- analyze_within_variance(
  data = bes_elections_final %>% filter(wave_pos == 1),
  sat_var = "lr", weight_var = "election_weight", time_var = "election",
  control_vars = bes_m5_controls_no_pos, country_name = "UK (BES)"
)
r2_agg <- fit_indexed_trend_aggregated(results_bes_pre_period, time_var = "time_period") %>%
  mutate(model_id = "R2", model_label = "Pre-election volatility (M5)", country = "UK (BES)")

cat("\n--- UK Robustness: Across-election volatility (R3) ---\n")
results_bes_swing_period <- analyze_within_variance(
  data = bes_elections_final %>% filter(wave_pos != 1),
  sat_var = "lr", weight_var = "election_weight", time_var = "election",
  control_vars = bes_m5_controls_no_pos, country_name = "UK (BES)"
)
r3_agg <- fit_indexed_trend_aggregated(results_bes_swing_period, time_var = "time_period") %>%
  mutate(model_id = "R3", model_label = "Across-election volatility (M5)", country = "UK (BES)")

cat("\n--- UK Robustness: Quasi-yearly volatility (R4) ---\n")
bes_yearly_final <- bes_yearly %>%
  mutate(
    year = as.numeric(year),
    lr_count = as.numeric(lr_count)
  ) %>%
  rename(pid = id)

results_bes_yearly_period <- analyze_within_variance(
  data = bes_yearly_final,
  sat_var = "lr", weight_var = "norm_weight", time_var = "year",
  control_vars = bes_m5_controls_no_pos, country_name = "UK (BES)"
)
r4_agg <- fit_indexed_trend_aggregated(results_bes_yearly_period, time_var = "time_period") %>%
  mutate(model_id = "R4", model_label = "Quasi-yearly volatility (M5)", country = "UK (BES)")

results_c2_agg_full <- bind_rows(results_c2_agg, r2_agg, r3_agg, r4_agg) %>%
  format_results_with_stars_agg()


# Harmonise labels so each model is ONE row after pivoting
# (Model 5's spec name differs between SHP and BES)
table_c2_data <- results_c2_agg_full %>%
  mutate(model_label = case_when(
    model_id == "model_5" ~ "Panel Tenure (Factor) + Demographics",
    TRUE ~ model_label
  )) %>%
  select(model_id, model_label, country, formatted) %>%
  pivot_wider(names_from = country, values_from = formatted, id_cols = c(model_id, model_label)) %>%
  mutate(sort_order = model_sort_key(model_id)) %>%
  arrange(sort_order) %>%
  select(-sort_order) %>%
  rename(Model = model_id, Specification = model_label) %>%
  select(Model, Specification, any_of(c("Switzerland (SHP)", "UK (BES)")))


# ============================================================
# GT TABLE: C2
# ============================================================

table_c2_gt <- table_c2_data %>%
  gt() %>%
  tab_header(title = "Table C2: Indexed Time Trends in Within-Person Variation \u2013 Political Attitudes") %>%
  tab_source_note(source_note = md("*Note:* Coefficient from OLS regression of indexed within-person volatility on years since panel start, one observation per period. Switzerland: yearly trends. UK: election-cycle trends (R1-R4 are robustness checks). Standard errors in parentheses. + p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001.")) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = 1)
  ) %>%
  tab_style(
    style = cell_borders(sides = "top", color = "black", weight = px(2)),
    locations = cells_body(columns = everything(), rows = Model == "R1")
  )

# Display
table_c2_gt



# ============================================================
# ============================================================
# SAVE ALL TABLES (LaTeX bodies only)
# Rows only: caption, column headers, rules and notes live in Overleaf.
# ============================================================
# ============================================================

dir.create(Tables_dir, showWarnings = FALSE, recursive = TRUE)

to_latex_text <- function(x) {
  x[is.na(x)] <- ""
  x <- gsub("\u2014 \\(n_periods=[0-9]+\\)", "\u2014", x)                 # missing trend -> dash
  x <- gsub("\\|\u0394([A-Za-z]+)\\|", "$|\\\\Delta \\\\mathrm{\\1}|$", x)  # |ΔLS| -> $|\Delta \mathrm{LS}|$
  x <- gsub("\u2013", "--", x)                                           # en dash (year ranges)
  x <- gsub("\u2014", "---", x)                                          # em dash
  x <- gsub("([%&_])", "\\\\\\1", x)                                     # escape % & _
  x <- gsub("(^|[ (])-([0-9])", "\\1$-$\\2", x)                          # true minus signs
  x <- sub("([0-9])([*+]+)$", "\\1$^{\\2}$", x)                          # stars as superscripts
  x
}

write_tex_body <- function(df, file, rule_after = character(0)) {
  first_col <- as.character(df[[1]])
  lines <- character(0)
  for (i in seq_len(nrow(df))) {
    cells  <- vapply(df[i, ], function(v) as.character(v), character(1))
    cells[is.na(cells)] <- ""
    parts  <- strsplit(cells, "\n", fixed = TRUE)
    top    <- vapply(parts, function(p) if (length(p) >= 1) p[1] else "", character(1))
    bottom <- vapply(parts, function(p) if (length(p) >= 2) p[2] else "", character(1))
    lines  <- c(lines, paste0(paste(to_latex_text(top), collapse = " & "), " \\\\"))
    if (any(nzchar(bottom))) {                     # coefficient tables: SE on its own row
      lines <- c(lines, paste0(paste(to_latex_text(bottom), collapse = " & "), " \\\\"))
    }
    if (first_col[i] %in% rule_after) lines <- c(lines, "\\midrule")
  }
  writeLines(lines, file)
}

cat("Saving LaTeX table bodies...\n")

write_tex_body(table_a1_data, file.path(Tables_dir, "Table_A1_body.tex"),
               rule_after = c("Years with LS", "Last Year: LS"))

write_tex_body(table_a2_data, file.path(Tables_dir, "Table_A2_body.tex"),
               rule_after = c("Years with LR", "Last Period: LR"))

row_order <- c("Years", "Individuals", "Mean Waves", "Observations",
               "New Respondent Obs.", "Balanced Panel Obs.",
               "Mean Age", "Female (%)", "Migrant (%)")
table_b_tex <- table_b_data %>% slice(match(row_order, Measure))
write_tex_body(table_b_tex, file.path(Tables_dir, "Table_B_body.tex"),
               rule_after = c("Years", "Balanced Panel Obs."))



c1_tex <- table_c1_data %>% mutate(Model = sub("^model_", "", Model))
write_tex_body(c1_tex, file.path(Tables_dir, "Table_C1_body.tex"))

# Table C2: main models (1-7), both countries
c2_tex <- table_c2_data %>%
  filter(!Model %in% c("model_8", "R1", "R2", "R3", "R4")) %>%
  mutate(Model = sub("^model_", "", Model))
write_tex_body(c2_tex, file.path(Tables_dir, "Table_C2_body.tex"))

# Table C3: UK robustness checks
c3_order <- c("model_1", "R1", "model_5", "model_8", "R2", "R3", "R4")
c3_tex <- table_c2_data %>%
  filter(Model %in% c3_order) %>%
  slice(match(c3_order, Model)) %>%
  mutate(Specification = case_when(
    Model == "model_1" ~ "No controls (Model 1)",
    Model == "R1"      ~ "Election position only",
    Model == "model_5" ~ "Main specification (Model 5)",
    Model == "model_8" ~ "Without days control",
    Model == "R2"      ~ "Pre-election volatility only",
    Model == "R3"      ~ "Across-election volatility only",
    Model == "R4"      ~ "Quasi-yearly volatility"
  )) %>%
  select(Specification, `UK (BES)`)
write_tex_body(c3_tex, file.path(Tables_dir, "Table_C3_body.tex"),
               rule_after = c("Election position only", "Main specification (Model 5)"))



# Table D: Income validation, mean absolute error (paper: tab:mae_lincat)
table_d_data <- sd_mae_comparison %>%
  transmute(
    Country,
    Nominal       = sprintf("%.2f", MAE_Nominal),
    Adjusted      = sprintf("%.2f", MAE_Adjusted),       # V-LINE, no Assumption 4
    Homoskedastic = sprintf("%.2f", MAE_Homoskedastic)   # V-LINE, with Assumption 4
  )
write_tex_body(table_d_data, file.path(Tables_dir, "Table_D_body.tex"))




# ============================================================
# Table F: Sensitivity of the hidden divergence to omitted confounders
# (paper: Table 11). LaTeX body only -- caption, headers, rules, notes in Overleaf.
# Columns: Country | Variable | Raw Coefficient | Index-Point Effect |
#          Allowable Multiples | Allowable Decline in Latent Volatility
# Allowable Decline is one value per country, centred with \multirow
# (needs \usepackage{multirow} in the preamble).
# Run AFTER sensitivity_summary and to_latex_text() / write_tex_body() exist.
# ============================================================

table_f_data <- sensitivity_summary %>%
  mutate(
    is_uk   = startsWith(country, "UK"),
    Country = ifelse(is_uk, "UK (BES)", country),
    Variable = case_when(
      term == "panel_tenure" & !is_uk ~ "Panel tenure",
      term == "panel_tenure" &  is_uk ~ "Panel tenure (waves)",
      term == "age"                   ~ "Age",
      term == "sex"                   ~ "Sex (Male$\\rightarrow$Female)",
      term == "migback_collapsed1"    ~ "Migrant (Native$\\rightarrow$Migrant)",
      term == "female"                ~ "Gender (Male$\\rightarrow$Female)",
      TRUE ~ NA_character_
    )
  )

# Check: every coefficient got a label, and each country has the expected rows
print(table_f_data %>% select(country, term, Variable), n = Inf)
stopifnot(!any(is.na(table_f_data$Variable)))
print(table(table_f_data$country))   # expect Germany 4, UK (BES) 3

# Format cells; Allowable Decline shown once per country, spanning its rows
table_f_tex <- table_f_data %>%
  group_by(Country) %>%
  mutate(
    `Allowable Decline` = ifelse(row_number() == 1,
                                 sprintf("\\multirow{%d}{*}{%.2f%%}", n(), first(x_star)),
                                 "")
  ) %>%
  ungroup() %>%
  transmute(
    Country,
    Variable,
    `Raw Coefficient`     = sprintf("%+.4f", estimate),
    `Index-Point Effect`  = sprintf("%+.2f", index_point_coefficient),
    `Allowable Multiples` = sprintf("%.2f", abs(swings_needed)),
    `Allowable Decline`
  )

f_file <- file.path(Tables_dir, "Table_F_body.tex")
write_tex_body(table_f_tex, f_file)

# Add a rule between the two countries (after the last Germany row)
f_lines  <- readLines(f_file)
n_ger    <- sum(table_f_tex$Country == "Germany")
f_lines  <- append(f_lines, "\\midrule", after = n_ger)
writeLines(f_lines, f_file)


# # ============================================================
# # ============================================================
# # SAVE ALL TABLES
# # ============================================================
# # ============================================================
# 
# 
# # # Clear and recreate directory
# # if (dir.exists(Tables_dir)) {
# #   unlink(Tables_dir, recursive = TRUE)
# # }
# # dir.create(Tables_dir, recursive = TRUE)
# 
# cat("\n")
# cat("========================================\n")
# cat("SAVING ALL TABLES\n")
# cat("========================================\n\n")
# 
# # --- PNG tables ---
# cat("Saving PNGs...\n")
# gtsave(table_a1_gt, filename = file.path(Tables_dir, "Table_A1_life_satisfaction_stats.png"))
# gtsave(table_a2_gt, filename = file.path(Tables_dir, "Table_A2_political_stats.png"))
# gtsave(table_b_gt,  filename = file.path(Tables_dir, "Table_B_panel_demographics.png"))
# gtsave(table_c1_gt, filename = file.path(Tables_dir, "Table_C1_life_satisfaction_trends.png"))
# gtsave(table_c2_gt, filename = file.path(Tables_dir, "Table_C2_political_trends.png"))
# 
# 
# # --- LaTeX tables ---
# cat("Saving LaTeX...\n")
# gtsave(table_a1_gt, filename = file.path(Tables_dir, "Table_A1_life_satisfaction_stats.tex"))
# gtsave(table_a2_gt, filename = file.path(Tables_dir, "Table_A2_political_stats.tex"))
# gtsave(table_b_gt,  filename = file.path(Tables_dir, "Table_B_panel_demographics.tex"))
# gtsave(table_c1_gt, filename = file.path(Tables_dir, "Table_C1_life_satisfaction_trends.tex"))
# gtsave(table_c2_gt, filename = file.path(Tables_dir, "Table_C2_political_trends.tex"))
# 
# # --- CSV data ---
# cat("Saving CSVs...\n")
# write.csv(table_a1_data, file.path(Tables_dir, "Table_A1_data.csv"), row.names = FALSE)
# write.csv(table_a2_data, file.path(Tables_dir, "Table_A2_data.csv"), row.names = FALSE)
# write.csv(table_b_data, file.path(Tables_dir, "Table_B_data.csv"), row.names = FALSE)
# write.csv(table_c1_data, file.path(Tables_dir, "Table_C1_data.csv"), row.names = FALSE)
# write.csv(table_c2_data, file.path(Tables_dir, "Table_C2_data.csv"), row.names = FALSE)
# 
# 
# cat("\n\u2713 All tables saved to:", Tables_dir, "\n")



# ============================================================
# Test-retest share: within-election vs quasi-yearly volatility (UK)
# ============================================================

el <- bes_elections_final %>% filter(!is.na(abs_d_lr), election_weight > 0)
yr <- bes_yearly_final    %>% filter(!is.na(abs_d_lr), norm_weight > 0)

# Days between surveys: checks the "a few weeks apart" claim
summary(el$days_gap)
summary(yr$days_gap)

# All respondents
share_all <- mean(el$abs_d_lr) / mean(yr$abs_d_lr)

# Same respondents in both samples
both <- intersect(el$pid, yr$pid)
length(both)
share_same <- mean(el$abs_d_lr[el$pid %in% both]) /
  mean(yr$abs_d_lr[yr$pid %in% both])

round(c(within_election = mean(el$abs_d_lr),
        quasi_yearly    = mean(yr$abs_d_lr),
        share_all       = share_all,
        share_same      = share_same), 3)


# ============================================================
# Appendix table: within-election vs quasi-yearly volatility, by election cycle
# ============================================================

# Assign each quasi-yearly observation to an election cycle (1-5).
# CHECK this mapping against your election coding before use.
year_to_cycle <- function(y) case_when(
  y <= 2015 ~ 1,
  y == 2016 ~ 2,
  y %in% 2017:2018 ~ 3,
  y %in% 2019:2021 ~ 4,
  y >= 2022 ~ 5
)

table(yr$year)                     # check which years exist before mapping
yr <- yr %>% mutate(election = year_to_cycle(year))
table(yr$year, yr$election)        # check the mapping

within_by_cycle <- el %>%
  filter(pid %in% both) %>%
  group_by(election) %>%
  summarise(within_election = mean(abs_d_lr),
            gap_within      = median(days_gap),
            n_within        = n(), .groups = "drop")

yearly_by_cycle <- yr %>%
  filter(pid %in% both) %>%
  group_by(election) %>%
  summarise(quasi_yearly = mean(abs_d_lr),
            gap_yearly   = median(days_gap),
            n_yearly     = n(), .groups = "drop")

ratio_table <- within_by_cycle %>%
  left_join(yearly_by_cycle, by = "election") %>%
  mutate(ratio = within_election / quasi_yearly)

print(ratio_table)

# Table G: within-election vs quasi-yearly volatility, by election cycle
table_g_data <- ratio_table %>%
  arrange(election) %>%
  transmute(
    Cycle             = c("2015 GE", "2016 EU ref.", "2017 GE", "2019 GE", "2024 GE"),
    `Within-election` = sprintf("%.2f", within_election),
    `Gap (days)`      = sprintf("%.0f", gap_within),
    `Quasi-yearly`    = sprintf("%.2f", quasi_yearly),
    `Gap (days) `     = sprintf("%.0f", gap_yearly),
    Ratio             = sprintf("%.2f", ratio)
  )
write_tex_body(table_g_data, file.path(Tables_dir, "Table_G_body.tex"))

