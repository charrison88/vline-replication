# ============================================================================
# 03 - V-LINE rescaling adjustments
#
# V-LINE replication package. Extracted verbatim from
# model_volatilities_091126.R lines 1320-2007 (9 Nov 2026 version).
# Paths and libraries come from R/00_config.R; source via run_all.R.
# ============================================================================

# ============================================================
# ============================================================
# Main Section
# 3) V-LINE RESCALING ADJUSTMENTS
# ============================================================
# ============================================================

# ============================================================
# HELPER FUNCTIONS
# ============================================================

svysd <- function(formula, design, na.rm = TRUE) {
  v <- survey::svyvar(formula, design, na.rm = na.rm)
  sqrt(as.numeric(v))
}


create_period_grouping <- function(data, 
                                   time_var = "year",
                                   grouping_type = "natural",
                                   base_year_natural = NULL,
                                   base_year_alternate = NULL) {
  
  
  data <- data %>% arrange(!!sym(time_var))
  unique_times <- sort(unique(data[[time_var]]))
  
  
  # Determine base year
  if (is.null(base_year_natural) || is.null(base_year_alternate)) {
    # Default: use first available year for natural, first + 1 for alternate
    base_year <- if (grouping_type == "natural") {
      min(unique_times)
    } else {
      min(unique_times) + 1
    }
  } else {
    base_year <- if (grouping_type == "natural") base_year_natural else base_year_alternate
  }
  
  # Filter to valid times
  valid_times <- unique_times[unique_times >= base_year]
  
  # If odd number, exclude last
  if (length(valid_times) %% 2 == 1) {
    valid_times <- valid_times[-length(valid_times)]
  }
  
  # Create period mapping (2-year periods)
  period_mapping <- data.frame(
    time_val = valid_times,
    period = rep(valid_times[seq(1, length(valid_times), by = 2)], each = 2)
  )
  names(period_mapping)[1] <- time_var
  
  # Join to data
  data <- data %>%
    filter(!!sym(time_var) %in% valid_times) %>%
    left_join(period_mapping, by = time_var)
  
  return(data)
}


# ============================================================
# CORE V-LINE FUNCTIONS
# ============================================================

# Function 1: Calculate cross-sectional statistics by period
calculate_cross_sectional_stats <- function(data, 
                                            target_var,
                                            weight_var = "norm_weight",
                                            calc_mean = TRUE) {
  
  require(survey)
  
  design <- create_survey_design(data, weight_var)
  period_stats <- data.frame()
  
  for (p in unique(data$period)) {
    design_p <- subset(design, period == p)
    
    # Always calculate SD
    sd_val <- svysd(as.formula(paste0("~", target_var)), design = design_p, na.rm = TRUE)
    
    # Optionally calculate mean
    if (calc_mean) {
      mean_val <- svymean(as.formula(paste0("~", target_var)), design = design_p, na.rm = TRUE)
      mean_level <- as.numeric(mean_val)
      mean_se <- as.numeric(SE(mean_val))
    } else {
      mean_level <- NA_real_
      mean_se <- NA_real_
    }
    
    period_stats <- rbind(period_stats, data.frame(
      period = p,
      mean_level = mean_level,
      mean_se = mean_se,
      sd_level = as.numeric(sd_val),
      n_obs = nrow(design_p$variables)
    ))
  }
  
  return(period_stats)
}


# Function 2: Calculate V-LINE adjustments
calculate_vline_adjustments <- function(period_stats,
                                        within_results,
                                        target_var,
                                        country_name,
                                        grouping_type,
                                        time_var = "year",
                                        calc_mean = TRUE) {
  
  
  # Extract within-variation for target variable and country
  within_variation <- within_results %>%
    filter(variable == target_var) %>%
    select(time_period, mean_within_var, indexed_within_var) %>%
    rename(period_numeric = time_period)
  
  # Use interpolated version for Germany
  if (country_name == "Germany" && "mean_within_var_interpolated" %in% names(within_results)) {
    within_variation <- within_results %>%
      filter(variable == target_var) %>%
      select(time_period, mean_within_var_interpolated, indexed_within_var_interpolated) %>%
      rename(
        period_numeric = time_period,
        mean_within_var = mean_within_var_interpolated,
        indexed_within_var = indexed_within_var_interpolated
      )
  }
  
  within_variation <- within_variation %>%
    mutate(
      raw_within = mean_within_var,
      indexed_within = indexed_within_var
    )
  
  # Add period_numeric to period_stats
  period_stats <- period_stats %>%
    mutate(period_numeric = as.numeric(period))
  
  # Merge
  period_stats <- period_stats %>%
    left_join(
      within_variation %>% select(period_numeric, raw_within, indexed_within),
      by = "period_numeric"
    )
  
  # Re-index for alternate grouping
  if (grouping_type == "alternate") {
    first_indexed_within <- first(na.omit(period_stats$indexed_within))
    period_stats <- period_stats %>%
      mutate(indexed_within = (indexed_within / first_indexed_within) * 100)
  }
  
  # Calculate adjusted metrics
  period_stats <- period_stats %>%
    mutate(
      # Adjusted SD (always)
      adjusted_sd = sd_level / (indexed_within / 100),
      
      # Adjusted Mean (if requested)
      adjusted_mean = if (calc_mean) mean_level / (indexed_within / 100) else NA_real_,
      
      # Metadata
      within_var_raw = raw_within,
      country = country_name,
      grouping = grouping_type,
      variable = target_var
    ) %>%
    select(-raw_within)
  
  return(period_stats)
}


# Function 3: Index all V-LINE results
index_vline_results <- function(vline_stats, calc_mean = TRUE) {
  
  vline_stats <- vline_stats %>%
    group_by(country, grouping, variable) %>%
    mutate(
      # Index SD (always)
      indexed_sd = (sd_level / first(na.omit(sd_level))) * 100,
      indexed_adjusted_sd = (adjusted_sd / first(na.omit(adjusted_sd))) * 100,
      
      # Index within-variation
      indexed_within_raw = (within_var_raw / first(na.omit(within_var_raw))) * 100
    )
  
  # Index mean if calculated
  if (calc_mean && !all(is.na(vline_stats$mean_level))) {
    vline_stats <- vline_stats %>%
      mutate(
        indexed_mean = (mean_level / first(na.omit(mean_level))) * 100,
        indexed_adjusted_mean = (adjusted_mean / first(na.omit(adjusted_mean))) * 100
      )
  } else {
    vline_stats <- vline_stats %>%
      mutate(
        indexed_mean = NA_real_,
        indexed_adjusted_mean = NA_real_
      )
  }
  
  vline_stats <- vline_stats %>% ungroup()
  
  return(vline_stats)
}


# ============================================================
# WRAPPER FUNCTION: Run V-LINE for single dataset
# ============================================================

# run_vline_analysis <- function(data,
#                                within_results,
#                                target_var,
#                                country_name,
#                                weight_var = "norm_weight",
#                                time_var = "year",
#                                base_year_natural = NULL,
#                                base_year_alternate = NULL,
#                                calc_mean = TRUE) {
#   
#   
#   cat("\n========== V-LINE Analysis:", country_name, "-", target_var, "==========\n")
#   
#   results_list <- list()
#   
#   for (grouping_type in c("natural", "alternate")) {
#     cat("  Grouping:", grouping_type, "\n")
#     
#     # Step 1: Create period grouping
#     data_with_periods <- create_period_grouping(
#       data = data,
#       time_var = time_var,
#       grouping_type = grouping_type,
#       base_year_natural = base_year_natural,
#       base_year_alternate = base_year_alternate
#     )
#     
#     # Step 2: Calculate cross-sectional stats
#     period_stats <- calculate_cross_sectional_stats(
#       data = data_with_periods,
#       target_var = target_var,
#       weight_var = weight_var,
#       calc_mean = calc_mean
#     )
#     
#     # Step 3: Calculate V-LINE adjustments
#     vline_stats <- calculate_vline_adjustments(
#       period_stats = period_stats,
#       within_results = within_results,
#       target_var = target_var,
#       country_name = country_name,
#       grouping_type = grouping_type,
#       time_var = time_var,
#       calc_mean = calc_mean
#     )
#     
#     results_list[[grouping_type]] <- vline_stats
#   }
#   
#   # Combine both groupings
#   combined <- bind_rows(results_list)
#   
#   # Step 4: Index all metrics
#   indexed_results <- index_vline_results(combined, calc_mean = calc_mean)
#   
#   cat("  ✓ Complete -", nrow(indexed_results), "rows\n")
#   
#   return(indexed_results)
# }


run_vline_analysis <- function(data,
                               within_results,
                               target_var,
                               country_name,
                               weight_var = "norm_weight",
                               time_var = "year",
                               base_year_natural = NULL,
                               base_year_alternate = NULL,
                               calc_mean = TRUE,
                               groupings = c("natural", "alternate")) {
  
  
  cat("\n========== V-LINE Analysis:", country_name, "-", target_var, "==========\n")
  
  results_list <- list()
  
  for (grouping_type in groupings) {
    cat("  Grouping:", grouping_type, "\n")

    # Step 1: Create period grouping
    data_with_periods <- create_period_grouping(
      data = data,
      time_var = time_var,
      grouping_type = grouping_type,
      base_year_natural = base_year_natural,
      base_year_alternate = base_year_alternate
    )

    # Step 2: Calculate cross-sectional stats
    period_stats <- calculate_cross_sectional_stats(
      data = data_with_periods,
      target_var = target_var,
      weight_var = weight_var,
      calc_mean = calc_mean
    )

    # Step 3: Calculate V-LINE adjustments
    vline_stats <- calculate_vline_adjustments(
      period_stats = period_stats,
      within_results = within_results,
      target_var = target_var,
      country_name = country_name,
      grouping_type = grouping_type,
      time_var = time_var,
      calc_mean = calc_mean
    )

    results_list[[grouping_type]] <- vline_stats
  }

  # Combine both groupings
  combined <- bind_rows(results_list)

  # Step 4: Index all metrics
  indexed_results <- index_vline_results(combined, calc_mean = calc_mean)

  cat("  ✓ Complete -", nrow(indexed_results), "rows\n")

  return(indexed_results)
}



    


# ============================================================
# BES ELECTION-SPECIFIC V-LINE FUNCTION
# ============================================================

run_vline_analysis_bes_election <- function(data,
                                            within_results,
                                            target_var = "lr",
                                            weight_var = "election_weight") {
  
  require(survey)
  require(srvyr)
  
  cat("\n========== V-LINE Analysis: BES Elections -", target_var, "==========\n")
  
  # Extract within-variation (from volatility results)
  within_variation <- within_results %>%
    filter(variable == target_var) %>%
    select(time_period, mean_within_var, indexed_within_var) %>%
    rename(
      election = time_period,
      raw_within = mean_within_var,
      indexed_within = indexed_within_var
    )
  
  # Create survey design (pool all waves)
  design <- data %>%
    filter(!is.na(!!sym(weight_var)), !!sym(weight_var) > 0) %>%
    as_survey_design(weights = !!sym(weight_var))
  
  # Calculate SD and mean by election (pooling across waves)
  sd_by_election <- data.frame()
  
  for (e in sort(unique(data$election))) {
    design_e <- subset(design, election == e)
    
    sd_val <- svysd(as.formula(paste0("~", target_var)), design = design_e, na.rm = TRUE)
    mean_val <- svymean(as.formula(paste0("~", target_var)), design = design_e, na.rm = TRUE)
    
    sd_by_election <- rbind(sd_by_election, data.frame(
      election = e,
      sd_level = as.numeric(sd_val),
      mean_level = as.numeric(mean_val),
      n_obs = nrow(design_e$variables)
    ))
  }
  
  # Merge with within-variation
  combined <- sd_by_election %>%
    left_join(within_variation, by = "election") %>%
    mutate(
      adjusted_sd = sd_level / (indexed_within / 100),
      within_var_raw = raw_within,
      country = "UK (BES Elections)",
      variable = target_var,
      grouping = "election_cycle"
    )
  
  # Index all metrics
  combined <- combined %>%
    mutate(
      indexed_sd = (sd_level / first(na.omit(sd_level))) * 100,
      indexed_adjusted_sd = (adjusted_sd / first(na.omit(adjusted_sd))) * 100,
      indexed_within_raw = (within_var_raw / first(na.omit(within_var_raw))) * 100,
      indexed_mean = (mean_level / first(na.omit(mean_level))) * 100,
      adjusted_mean = NA_real_,
      indexed_adjusted_mean = NA_real_,
      period = election,
      period_numeric = as.numeric(election)
    )
  
  cat("  ✓ Complete -", nrow(combined), "election cycles\n")
  
  return(combined)
}


# ============================================================
# ============================================================
# APPLY V-LINE ANALYSIS TO ALL DATASETS
# ============================================================
# ============================================================

# ============================================================
# DEFINITIONS
# ============================================================

# Base years by country/variable
base_years <- list(
  Germany = list(lfsato = c(natural = 1984, alternate = 1985)),
  Australia = list(lfsato = c(natural = 2001, alternate = 2002)),
  Switzerland = list(
    sat_life = c(natural = 2000, alternate = 2001),
    lrself = c(natural = 1999, alternate = 2000)
  )
)


# ============================================================
# ============================================================
# Main Section
# 3a) SATISFACTION V-LINE ADJUSTMENTS
# ============================================================
# ============================================================

# Filter within-results to Model 5 only
within_model5_lsat <- all_lsat_results %>%
  filter(model_id == "model_5")


# ============================================================
# A1: GERMANY
# ============================================================

vline_ger_lsat <- run_vline_analysis(
  data = ger_final,
  within_results = within_model5_lsat %>% filter(country == "Germany"),
  target_var = "lfsato",
  country_name = "Germany",
  weight_var = "norm_weight",
  time_var = "year",
  base_year_natural = 1984,
  base_year_alternate = 1985,
  calc_mean = TRUE
)

# ============================================================
# A2: AUSTRALIA
# ============================================================

vline_aus_lsat <- run_vline_analysis(
  data = aus_final,
  within_results = within_model5_lsat %>% filter(country == "Australia"),
  target_var = "lfsato",
  country_name = "Australia",
  weight_var = "norm_weight",
  time_var = "year",
  base_year_natural = 2001,
  base_year_alternate = 2002,
  calc_mean = TRUE
)

# ============================================================
# A3: SWITZERLAND
# ============================================================

vline_shp_lsat <- run_vline_analysis(
  data = shp_final,
  within_results = within_model5_lsat %>% filter(country == "Switzerland"),
  target_var = "sat_life",
  country_name = "Switzerland",
  weight_var = "norm_weight",
  time_var = "year",
  base_year_natural = 2000,
  base_year_alternate = 2001,
  calc_mean = TRUE
)

# ============================================================
# A4: COVID for Switzerland/Germany
# ============================================================

# Germany COVID
vline_ger_covid <- ger_covid %>%
  filter(year %in% c(2018, 2019, 2021, 2022)) %>%
  mutate(period = ifelse(year %in% c(2018, 2019), 2018, 2021)) %>%
  group_by(period) %>%
  summarise(
    mean_level = weighted.mean(lfsato, norm_weight, na.rm = TRUE),
    sd_level = sqrt(Hmisc::wtd.var(lfsato, norm_weight, na.rm = TRUE)),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  left_join(
    results_ger_covid_lsat %>% select(time_period, indexed_within_var),
    by = c("period" = "time_period")
  ) %>%
  mutate(
    scale_factor = (indexed_within_var / 100),
    adjusted_sd = sd_level / scale_factor,
    adjusted_mean = mean_level / scale_factor,
    indexed_sd = (sd_level / first(sd_level)) * 100,
    indexed_adjusted_sd = (adjusted_sd / first(adjusted_sd)) * 100,
    indexed_mean = (mean_level / first(mean_level)) * 100,
    indexed_adjusted_mean = (adjusted_mean / first(adjusted_mean)) * 100,
    period_numeric = period,
    country = "Germany (COVID Era)",
    variable = "lfsato",
    grouping = "natural"
  )

# Switzerland COVID
vline_shp_covid <- shp_covid %>%
  filter(year %in% c(2018, 2019, 2021, 2022)) %>%
  mutate(period = ifelse(year %in% c(2018, 2019), 2018, 2021)) %>%
  group_by(period) %>%
  summarise(
    mean_level = weighted.mean(sat_life, norm_weight, na.rm = TRUE),
    sd_level = sqrt(Hmisc::wtd.var(sat_life, norm_weight, na.rm = TRUE)),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  left_join(
    results_shp_covid_lsat %>% select(time_period, indexed_within_var),
    by = c("period" = "time_period")
  ) %>%
  mutate(
    scale_factor = (indexed_within_var / 100),
    adjusted_sd = sd_level / scale_factor,
    adjusted_mean = mean_level / scale_factor,
    indexed_sd = (sd_level / first(sd_level)) * 100,
    indexed_adjusted_sd = (adjusted_sd / first(adjusted_sd)) * 100,
    indexed_mean = (mean_level / first(mean_level)) * 100,
    indexed_adjusted_mean = (adjusted_mean / first(adjusted_mean)) * 100,
    period_numeric = period,
    country = "Switzerland (COVID Era)",
    variable = "sat_life",
    grouping = "natural"
  )


# ============================================================
# COMBINE AND SAVE LS V-LINE RESULTS (including COVID)
# ============================================================

all_vline_lsat <- bind_rows(
  vline_ger_lsat,
  vline_aus_lsat,
  vline_shp_lsat,
  vline_ger_covid,
  vline_shp_covid
) %>%
  mutate(domain = "Life Satisfaction")


# Column order for output
vline_column_order <- c(
  "domain", "country", "variable", "grouping",
  "period", "period_numeric", "n_obs",
  # Within-variation
  "within_var_raw", "indexed_within_raw", "indexed_within",
  # SD metrics
  "sd_level", "indexed_sd", "adjusted_sd", "indexed_adjusted_sd",
  # Mean metrics (life satisfaction only)
  "mean_level", "indexed_mean", "adjusted_mean", "indexed_adjusted_mean",
  # SE
  "mean_se"
)

all_vline_lsat <- all_vline_lsat %>%
  select(any_of(vline_column_order), everything())

# Save
write_csv(all_vline_lsat, file.path(modeloutput_path, "vline_life_satisfaction.csv"))





# ============================================================
# ============================================================
# Main Section
# 3b) POLITICAL V-LINE ADJUSTMENTS
# ============================================================
# ============================================================


# Filter within-results to Model 5 only
within_model5_pol <- all_pol_results %>%
  filter(model_id == "model_5")

# ============================================================
# B1: SWITZERLAND (SHP) - Left-Right
# ============================================================

vline_shp_pol <- run_vline_analysis(
  data = shp_politics,
  within_results = within_model5_pol %>% filter(country == "Switzerland (SHP)"),
  target_var = "lrself",
  country_name = "Switzerland (SHP)",
  weight_var = "norm_weight",
  time_var = "year",
  base_year_natural = 1999,
  base_year_alternate = 2000,
  calc_mean = FALSE
)

# ============================================================
# B2: UK (BES) - Election Cycles
# ============================================================

vline_bes_election <- run_vline_analysis_bes_election(
  data = bes_elections_final,
  within_results = within_model5_pol %>% filter(country == "UK (BES Elections)"),
  target_var = "lr",
  weight_var = "election_weight"
)

# ============================================================
# COMBINE POLITICAL V-LINE RESULTS
# ============================================================

all_vline_pol <- bind_rows(
  vline_shp_pol,
  vline_bes_election
) %>%
  mutate(domain = "Political")

cat("\n✓ Political V-LINE complete\n")
cat("Total rows:", nrow(all_vline_pol), "\n")


# ============================================================
# ============================================================
# SAVE V-LINE OUTPUTS
# ============================================================
# ============================================================

# Column order for output
vline_column_order <- c(
  "domain", "country", "variable", "grouping",
  "period", "period_numeric", "n_obs",
  # Within-variation
  "within_var_raw", "indexed_within_raw", "indexed_within",
  # SD metrics
  "sd_level", "indexed_sd", "adjusted_sd", "indexed_adjusted_sd",
  # Mean metrics (life satisfaction only)
  "mean_level", "indexed_mean", "adjusted_mean", "indexed_adjusted_mean",
  # SE
  "mean_se"
)

all_vline_pol <- all_vline_pol %>%
  select(any_of(vline_column_order), everything())

# Save
write_csv(all_vline_pol, file.path(modeloutput_path, "vline_political.csv"))





