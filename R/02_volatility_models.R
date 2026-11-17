# ============================================================================
# 02 - Within-person volatility models (Models 1-8)
#
# V-LINE replication package. Extracted verbatim from
# model_volatilities_091126.R lines 485-1319 (9 Nov 2026 version).
# Paths and libraries come from R/00_config.R; source via run_all.R.
# ============================================================================

# ============================================================
# GENERALISED ANALYSIS FUNCTIONS
# ============================================================

analyze_within_variance <- function(data,
                                    sat_var,
                                    weight_var = "norm_weight",
                                    time_var = "year",
                                    control_vars = NULL,
                                    country_name = NULL) {
  
  
  require(srvyr)
  require(survey)
  require(dplyr)
  require(zoo)
  require(emmeans)
  
  abs_diff_name <- paste0("abs_d_", sat_var)
  
  # Build list of all variables needed for the model
  vars_needed <- c(abs_diff_name, weight_var, time_var)
  
  if (!is.null(control_vars) && length(control_vars) > 0) {
    # Extract actual variable names from expressions like "factor(panel_tenure)"
    extracted_vars <- sapply(control_vars, function(var) {
      gsub("^.*\\((.*)\\).*$", "\\1", var)
    })
    vars_needed <- c(vars_needed, extracted_vars)
  }
  
  # Filter to complete cases for ALL variables in the model
  data_complete <- data %>%
    filter(if_all(all_of(vars_needed), ~ !is.na(.))) %>%
    filter(!!sym(weight_var) > 0)
  
  # Create survey design from complete data
  design <- data_complete %>%
    as_survey_design(weights = !!sym(weight_var))
  
  # Build regression formula with time fixed effects
  if (!is.null(control_vars) && length(control_vars) > 0) {
    formula_str <- paste0(abs_diff_name, " ~ factor(", time_var, ") + ", 
                          paste(control_vars, collapse = " + "))
    cat("  Fitting model with controls:", paste(control_vars, collapse = ", "), "\n")
  } else {
    formula_str <- paste0(abs_diff_name, " ~ factor(", time_var, ")")
    cat("  Fitting model without controls\n")
  }
  
  model_formula <- as.formula(formula_str)
  
  # Fit model
  model <- svyglm(model_formula, design = design, na.action = na.exclude)
  
  print(summary(model))
  
  # Get estimated marginal means by time period
  emm_formula <- as.formula(paste("~", time_var))
  emm <- emmeans(model, emm_formula, data = data_complete, rg.limit = 10000000)
  
  # Convert to dataframe
  emm_df <- as.data.frame(emm)
  
  # Create results dataframe with standardised column name
  within_stats <- emm_df %>%
    rename(mean_within_var = emmean,
           time_period = !!sym(time_var)) %>%
    select(time_period, mean_within_var)
  
  # Add sample sizes by time period
  sample_sizes <- data_complete %>%
    filter(!is.na(!!sym(abs_diff_name))) %>%
    group_by(!!sym(time_var)) %>%
    summarise(n_obs = n()) %>%
    ungroup() %>%
    rename(time_period = !!sym(time_var))
  
  # Join with consistent numeric type
  within_stats <- within_stats %>%
    mutate(time_period = as.numeric(as.character(time_period))) %>%
    left_join(
      sample_sizes %>% mutate(time_period = as.numeric(as.character(time_period))),
      by = "time_period"
    )
  
  # Add metadata
  within_stats <- within_stats %>%
    arrange(time_period) %>%
    mutate(
      variable = sat_var,
      country = country_name,
      time_var_name = time_var,
      controls_used = ifelse(!is.null(control_vars) && length(control_vars) > 0, 
                             paste(control_vars, collapse = ", "), 
                             "none")
    )
  
  # Handle country-specific interpolation (Germany has gaps in some variables)
  if (!is.null(country_name) && country_name == "Germany") {
    within_stats <- within_stats %>%
      mutate(mean_within_var_interpolated = na.approx(mean_within_var, na.rm = FALSE))
  } else {
    within_stats <- within_stats %>%
      mutate(mean_within_var_interpolated = mean_within_var)
  }
  
  # Calculate indexed values (first non-NA value = 100)
  within_stats <- within_stats %>%
    mutate(
      mean_within_var = ifelse(mean_within_var == 0, NA_real_, mean_within_var),
      mean_within_var_interpolated = ifelse(mean_within_var_interpolated == 0, 
                                            NA_real_, mean_within_var_interpolated),
      first_value = first(na.omit(mean_within_var)),
      indexed_within_var = (mean_within_var / first_value) * 100,
      indexed_within_var_interpolated = (mean_within_var_interpolated / first_value) * 100
    ) %>%
    select(-first_value)
  
  return(within_stats)
}


process_country_analysis <- function(processed_data,
                                     sat_vars,
                                     country_name,
                                     weight_var = "norm_weight",
                                     time_var = "year",
                                     control_vars = NULL) {
  
  results_list <- list()
  
  cat("\n========== Analyzing", country_name, "==========\n")
  cat("Time variable:", time_var, "\n")
  
  if (!is.null(control_vars) && length(control_vars) > 0) {
    cat("Controls:", paste(control_vars, collapse = ", "), "\n")
  } else {
    cat("Controls: none\n")
  }
  
  for (sat_var in sat_vars) {
    cat("\nAnalyzing:", sat_var, "\n")
    
    # Check if the difference variable exists
    abs_diff_name <- paste0("abs_d_", sat_var)
    if (!(abs_diff_name %in% names(processed_data))) {
      cat("  ✗ Skipping - no difference variable found for", sat_var, "\n")
      next
    }
    
    result <- analyze_within_variance(
      data = processed_data,
      sat_var = sat_var,
      weight_var = weight_var,
      time_var = time_var,
      control_vars = control_vars,
      country_name = country_name
    )
    
    if (!is.null(result)) {
      results_list[[sat_var]] <- result
      cat("  ✓ Success -", nrow(result), "periods analyzed\n")
    } else {
      cat("  ✗ Failed - analysis error\n")
    }
  }
  
  # Combine all results
  if (length(results_list) > 0) {
    combined_results <- bind_rows(results_list)
    return(combined_results)
  } else {
    return(NULL)
  }
}


process_model_results <- function(dataset_specs,
                                  sat_vars,
                                  variable_labels = NULL,
                                  control_vars = NULL,
                                  model_name = "Model") {
  
  
  # dataset_specs: a list of lists, each containing:
  #   - data:         the processed dataset (required)
  #   - country_name: string identifier (required)
  #   - weight_var:   weight variable name (default: "norm_weight")
  #   - time_var:     time variable name (default: "year")
  
  cat("\n========================================\n")
  cat("Processing:", model_name, "\n")
  cat("Datasets:", length(dataset_specs), "\n")
  cat("Variables:", paste(sat_vars, collapse = ", "), "\n")
  cat("========================================\n")
  
  all_results <- list()
  
  for (i in seq_along(dataset_specs)) {
    spec <- dataset_specs[[i]]
    
    # Extract parameters with defaults
    data <- spec$data
    country_name <- spec$country_name
    weight_var <- if (!is.null(spec$weight_var)) spec$weight_var else "norm_weight"
    time_var <- if (!is.null(spec$time_var)) spec$time_var else "year"
    
    cat("\n--- Processing", country_name, "(", i, "of", length(dataset_specs), ") ---\n")
    
    result <- process_country_analysis(
      processed_data = data,
      sat_vars = sat_vars,
      country_name = country_name,
      weight_var = weight_var,
      time_var = time_var,
      control_vars = control_vars
    )
    
    if (!is.null(result) && nrow(result) > 0) {
      # Add derived columns
      result <- result %>%
        filter(n_obs > 0) %>%
        group_by(country, variable) %>%
        mutate(
          min_time = min(time_period),
          periods_since_start = time_period - min_time
        ) %>%
        ungroup()
      
      # Add variable labels if provided
      if (!is.null(variable_labels)) {
        result <- result %>%
          mutate(variable_label = factor(variable_labels[variable], levels = variable_labels))
      }
      
      all_results[[country_name]] <- result
    }
  }
  
  # Combine all results
  if (length(all_results) > 0) {
    combined_results <- bind_rows(all_results)
    cat("\n✓ Processing complete for", model_name, "\n")
    cat("Total rows:", nrow(combined_results), "\n")
    return(combined_results)
  } else {
    cat("\n✗ No results generated for", model_name, "\n")
    return(NULL)
  }
}





# ============================================================
# ============================================================
# Main Section
# 2a: SATISFACTION VOLATILITY
# ============================================================
# ============================================================


# Life satisfaction variables
lsat_vars_soep <- c("lfsato")
lsat_vars_hilda <- c("lfsato")
lsat_vars_shp <- c("sat_life")

# Political variables
pol_vars_shp <- c("lrself")
pol_vars_bes <- c("lr")

# Variable labels
lsat_labels <- c(
  "lfsato" = "Life Satisfaction",
  "sat_life" = "Life Satisfaction"
)

pol_labels <- c(
  "lrself" = "Left-Right (SHP)",
  "lr" = "Left-Right (BES)"
)

# Standard controls (GSOEP, HILDA, SHP)
demo_controls_standard <- c("agecat", "sex", "migback_collapsed")
demo_controls_bes <- c("agecat", "female")
demo_controls_shp <- c("agecat", "sex")

# Model specifications
model_specs <- list(
  model_1 = list(name = "No Controls", controls = NULL),
  model_2 = list(name = "Demographic Controls", controls = demo_controls_standard),
  model_3 = list(name = "Linear Panel Tenure", controls = c("panel_tenure")),
  model_4 = list(name = "Panel Tenure (Factor)", controls = c("factor(panel_tenure)")),
  model_5 = list(name = "Panel Tenure (Factor) + Demographics", 
                 controls = c("factor(panel_tenure)", demo_controls_standard)),
  model_6 = list(name = "New Respondents Only", controls = NULL, filter = "new_respondents"),
  model_7 = list(name = "Balanced Panel", controls = NULL, filter = "balanced")
)

# SHP specifications
model_specs_shp <- list(
  model_1 = list(name = "No Controls", controls = NULL),
  model_2 = list(name = "Demographic Controls", controls = demo_controls_shp),
  model_3 = list(name = "Linear Panel Tenure", controls = c("panel_tenure")),
  model_4 = list(name = "Panel Tenure (Factor)", controls = c("factor(panel_tenure)")),
  model_5 = list(name = "Panel Tenure (Factor) + Demographics", 
                 controls = c("factor(panel_tenure)", demo_controls_shp)),
  model_6 = list(name = "New Respondents Only", controls = NULL, filter = "new_respondents"),
  model_7 = list(name = "Balanced Panel", controls = NULL, filter = "balanced")
)


# BES model specifications (different control variables)
# Model 1 has no controls, so it matches the raw-data table (Table A2).
model_specs_bes <- list(
  model_1 = list(name = "No Controls", controls = NULL),
  model_2 = list(name = "Demographic Controls",
                 controls = c("factor(wave_pos)", demo_controls_bes)),
  model_3 = list(name = "Linear Panel Tenure",
                 controls = c("factor(wave_pos)", "panel_tenure")),
  model_4 = list(name = "Panel Tenure (Factor)",
                 controls = c("factor(wave_pos)", "factor(panel_tenure)")),
  model_5 = list(name = "Panel Tenure (Factor) + Demographics + Days until next survey",
                 controls = c("factor(wave_pos)", "factor(panel_tenure)", "days_gap", demo_controls_bes)),
  model_6 = list(name = "New Respondents Only",
                 controls = c("factor(wave_pos)"), filter = "new_respondents"),
  model_7 = list(name = "Balanced Panel",
                 controls = c("factor(wave_pos)"), filter = "balanced_manual"),
  model_8 = list(name = "Model 5 without days control",
                 controls = c("factor(wave_pos)", "factor(panel_tenure)", demo_controls_bes)),
  R1      = list(name = "Election position only",
                 controls = c("factor(wave_pos)"))
)

run_all_models <- function(data_final,
                           sat_vars,
                           country_name,
                           variable_labels,
                           model_specs,
                           weight_var = "norm_weight",
                           time_var = "year",
                           id_var = "pid",
                           tenure_var = "panel_tenure") {
  
  all_results <- list()
  
  for (model_id in names(model_specs)) {
    spec <- model_specs[[model_id]]
    
    cat("\n========================================\n")
    cat("Running", model_id, ":", spec$name, "\n")
    cat("========================================\n")
    
    # # Prepare data based on filter
    # if (!is.null(spec$filter)) {
    #   if (spec$filter == "new_respondents") {
    #     data_use <- data_final %>% filter(!!sym(tenure_var) <= 1)
    #     cat("Filtering to new respondents (", tenure_var, "<= 1)\n")
    #   } else if (spec$filter == "balanced") {
    #     data_use <- makeBalancedPanel(data_final, idname = id_var, tname = time_var)
    #     cat("Creating balanced panel\n")
    #   } else if (spec$filter == "balanced_manual") {
    #     long_stayer_ids <- data_final %>%
    #       filter(!!sym(tenure_var) >= 27) %>%
    #       pull(!!sym(id_var)) %>%
    #       unique()
    #     data_use <- data_final %>% filter(!!sym(id_var) %in% long_stayer_ids)
    #     cat("Filtering to long-stayers (", tenure_var, ">= 27):", length(long_stayer_ids), "individuals\n")
    #   }
    # } else {
    #   data_use <- data_final
    # }
    
    # Prepare data based on filter
    if (!is.null(spec$filter)) {
      if (spec$filter == "new_respondents") {
        data_use <- data_final %>% filter(!!sym(tenure_var) <= 1)
        cat("Filtering to new respondents (", tenure_var, "<= 1)\n")
      } else if (spec$filter == "balanced") {
        data_use <- makeBalancedPanel(data_final, idname = id_var, tname = time_var)
        cat("Creating balanced panel\n")
      } else if (spec$filter == "balanced_manual") {
        # For BES: individuals with >= 2 valid vol observations in EACH election
        
        # Construct the volatility variable name
        vol_var <- paste0("abs_d_", sat_vars[1])
        
        # Get all unique elections/time periods
        all_periods <- unique(data_final[[time_var]])
        n_periods_total <- length(all_periods)
        
        # Count valid volatility observations per person per time period
        vol_counts_per_period <- data_final %>%
          filter(!is.na(!!sym(vol_var))) %>%
          group_by(!!sym(id_var), !!sym(time_var)) %>%
          summarise(
            n_vol_obs = n(),
            .groups = "drop"
          )
        
        # For each person, count how many periods they have >= 2 valid vol observations
        balanced_check <- vol_counts_per_period %>%
          filter(n_vol_obs >= 2) %>%
          group_by(!!sym(id_var)) %>%
          summarise(
            n_periods_with_2_vol = n(),
            .groups = "drop"
          )
        
        # Get IDs of individuals with >= 2 vol obs in ALL periods
        balanced_ids <- balanced_check %>%
          filter(n_periods_with_2_vol == n_periods_total) %>%
          pull(!!sym(id_var))
        
        data_use <- data_final %>% filter(!!sym(id_var) %in% balanced_ids)
        
        cat("Balanced panel (>=2", vol_var, "in each of", n_periods_total, "elections):",
            length(balanced_ids), "individuals,", nrow(data_use), "obs\n")
      }
    } else {
      data_use <- data_final
    }
    
    # Run analysis
    result <- process_country_analysis(
      processed_data = data_use,
      sat_vars = sat_vars,
      country_name = country_name,
      weight_var = weight_var,
      time_var = time_var,
      control_vars = spec$controls
    )
    
    if (!is.null(result) && nrow(result) > 0) {
      result <- result %>%
        mutate(
          model_id = model_id,
          model_label = spec$name,
          variable_label = variable_labels[variable]
        )
      all_results[[model_id]] <- result
    }
  }
  
  # Combine all models
  if (length(all_results) > 0) {
    combined <- bind_rows(all_results)
    return(combined)
  } else {
    return(NULL)
  }
}



# ============================================================
# A1: GERMANY (GSOEP) - (ALL 7 MODELS)
# ============================================================

cat("\n>>> GERMANY (GSOEP) <<<\n")

results_ger_lsat <- run_all_models(
  data_final = ger_final,
  sat_vars = lsat_vars_soep,
  country_name = "Germany",
  variable_labels = lsat_labels,
  model_specs = model_specs,
  weight_var = "norm_weight",
  time_var = "year",
  id_var = "pid",
  tenure_var = "panel_tenure"
)


# ============================================================
# A2: AUSTRALIA (HILDA) - ALL 7 MODELS
# ============================================================

cat("\n>>> AUSTRALIA (HILDA) <<<\n")

results_aus_lsat <- run_all_models(
  data_final = aus_final,
  sat_vars = lsat_vars_hilda,
  country_name = "Australia",
  variable_labels = lsat_labels,
  model_specs = model_specs,
  weight_var = "norm_weight",
  time_var = "year",
  id_var = "pid",
  tenure_var = "panel_tenure"
)

# ============================================================
# A3: SWITZERLAND (SHP) - (ALL 7 MODELS)
# ============================================================

cat("\n>>> SWITZERLAND (SHP) <<<\n")

results_shp_lsat <- run_all_models(
  data_final = shp_final,
  sat_vars = lsat_vars_shp,
  country_name = "Switzerland",
  variable_labels = lsat_labels,
  model_specs = model_specs_shp,
  weight_var = "norm_weight",
  time_var = "year",
  id_var = "pid",
  tenure_var = "panel_tenure"
)


# ============================================================
# A4: Germany/Switzerland, COVID Models
# All time periods (not capped at 2019)
# ============================================================

cat("\n>>> GERMANY (GSOEP) - COVID Era (Preferred Specification) <<<\n")

results_ger_covid_lsat <- process_country_analysis(
  processed_data = ger_covid,
  sat_vars = "lfsato",
  country_name = "Germany (COVID Era)",
  weight_var = "norm_weight",
  time_var = "year",
  control_vars = c("factor(panel_tenure)", demo_controls_standard)
) %>%
  mutate(
    model_id = "model_5",
    model_label = "Panel Tenure (Factor) + Demographics",
    variable_label = lsat_labels[variable]
  ) %>%
  filter(time_period %in% c(2018, 2021)) %>%
  mutate(
    base_within_var = mean_within_var_interpolated[time_period == 2018],
    indexed_within_var = (mean_within_var_interpolated / base_within_var) * 100,
    indexed_within_var_interpolated = (mean_within_var_interpolated / base_within_var) * 100
  )

cat("\n>>> SWITZERLAND (SHP) - COVID Era (Preferred Specification) <<<\n")

results_shp_covid_lsat <- process_country_analysis(
  processed_data = shp_covid,
  sat_vars = "sat_life",
  country_name = "Switzerland (COVID Era)",
  weight_var = "norm_weight",
  time_var = "year",
  control_vars = c("factor(panel_tenure)", demo_controls_standard)
) %>%
  mutate(
    model_id = "model_5",
    model_label = "Panel Tenure (Factor) + Demographics",
    variable_label = lsat_labels[variable]
  ) %>%
  filter(time_period %in% c(2018, 2021)) %>%
  mutate(
    base_within_var = mean_within_var_interpolated[time_period == 2018],
    indexed_within_var = (mean_within_var_interpolated / base_within_var) * 100,
    indexed_within_var_interpolated = (mean_within_var_interpolated / base_within_var) * 100
  )


# ============================================================
# COMBINE ALL LIFE SATISFACTION RESULTS AND SAVE
# ============================================================

all_lsat_results <- bind_rows(
  results_ger_lsat,
  results_aus_lsat,
  results_shp_lsat
) %>%
  mutate(domain = "Life Satisfaction")

cat("\n✓ Life Satisfaction analysis complete\n")
cat("Total rows:", nrow(all_lsat_results), "\n")

column_order <- c("domain", "country", "model_id", "model_label", 
                  "variable", "variable_label", "time_period", "time_var_name",
                  "mean_within_var", "mean_within_var_interpolated",
                  "indexed_within_var", "indexed_within_var_interpolated",
                  "n_obs", "controls_used")

all_lsat_results <- all_lsat_results %>%
  select(any_of(column_order), everything())


write_csv(all_lsat_results, file.path(modeloutput_path, "volatility_life_satisfaction.csv"))







# ============================================================
# ============================================================
# Main Section
# 2b) POLITICAL ATTITUDES ANALYSIS
# ============================================================
# ============================================================


# ============================================================
# B1: SWITZERLAND (SHP) - (ALL 7 MODELS)
# ============================================================

cat("\n>>> SWITZERLAND (SHP) - Left-Right <<<\n")

results_shp_pol <- run_all_models(
  data_final = shp_politics,
  sat_vars = pol_vars_shp,
  country_name = "Switzerland (SHP)",
  variable_labels = pol_labels,
  model_specs = model_specs_shp,
  weight_var = "norm_weight",
  time_var = "year",
  id_var = "pid",
  tenure_var = "panel_tenure"
)



# ============================================================
# B3: UK (BES) - By Election Cycle (ALL 8 MODELS)
# ============================================================

cat("\n>>> UK (BES) - By Election <<<\n")


results_bes_election <- run_all_models(
  data_final = bes_elections_final,
  sat_vars = pol_vars_bes,
  country_name = "UK (BES Elections)",
  variable_labels = pol_labels,
  model_specs = model_specs_bes,
  weight_var = "election_weight",
  time_var = "election",
  id_var = "pid",
  tenure_var = "panel_tenure"
)


# ============================================================
# B4: UK (BES) - Pre-Election Only (MODEL 5 ONLY - Robustness)
# ============================================================

cat("\n>>> UK (BES) - Pre-Election Only (Preferred Specification) <<<\n")

bes_elections_pre <- bes_elections_final %>%
  filter(wave_pos == 1)

results_bes_election_pre <- process_country_analysis(
  processed_data = bes_elections_pre,
  sat_vars = pol_vars_bes,
  country_name = "UK (BES Elections, Pre-Election)",
  weight_var = "election_weight",
  time_var = "election",
  control_vars = c("factor(panel_tenure)", "days_gap", demo_controls_bes)
) %>%
  mutate(
    model_id = "model_5",
    model_label = "Panel Tenure (Factor) + Demographics + Days (Pre-Election)",
    variable_label = pol_labels[variable]
  )


# ============================================================
# B5: UK (BES) - Election Swing (pre-to-post) (MODEL 5 ONLY - Robustness)
# ============================================================

cat("\n>>> UK (BES) - Election Swing (Preferred Specification) <<<\n")

bes_elections_election_swing <- bes_elections_final %>%
  filter(wave_pos != 1)

results_bes_election_swing <- process_country_analysis(
  processed_data = bes_elections_election_swing,
  sat_vars = pol_vars_bes,
  country_name = "UK (BES Elections, Election Swing)",
  weight_var = "election_weight",
  time_var = "election",
  control_vars = c("factor(panel_tenure)", "days_gap", demo_controls_bes)
) %>%
  mutate(
    model_id = "model_5",
    model_label = "Panel Tenure (Factor) + Demographics + Days (Election Swing)",
    variable_label = pol_labels[variable]
  )

# ============================================================
# B6: UK (BES) - Yearly (MODEL 5 ONLY - Robustness)
# ============================================================

cat("\n>>> UK (BES) - Yearly (Preferred Specification) <<<\n")

bes_yearly_final <- bes_yearly %>%
  mutate(
    year = as.numeric(year),
    lr_count = as.numeric(lr_count)
  ) %>%
  rename(pid = id)

results_bes_yearly <- process_country_analysis(
  processed_data = bes_yearly_final,
  sat_vars = pol_vars_bes,
  country_name = "UK (BES Yearly)",
  weight_var = "norm_weight",
  time_var = "year",
  control_vars = c("factor(panel_tenure)", "days_gap", demo_controls_bes)
) %>%
  mutate(
    model_id = "model_8",
    model_label = "Demographics + Tenure + Days Between Surveys",
    variable_label = pol_labels[variable]
  )

# ============================================================
# COMBINE ALL POLITICAL RESULTS
# ============================================================

all_pol_results <- bind_rows(
  results_shp_pol,
  results_bes_election,
  results_bes_election_pre,
  results_bes_election_swing,
  results_bes_yearly
) %>%
  mutate(domain = "Political")

cat("\n✓ Political analysis complete\n")
cat("Total rows:", nrow(all_pol_results), "\n")


column_order <- c("domain", "country", "model_id", "model_label", 
                  "variable", "variable_label", "time_period", "time_var_name",
                  "mean_within_var", "mean_within_var_interpolated",
                  "indexed_within_var", "indexed_within_var_interpolated",
                  "n_obs", "controls_used")


all_pol_results <- all_pol_results %>%
  select(any_of(column_order), everything())

write_csv(all_pol_results, file.path(modeloutput_path, "volatility_political.csv"))








# 
# 
# 
# # ============================================================
# # B7: DAYS-GAP COEFFICIENTS — Model 5 + days_gap, all datasets
# # Same svyglm, complete-case filter and controls as Model 5.
# # For BES this reproduces the existing Model 5 exactly.
# # ============================================================
# 
# days_gap_coef <- function(data, outcome, controls, label,
#                           weight_var = "norm_weight", time_var = "year") {
#   
#   abs_var   <- paste0("abs_d_", outcome)
#   ctrl_vars <- unname(vapply(controls, function(v) gsub("^.*\\((.*)\\).*$", "\\1", v), character(1)))
#   
#   d <- data %>%
#     filter(if_all(all_of(c(abs_var, weight_var, time_var, "days_gap", ctrl_vars)), ~ !is.na(.))) %>%
#     filter(!!sym(weight_var) > 0)
#   
#   design <- d %>% as_survey_design(weights = !!sym(weight_var))
#   
#   f <- as.formula(paste0(abs_var, " ~ factor(", time_var, ") + ",
#                          paste(c(controls, "days_gap"), collapse = " + ")))
#   m <- svyglm(f, design = design)
#   
#   b    <- coef(m)[["days_gap"]]
#   se   <- sqrt(diag(vcov(m)))[["days_gap"]]
#   ybar <- weighted.mean(d[[abs_var]], d[[weight_var]])
#   
#   tibble(
#     sample        = label,
#     n             = nrow(d),
#     mean_abs_d    = ybar,
#     gap_p10       = unname(quantile(d$days_gap, 0.10)),
#     gap_median    = median(d$days_gap),
#     gap_p90       = unname(quantile(d$days_gap, 0.90)),
#     coef_per_day  = b,
#     se_per_day    = se,
#     # Indexed: % change in mean volatility per 30 extra days
#     pct_per_30d    = 100 * 30 * b / ybar,
#     pct_per_30d_se = 100 * 30 * se / ybar
#   )
# }
# 
# # Rows with a valid |d| but no days_gap should be ~0; check before trusting the N
# cat("\nValid |d| rows missing days_gap:\n")
# print(c(
#   aus_ls = sum(!is.na(aus_final$abs_d_lfsato)    & is.na(aus_final$days_gap)),
#   shp_ls = sum(!is.na(shp_final$abs_d_sat_life)  & is.na(shp_final$days_gap)),
#   shp_lr = sum(!is.na(shp_politics$abs_d_lrself) & is.na(shp_politics$days_gap))
# ))
# 
# days_gap_results <- bind_rows(
#   days_gap_coef(aus_final, "lfsato",
#                 c("factor(panel_tenure)", demo_controls_standard),
#                 label = "Australia (HILDA), LS"),
#   days_gap_coef(shp_final, "sat_life",
#                 c("factor(panel_tenure)", demo_controls_shp),
#                 label = "Switzerland (SHP), LS"),
#   days_gap_coef(shp_politics, "lrself",
#                 c("factor(panel_tenure)", demo_controls_shp),
#                 label = "Switzerland (SHP), LR"),
#   days_gap_coef(bes_elections_final, "lr",
#                 c("factor(wave_pos)", "factor(panel_tenure)", demo_controls_bes),
#                 label = "UK (BES), within-election LR",
#                 weight_var = "election_weight", time_var = "election"),
#   days_gap_coef(bes_yearly_final, "lr",
#                 c("factor(panel_tenure)", demo_controls_bes),
#                 label = "UK (BES), quasi-yearly LR",
#                 weight_var = "norm_weight", time_var = "year")
# )
# 
# print(days_gap_results, width = Inf)
# # write_csv(days_gap_results, file.path(modeloutput_path, "days_gap_coefficients.csv"))
# 
# 






