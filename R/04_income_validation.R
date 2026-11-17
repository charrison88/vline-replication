# ============================================================================
# 04 - Income proof of concept (linearised categories)
#
# V-LINE replication package. Extracted verbatim from
# model_volatilities_091126.R lines 2008-2575 (9 Nov 2026 version).
# Paths and libraries come from R/00_config.R; source via run_all.R.
# ============================================================================

# ============================================================
# ============================================================
# Main Section
# 4) INCOME VOLATILITY VALIDATION EXERCISE
# (The decile exercise has been removed: it validated the same
# logic as the linearised categories but with coarser, unevenly-
# spaced bins, so it added no separate information.)
#   4a) Linearised Income Categories
#   4b) Summary Statistics
#     i)  Mean Absolute Error
#     ii) How non-linear are the income categories?
# ============================================================
# ============================================================

# ============================================================================
# HELPER FUNCTIONS - INCOME-SPECIFIC
# ============================================================================

add_linearised_income_categories <- function(data, income_var, n_categories = 11, time_var = "year") {
  
  cat("\n=== Adding Linearised Income Categories ===\n")
  
  data %>%
    group_by(!!sym(time_var)) %>%
    mutate(
      min_inc = min(!!sym(income_var), na.rm = TRUE),
      max_inc = max(!!sym(income_var), na.rm = TRUE),
      width = (max_inc - min_inc) / n_categories,
      !!paste0(income_var, "_lincat") := pmin(
        floor((!!sym(income_var) - min_inc) / width) + 1,
        n_categories
      )
    ) %>%
    select(-min_inc, -max_inc, -width) %>%
    ungroup()
}

calculate_cross_sectional_stats_income <- function(data, 
                                                   target_var,
                                                   time_var = "year") {
  
  # Create 2-year periods
  min_year <- min(data[[time_var]], na.rm = TRUE)
  
  data <- data %>%
    mutate(
      years_since_start = !!sym(time_var) - min_year,
      time_period = min_year + (years_since_start %/% 2) * 2
    )
  
  # Calculate unweighted mean and SD by 2-year period
  results <- data %>%
    filter(!is.na(!!sym(target_var))) %>%
    group_by(time_period) %>%
    summarise(
      mean_level = mean(!!sym(target_var), na.rm = TRUE),
      sd_level = sd(!!sym(target_var), na.rm = TRUE),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    arrange(time_period)
  
  # Index to first period
  results %>%
    mutate(
      indexed_mean = (mean_level / first(mean_level)) * 100,
      indexed_sd = (sd_level / first(sd_level)) * 100
    )
}

trim_by_income <- function(data, income_var, trim_pct = 3, time_var = "year") {
  
  cat("\n=== Trimming top/bottom", trim_pct, "% by 2-year period ===\n")
  
  # Create 2-year periods
  min_year <- min(data[[time_var]], na.rm = TRUE)
  
  data %>%
    mutate(
      years_since_start = !!sym(time_var) - min_year,
      time_period = min_year + (years_since_start %/% 2) * 2
    ) %>%
    group_by(time_period) %>%
    filter(
      !is.na(!!sym(income_var)),
      !!sym(income_var) >= quantile(!!sym(income_var), trim_pct/100, na.rm = TRUE),
      !!sym(income_var) <= quantile(!!sym(income_var), 1 - trim_pct/100, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(-years_since_start, -time_period)
}



# ============================================================================
# DATA PREPARATION
# ============================================================================

germany_income_data <- germany_cleaned %>%
  filter(psample != 7, !psample %in% c(21, 22)) %>%
  filter(year <= 2019)

australia_income_data <- australia_cleaned %>%
  filter(year <= 2019)

switzerland_income_data <- shp_cleaned%>%
  filter(year <= 2019)


# ============================================================================
# ============================================================================
# Main Section
# 4a) Linearised Income Categories
# ============================================================================
# ============================================================================

# ----------------------------------------------------------------------------
# Step 1: Trim data (2.5% top/bottom)
# ----------------------------------------------------------------------------

ger_trimmed <- trim_by_income(germany_income_data, "hh_income", trim_pct = 2.5)
aus_trimmed <- trim_by_income(australia_income_data, "hh_income", trim_pct = 2.5)
che_trimmed <- trim_by_income(switzerland_income_data, "hh_income", trim_pct = 2.5)

# ----------------------------------------------------------------------------
# Step 2: Add linearised categories
# ----------------------------------------------------------------------------

ger_lincat <- add_linearised_income_categories(ger_trimmed, "hh_income", n_categories = 11)
aus_lincat <- add_linearised_income_categories(aus_trimmed, "hh_income", n_categories = 11)
che_lincat <- add_linearised_income_categories(che_trimmed, "hh_income", n_categories = 11)

# ----------------------------------------------------------------------------
# Step 3: Create within-person differences
# ----------------------------------------------------------------------------

lincat_vars <- c("hh_income", "hh_income_lincat")

ger_lincat <- create_within_person_differences(ger_lincat, lincat_vars)
aus_lincat <- create_within_person_differences(aus_lincat, lincat_vars)
che_lincat <- create_within_person_differences(che_lincat, lincat_vars)

# ----------------------------------------------------------------------------
# Step 4: Volatility analysis (no controls)
# ----------------------------------------------------------------------------

cat("\n========== LINCAT VOLATILITY ANALYSIS ==========\n")

# Germany - lincat volatility
vol_ger_lincat <- analyze_within_variance(
  data = ger_lincat,
  sat_var = "hh_income_lincat",
  weight_var = "norm_weight",
  time_var = "year",
  control_vars = NULL,
  country_name = "Germany"
) %>% mutate(metric = "lincat_volatility")

# Germany - raw income volatility (trimmed)
vol_ger_income_trimmed <- analyze_within_variance(
  data = ger_lincat,
  sat_var = "hh_income",
  weight_var = "norm_weight",
  time_var = "year",
  control_vars = NULL,
  country_name = "Germany"
) %>% mutate(metric = "income_volatility_trimmed")

# Australia - lincat volatility
vol_aus_lincat <- analyze_within_variance(
  data = aus_lincat,
  sat_var = "hh_income_lincat",
  weight_var = "norm_weight",
  time_var = "year",
  control_vars = NULL,
  country_name = "Australia"
) %>% mutate(metric = "lincat_volatility")

# Australia - raw income volatility (trimmed)
vol_aus_income_trimmed <- analyze_within_variance(
  data = aus_lincat,
  sat_var = "hh_income",
  weight_var = "norm_weight",
  time_var = "year",
  control_vars = NULL,
  country_name = "Australia"
) %>% mutate(metric = "income_volatility_trimmed")

# Switzerland - lincat volatility
vol_che_lincat <- analyze_within_variance(
  data = che_lincat,
  sat_var = "hh_income_lincat",
  weight_var = "norm_weight",
  time_var = "year",
  control_vars = NULL,
  country_name = "Switzerland"
) %>% mutate(metric = "lincat_volatility")

# Switzerland - raw income volatility (trimmed)
vol_che_income_trimmed <- analyze_within_variance(
  data = che_lincat,
  sat_var = "hh_income",
  weight_var = "norm_weight",
  time_var = "year",
  control_vars = NULL,
  country_name = "Switzerland"
) %>% mutate(metric = "income_volatility_trimmed")

# ----------------------------------------------------------------------------
# Step 5: Cross-sectional statistics
# ----------------------------------------------------------------------------

cat("\n========== LINCAT CROSS-SECTIONAL STATS ==========\n")

# Germany - lincat levels (what we adjust)
cs_ger_lincat <- calculate_cross_sectional_stats_income(
  ger_lincat, "hh_income_lincat"
) %>% mutate(country = "Germany", variable = "hh_income_lincat", metric = "lincat_stats")

# Germany - income levels (ground truth)
cs_ger_income_trimmed <- calculate_cross_sectional_stats_income(
  ger_lincat, "hh_income"
) %>% mutate(country = "Germany", variable = "hh_income", metric = "income_stats_trimmed")

# Australia - lincat levels (what we adjust)
cs_aus_lincat <- calculate_cross_sectional_stats_income(
  aus_lincat, "hh_income_lincat"
) %>% mutate(country = "Australia", variable = "hh_income_lincat", metric = "lincat_stats")

# Australia - income levels (ground truth)
cs_aus_income_trimmed <- calculate_cross_sectional_stats_income(
  aus_lincat, "hh_income"
) %>% mutate(country = "Australia", variable = "hh_income", metric = "income_stats_trimmed")

# Switzerland - lincat levels (what we adjust)
cs_che_lincat <- calculate_cross_sectional_stats_income(
  che_lincat, "hh_income_lincat"
) %>% mutate(country = "Switzerland", variable = "hh_income_lincat", metric = "lincat_stats")

# Switzerland - income levels (ground truth)
cs_che_income_trimmed <- calculate_cross_sectional_stats_income(
  che_lincat, "hh_income"
) %>% mutate(country = "Switzerland", variable = "hh_income", metric = "income_stats_trimmed")


# Volatility results
lincat_volatility <- bind_rows(
  vol_ger_lincat, vol_ger_income_trimmed,
  vol_aus_lincat, vol_aus_income_trimmed,
  vol_che_lincat, vol_che_income_trimmed
)

# Cross-sectional results
lincat_cross_sectional <- bind_rows(
  cs_ger_lincat, cs_ger_income_trimmed,
  cs_aus_lincat, cs_aus_income_trimmed,
  cs_che_lincat, cs_che_income_trimmed
)


cat("\n========== CALCULATING V-LINE: LINEARISED CATEGORIES ==========\n")

calculate_lincat_vline <- function(vol_data, cs_data, country_name) {
  
  # Lincat volatility
  lincat_vol <- vol_data %>%
    filter(country == country_name, metric == "lincat_volatility") %>%
    select(time_period, indexed_lincat_volatility = indexed_within_var_interpolated)
  
  # Raw income volatility (trimmed)
  income_vol <- vol_data %>%
    filter(country == country_name, metric == "income_volatility_trimmed") %>%
    select(time_period, indexed_income_volatility = indexed_within_var_interpolated)
  
  # Cross-sectional lincat levels (what we adjust)
  lincat_levels <- cs_data %>%
    filter(country == country_name, metric == "lincat_stats") %>%
    select(time_period, 
           indexed_mean_lincat_level = indexed_mean, 
           indexed_sd_lincat_level = indexed_sd)
  
  # Cross-sectional income levels (ground truth)
  income_levels <- cs_data %>%
    filter(country == country_name, metric == "income_stats_trimmed") %>%
    select(time_period, 
           indexed_mean_income_level = indexed_mean, 
           indexed_sd_income_level = indexed_sd)
  
  # Merge and calculate V-LINE adjustments
  result <- lincat_vol %>%
    left_join(income_vol, by = "time_period") %>%
    left_join(lincat_levels, by = "time_period") %>%
    left_join(income_levels, by = "time_period") %>%
    mutate(
      country = country_name,
      stretching_factor = indexed_income_volatility / indexed_lincat_volatility,
      adjusted_mean_lincat = indexed_mean_lincat_level * stretching_factor,
      adjusted_sd_lincat = indexed_sd_lincat_level * stretching_factor
    ) %>%
    select(country, time_period, 
           indexed_lincat_volatility, indexed_income_volatility,
           stretching_factor,
           indexed_mean_lincat_level, indexed_sd_lincat_level,
           adjusted_mean_lincat, adjusted_sd_lincat,
           indexed_mean_income_level, indexed_sd_income_level)
  
  return(result)
}

# Germany
ger_lincat_vline <- calculate_lincat_vline(lincat_volatility, lincat_cross_sectional, "Germany")

# Australia
aus_lincat_vline <- calculate_lincat_vline(lincat_volatility, lincat_cross_sectional, "Australia")

# Switzerland
che_lincat_vline <- calculate_lincat_vline(lincat_volatility, lincat_cross_sectional, "Switzerland")

# Combine and save
income_lincat_vline <- bind_rows(ger_lincat_vline, aus_lincat_vline, che_lincat_vline)

write_csv(income_lincat_vline, file.path(modeloutput_path, "income_lincat_vline.csv"))

cat("✓ Saved income_lincat_vline.csv\n")


# ============================================================
# ============================================================
# Main Section
# 4b) Summary Statistics from Income Exercise
#   i) Mean Absolute Error
# ============================================================
# ============================================================


# ============================================================================
# i) MEAN ABSOLUTE ERROR CALCULATIONS
# ============================================================================

calculate_mae <- function(observed, estimated, variable_name, country_name) {
  
  # Remove any NA values
  valid_idx <- !is.na(observed) & !is.na(estimated)
  obs <- observed[valid_idx]
  est <- estimated[valid_idx]
  
  # Calculate absolute errors
  abs_errors <- abs(obs - est)
  
  # Calculate MAE
  mae <- mean(abs_errors)
  
  # Print results
  cat(sprintf("\n%s - %s:\n", country_name, variable_name))
  cat(sprintf("  MAE: %.2f indexed percentage points\n", mae))
  cat(sprintf("  N observations: %d\n", length(obs)))
  cat(sprintf("  Min error: %.2f\n", min(abs_errors)))
  cat(sprintf("  Max error: %.2f\n", max(abs_errors)))
  
  return(list(
    mae = mae,
    n_obs = length(obs),
    abs_errors = abs_errors,
    variable = variable_name,
    country = country_name
  ))
}

# ============================================================================
# VALIDATION EXERCISE: LINEARISED INCOME CATEGORIES
# ============================================================================

cat("\n")
cat("====================================================\n")
cat("VALIDATION EXERCISE: LINEARISED INCOME CATEGORIES\n")
cat("====================================================\n")

# Filter by country
ger_lincat_vline_data <- income_lincat_vline %>% filter(country == "Germany")
aus_lincat_vline_data <- income_lincat_vline %>% filter(country == "Australia")
che_lincat_vline_data <- income_lincat_vline %>% filter(country == "Switzerland")

# MAE: Adjusted Mean vs Observed Mean Income
cat("\n--- MAE: Adjusted Mean Lincat vs Observed Mean Income ---\n")

ger_mae_lincat_mean <- calculate_mae(
  observed = ger_lincat_vline_data$indexed_mean_income_level,
  estimated = ger_lincat_vline_data$adjusted_mean_lincat,
  variable_name = "Adjusted Mean vs Observed Mean",
  country_name = "Germany (Linearised)"
)

aus_mae_lincat_mean <- calculate_mae(
  observed = aus_lincat_vline_data$indexed_mean_income_level,
  estimated = aus_lincat_vline_data$adjusted_mean_lincat,
  variable_name = "Adjusted Mean vs Observed Mean",
  country_name = "Australia (Linearised)"
)

che_mae_lincat_mean <- calculate_mae(
  observed = che_lincat_vline_data$indexed_mean_income_level,
  estimated = che_lincat_vline_data$adjusted_mean_lincat,
  variable_name = "Adjusted Mean vs Observed Mean",
  country_name = "Switzerland (Linearised)"
)

# MAE: Adjusted SD vs Observed Income SD
cat("\n--- MAE: Adjusted SD Lincat vs Observed Income SD ---\n")

ger_mae_lincat_sd <- calculate_mae(
  observed = ger_lincat_vline_data$indexed_sd_income_level,
  estimated = ger_lincat_vline_data$adjusted_sd_lincat,
  variable_name = "Adjusted SD vs Observed SD",
  country_name = "Germany (Linearised)"
)

aus_mae_lincat_sd <- calculate_mae(
  observed = aus_lincat_vline_data$indexed_sd_income_level,
  estimated = aus_lincat_vline_data$adjusted_sd_lincat,
  variable_name = "Adjusted SD vs Observed SD",
  country_name = "Australia (Linearised)"
)

che_mae_lincat_sd <- calculate_mae(
  observed = che_lincat_vline_data$indexed_sd_income_level,
  estimated = che_lincat_vline_data$adjusted_sd_lincat,
  variable_name = "Adjusted SD vs Observed SD",
  country_name = "Switzerland (Linearised)"
)


# ============================================================================
# MAE: Homoskedastic V-LINE (with Assumption 4) vs Observed Income SD
# ============================================================================

cat("\n--- MAE: Homoskedastic (Assumption 4) SD vs Observed Income SD ---\n")

add_homoskedastic_sd <- function(df) {
  df %>%
    mutate(
      homoskedastic_stretching_factor = 100 / indexed_lincat_volatility,
      homoskedastic_sd_lincat = indexed_sd_lincat_level * homoskedastic_stretching_factor
    )
}

ger_lincat_vline_data <- add_homoskedastic_sd(ger_lincat_vline_data)
aus_lincat_vline_data <- add_homoskedastic_sd(aus_lincat_vline_data)
che_lincat_vline_data <- add_homoskedastic_sd(che_lincat_vline_data)

ger_mae_homosked_sd <- calculate_mae(
  observed = ger_lincat_vline_data$indexed_sd_income_level,
  estimated = ger_lincat_vline_data$homoskedastic_sd_lincat,
  variable_name = "Homoskedastic (Assumption 4) SD vs Observed SD",
  country_name = "Germany (Linearised)"
)

aus_mae_homosked_sd <- calculate_mae(
  observed = aus_lincat_vline_data$indexed_sd_income_level,
  estimated = aus_lincat_vline_data$homoskedastic_sd_lincat,
  variable_name = "Homoskedastic (Assumption 4) SD vs Observed SD",
  country_name = "Australia (Linearised)"
)

che_mae_homosked_sd <- calculate_mae(
  observed = che_lincat_vline_data$indexed_sd_income_level,
  estimated = che_lincat_vline_data$homoskedastic_sd_lincat,
  variable_name = "Homoskedastic (Assumption 4) SD vs Observed SD",
  country_name = "Switzerland (Linearised)"
)


mae_summary <- tibble(
  Country = c("Germany", "Germany", "Germany",
              "Australia", "Australia", "Australia",
              "Switzerland", "Switzerland", "Switzerland"),
  Method = "Linearised",
  Target = rep(c("Mean Income", "Income SD (no Assumption 4)", "Income SD (with Assumption 4)"), 3),
  MAE = c(
    ger_mae_lincat_mean$mae, ger_mae_lincat_sd$mae, ger_mae_homosked_sd$mae,
    aus_mae_lincat_mean$mae, aus_mae_lincat_sd$mae, aus_mae_homosked_sd$mae,
    che_mae_lincat_mean$mae, che_mae_lincat_sd$mae, che_mae_homosked_sd$mae
  )
)

# ============================================================================
# HOW CLOSE ARE THE ORIGINAL (NOMINAL) INCOME CATEGORY VARIABLES, BEFORE ADJUSTMENT?
# ============================================================================
cat("\n--- MAE: Nominal (Unadjusted) SD vs Observed Income SD ---\n")

ger_mae_lincat_sd_nominal <- calculate_mae(
  observed = ger_lincat_vline_data$indexed_sd_income_level,
  estimated = ger_lincat_vline_data$indexed_sd_lincat_level,
  variable_name = "Nominal SD vs Observed SD",
  country_name = "Germany (Linearised, Nominal)"
)
aus_mae_lincat_sd_nominal <- calculate_mae(
  observed = aus_lincat_vline_data$indexed_sd_income_level,
  estimated = aus_lincat_vline_data$indexed_sd_lincat_level,
  variable_name = "Nominal SD vs Observed SD",
  country_name = "Australia (Linearised, Nominal)"
)
che_mae_lincat_sd_nominal <- calculate_mae(
  observed = che_lincat_vline_data$indexed_sd_income_level,
  estimated = che_lincat_vline_data$indexed_sd_lincat_level,
  variable_name = "Nominal SD vs Observed SD",
  country_name = "Switzerland (Linearised, Nominal)"
)

sd_mae_comparison <- tibble(
  Country = c("Germany", "Australia", "Switzerland"),
  Method  = "Linearised",
  MAE_Nominal        = c(ger_mae_lincat_sd_nominal$mae, aus_mae_lincat_sd_nominal$mae, che_mae_lincat_sd_nominal$mae),
  MAE_Adjusted        = c(ger_mae_lincat_sd$mae,          aus_mae_lincat_sd$mae,          che_mae_lincat_sd$mae),
  MAE_Homoskedastic   = c(ger_mae_homosked_sd$mae,        aus_mae_homosked_sd$mae,        che_mae_homosked_sd$mae)
)
print(sd_mae_comparison)











# ============================================================
# ============================================================
# Main Section
# 4b) Summary Statistics from Income Exercise
#   ii) How non-linear are the income categories?
# ============================================================
# ============================================================

measure_linearity_by_year <- function(data, category_var, income_var = "hh_income",
                                      weight_var = "norm_weight", time_var = "year") {
  data %>%
    filter(!is.na(!!sym(category_var)), !is.na(!!sym(income_var)), !is.na(!!sym(weight_var))) %>%
    group_by(!!sym(time_var), !!sym(category_var)) %>%
    summarise(mean_income = weighted.mean(!!sym(income_var), !!sym(weight_var)), .groups = "drop") %>%
    arrange(!!sym(time_var), !!sym(category_var)) %>%
    group_by(!!sym(time_var)) %>%
    summarise(
      K = n(),
      var_delta = var(diff(mean_income)),
      max_var_delta = ((K - 2) / (K - 1)^2) * (last(mean_income) - first(mean_income))^2,
      C_alpha = var_delta / max_var_delta,
      .groups = "drop"
    )
}

ger_lincat_by_year <- measure_linearity_by_year(ger_lincat, "hh_income_lincat")
print(ger_lincat_by_year)
mean(ger_lincat_by_year$C_alpha[is.finite(ger_lincat_by_year$C_alpha)])

aus_lincat_by_year <- measure_linearity_by_year(aus_lincat, "hh_income_lincat")
print(aus_lincat_by_year)
mean(aus_lincat_by_year$C_alpha[is.finite(aus_lincat_by_year$C_alpha)])

che_lincat_by_year <- measure_linearity_by_year(che_lincat, "hh_income_lincat")
print(che_lincat_by_year)
mean(che_lincat_by_year$C_alpha[is.finite(che_lincat_by_year$C_alpha)])





