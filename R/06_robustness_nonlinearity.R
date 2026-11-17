# ============================================================================
# 06 - Robustness: non-linear reporting functions
#
# V-LINE replication package. Extracted verbatim from
# model_volatilities_091126.R lines 3166-3516 (9 Nov 2026 version).
# Paths and libraries come from R/00_config.R; source via run_all.R.
# ============================================================================

# ============================================================
# ============================================================
# Main Section
# 6) Linearity Robustness Check
# How robust is the V-LINE to monotonic Scale Transformations?
# Transformations: Power (concave/convex), Inverse S-curve
# Cost levels: C = 0.00, 0.05, 0.10, 0.20
# ============================================================
# ============================================================


library(tidyverse)
library(srvyr)

# ============================================================
# TRANSFORMATION FUNCTIONS
# ============================================================

transform_power <- function(x, p) {
  10 * (x / 10)^p
}

transform_s_curve <- function(x, k) {
  if (abs(k) < 1e-10) return(x)
  g <- function(t) 1 / (1 + exp(-k * (t - 5)))
  g0 <- g(0)
  g10 <- g(10)
  10 * (g(x) - g0) / (g10 - g0)
}

transform_inv_s_curve <- function(x, k) {
  if (abs(k) < 1e-10) return(x)
  s <- transform_s_curve(x, k)
  s_safe <- pmax(s, 0.001)
  result <- x^2 / s_safe
  result_max <- 100 / transform_s_curve(10, k)
  10 * result / result_max
}

# ============================================================
# KAISER-LEPINTEUR COST FUNCTION (α = 2)
# ============================================================

calculate_cost <- function(transformed_labels) {
  delta <- diff(transformed_labels)
  pop_var <- mean((delta - mean(delta))^2)
  max_var <- 9  # For 11-point scale
  
  sqrt(pop_var / max_var)
}

# ============================================================
# FIND PARAMETERS FOR TARGET COSTS
# ============================================================

find_param <- function(transform_fn, target_cost, search_interval) {
  if (target_cost < 1e-10) return(list(param = 1, achieved_cost = 0))  # Linear case
  objective <- function(param) {
    (calculate_cost(transform_fn(0:10, param)) - target_cost)^2
  }
  result <- optimize(objective, interval = search_interval)
  list(
    param = result$minimum, 
    achieved_cost = calculate_cost(transform_fn(0:10, result$minimum))
  )
}

# Target costs
target_costs <- c(0.00, 0.05, 0.10, 0.20)

# Build parameter table for selected transformations only
param_table <- tibble()

for (tc in target_costs) {
  
  if (tc < 1e-10) {
    # Linear case - add placeholder rows
    param_table <- bind_rows(param_table, tibble(
      target_cost = tc, type = "linear", direction = "none",
      param = 1, achieved_cost = 0))
  } else {
    # Power convex (p > 1)
    res <- find_param(transform_power, tc, c(1.001, 10))
    param_table <- bind_rows(param_table, tibble(
      target_cost = tc, type = "power", direction = "convex",
      param = res$param, achieved_cost = res$achieved_cost))
    
    # Power concave (p < 1)
    res <- find_param(transform_power, tc, c(0.1, 0.999))
    param_table <- bind_rows(param_table, tibble(
      target_cost = tc, type = "power", direction = "concave",
      param = res$param, achieved_cost = res$achieved_cost))
    
    # Inverse S-curve
    res <- find_param(transform_inv_s_curve, tc, c(0.01, 10))
    param_table <- bind_rows(param_table, tibble(
      target_cost = tc, type = "inv_s_curve", direction = "stretch_extremes",
      param = res$param, achieved_cost = res$achieved_cost))
  }
}

cat("\n=== Transformation Parameters ===\n")
print(param_table, n = 20)

# ============================================================
# APPLY TRANSFORMATIONS TO DATA
# ============================================================

apply_transformations <- function(data, ls_var = "lfsato") {
  
  ls_orig <- data[[ls_var]]
  
  # Always include original (linear) as lfsato already exists
  # Add transformed versions
  for (i in 1:nrow(param_table)) {
    row <- param_table[i, ]
    
    if (row$type == "linear") {
      # Linear is just the original - create explicit copy
      var_name <- "ls_linear_c0.00"
      data[[var_name]] <- ls_orig
    } else {
      var_name <- sprintf("ls_%s_%s_c%.2f", row$type, row$direction, row$target_cost)
      
      data[[var_name]] <- switch(row$type,
                                 "power" = transform_power(ls_orig, row$param),
                                 "inv_s_curve" = transform_inv_s_curve(ls_orig, row$param)
      )
    }
  }
  
  return(data)
}

# Apply transformations
cat("\nApplying transformations to Germany...\n")
ger_transformed <- apply_transformations(ger_final)

cat("Applying transformations to Australia...\n")
aus_transformed <- apply_transformations(aus_final)

# Get list of transformed variables
transformed_vars <- c(
  "ls_linear_c0.00",
  "ls_power_convex_c0.05", "ls_power_concave_c0.05", "ls_inv_s_curve_stretch_extremes_c0.05",
  "ls_power_convex_c0.10", "ls_power_concave_c0.10", "ls_inv_s_curve_stretch_extremes_c0.10",
  "ls_power_convex_c0.20", "ls_power_concave_c0.20", "ls_inv_s_curve_stretch_extremes_c0.20"
)

# Model 5 controls
ger_transformed <- create_within_person_differences(
  data = ger_transformed,
  sat_vars = transformed_vars,
  id_var = "pid",
  time_var = "year"
)

aus_transformed <- create_within_person_differences(
  data = aus_transformed,
  sat_vars = transformed_vars,
  id_var = "pid",
  time_var = "year"
)

# ============================================================
# STEP 1: VOLATILITY ANALYSIS FOR ALL TRANSFORMED VARIABLES
# ============================================================

cat("\n========== VOLATILITY ANALYSIS ==========\n")

comparison_controls <- c("panel_tenure", "agecat", "sex", "migback_collapsed")


# Germany
vol_ger_transformed <- process_country_analysis(
  processed_data = ger_transformed,
  sat_vars = transformed_vars,
  country_name = "Germany",
  weight_var = "norm_weight",
  time_var = "year",
  control_vars = comparison_controls
)

# Australia
vol_aus_transformed <- process_country_analysis(
  processed_data = aus_transformed,
  sat_vars = transformed_vars,
  country_name = "Australia",
  weight_var = "norm_weight",
  time_var = "year",
  control_vars = comparison_controls
)

# Combine
all_vol_transformed <- bind_rows(vol_ger_transformed, vol_aus_transformed)

# ============================================================
# STEP 2: V-LINE ANALYSIS FOR ALL TRANSFORMED VARIABLES
# ============================================================

cat("\n========== V-LINE ANALYSIS ==========\n")

vline_results_list <- list()

for (tvar in transformed_vars) {
  
  # Germany
  vline_ger <- run_vline_analysis(
    data = ger_transformed,
    within_results = all_vol_transformed %>% filter(country == "Germany"),
    target_var = tvar,
    country_name = "Germany",
    weight_var = "norm_weight",
    time_var = "year",
    base_year_natural = 1984,
    base_year_alternate = 1985,
    calc_mean = TRUE, 
    groupings = "natural"
  )
  vline_results_list[[paste0("ger_", tvar)]] <- vline_ger
  
  # Australia
  vline_aus <- run_vline_analysis(
    data = aus_transformed,
    within_results = all_vol_transformed %>% filter(country == "Australia"),
    target_var = tvar,
    country_name = "Australia",
    weight_var = "norm_weight",
    time_var = "year",
    base_year_natural = 2001,
    base_year_alternate = 2002,
    calc_mean = TRUE,
    groupings = "natural"
  )
  vline_results_list[[paste0("aus_", tvar)]] <- vline_aus
}

# Combine all V-LINE results
all_vline_transformed <- bind_rows(vline_results_list)

write_csv(all_vline_transformed, file.path(modeloutput_path, "v_line_nonlinear.csv"))




# ============================================================
# ============================================================
# Main Section
# 5b) Linearity Robustness Check — UK (BES, Political Ideology)
# ============================================================
# ============================================================

cat("\nApplying transformations to UK (BES)...\n")
uk_transformed <- apply_transformations(bes_elections_final, ls_var = "lr")

uk_transformed <- create_within_person_differences(
  data = uk_transformed,
  sat_vars = transformed_vars,
  id_var = "pid",
  time_var = "wave"
)

diff_vars     <- paste0("d_", transformed_vars)
abs_diff_vars <- paste0("abs_d_", transformed_vars)

uk_transformed <- uk_transformed %>%
  mutate(across(all_of(c(diff_vars, abs_diff_vars)),
                ~ ifelse(wave_pos == 3, NA_real_, .)))


# ============================================================
# STEP 1: VOLATILITY ANALYSIS FOR ALL TRANSFORMED VARIABLES (UK)
# ============================================================

cat("\n========== VOLATILITY ANALYSIS: UK ==========\n")

# Matches the working BES Model 5 spec exactly (Section 2b) — no
# reason to invent a simplified control set now that the diffing
# is correct: factor(wave_pos) and factor(panel_tenure) are both
# properly identified once differences are computed wave-to-wave.
bes_robustness_controls <- c("factor(wave_pos)", "factor(panel_tenure)", "days_gap", demo_controls_bes)

vol_uk_transformed <- process_country_analysis(
  processed_data = uk_transformed,
  sat_vars = transformed_vars,
  country_name = "UK (BES Elections)",
  weight_var = "election_weight",
  time_var = "election",
  control_vars = bes_robustness_controls
)

# ============================================================
# STEP 2: V-LINE ANALYSIS FOR ALL TRANSFORMED VARIABLES (UK)
# ============================================================

cat("\n========== V-LINE ANALYSIS: UK ==========\n")

vline_results_list_uk <- list()

for (tvar in transformed_vars) {
  vline_uk <- run_vline_analysis_bes_election(
    data = uk_transformed,
    within_results = vol_uk_transformed %>% filter(country == "UK (BES Elections)"),
    target_var = tvar,
    weight_var = "election_weight"
  )
  vline_results_list_uk[[paste0("uk_", tvar)]] <- vline_uk
}

all_vline_transformed_uk <- bind_rows(vline_results_list_uk)

write_csv(all_vline_transformed_uk, file.path(modeloutput_path, "v_line_nonlinear_uk.csv"))

cat("\n✓ UK linearity robustness check complete — saved v_line_nonlinear_uk.csv\n")



# ============================================================
# TRANSFORMATION CURVES (for the plotting script, Section 9c)
# ============================================================

grid <- seq(0, 10, by = 0.05)

transform_curves <- param_table %>%
  filter(type != "linear") %>%
  pmap_dfr(function(target_cost, type, direction, param, achieved_cost) {
    tibble(
      R  = grid,
      fR = switch(type,
                  "power"       = transform_power(grid, param),
                  "inv_s_curve" = transform_inv_s_curve(grid, param)),
      transform_type = case_when(
        type == "inv_s_curve"  ~ "Inverse-S",
        direction == "concave" ~ "Concave",
        TRUE                   ~ "Convex"
      ),
      cost       = target_cost,
      cost_label = sprintf("C = %.2f", target_cost),
      param      = param
    )
  })

write_csv(transform_curves, file.path(modeloutput_path, "nonlinear_transformation_curves.csv"))








