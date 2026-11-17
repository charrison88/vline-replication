# ============================================================================
# 07 - Robustness: sensitivity to Assumption 4
#
# V-LINE replication package. Extracted verbatim from
# model_volatilities_091126.R lines 3517-3729 (9 Nov 2026 version).
# Paths and libraries come from R/00_config.R; source via run_all.R.
# ============================================================================

# ============================================================================
# ============================================================================
# Main Section
# 7) Sensitivity to Psychological Numbing
# ============================================================================
# ============================================================================

# Logic:
# 1. Compute x*, the threshold fractional decline in TRUE volatility that
#    would be needed to fully offset an observed hidden divergence (nominal
#    inequality falling while V-LINE-adjusted inequality rises).
# 2. Fit a sensitivity regression matching the paper's own Model 5/6
#    structure (period fixed effects, panel tenure, demographic controls),
#    but with age and tenure entered as single continuous (smooth) terms
#    rather than factor dummies, so each has one invertible coefficient.
# 3. Rescale every coefficient onto the same "index points" scale used
#    throughout the paper's figures (indexed to 100), so each control's
#    effect size can be compared directly against x* at a glance, and
#    convert into "swings needed" -- how many years, waves, or full
#    0%->100% compositional shifts of that variable would be required to
#    close the gap.

library(tidyverse)

# ============================================================
# STEP 1: x* -- THRESHOLD FRACTIONAL DECLINE IN TRUE VOLATILITY
# ============================================================
# Based on actual endpoint values (first vs. last period), not a fitted
# linear trend, since "hidden divergence" in the paper is defined as an
# observed reversal between two actual periods, not a smoothed slope.

calculate_x_star <- function(vline_data, country_name, grouping_type = "natural") {
  
  country_data <- vline_data %>%
    filter(country == country_name, grouping == grouping_type) %>%
    arrange(period_numeric)
  
  R_hat <- last(country_data$indexed_adjusted_sd) / first(country_data$indexed_adjusted_sd)
  x_star <- (R_hat - 1) / R_hat
  
  cat("\n", country_name, ": R_hat =", round(R_hat, 4), ", x* =", round(x_star * 100, 2), "%\n")
  cat("  Periods:", first(country_data$period_numeric), "to", last(country_data$period_numeric), "\n")
  
  list(
    country = country_name,
    R_hat = R_hat,
    x_star = x_star,
    initial_within_var = first(country_data$within_var_raw)
  )
}

# ============================================================
# STEP 2: SENSITIVITY MODEL -- MODEL 5/6 STRUCTURE, CONTINUOUS AGE & TENURE
# ============================================================

estimate_sensitivity_model <- function(data, outcome_var, period_var, tenure_var,
                                       demographic_vars, weight_var,
                                       extra_controls = NULL, country_name) {
  
  cat("\n========== Sensitivity Model for", country_name, "==========\n")
  
  vars_needed <- c(outcome_var, period_var, tenure_var, demographic_vars, weight_var, extra_controls)
  
  subset_data <- data %>%
    filter(if_all(all_of(vars_needed), ~ !is.na(.))) %>%
    filter(.data[[weight_var]] > 0)
  
  cat("Sample size:", nrow(subset_data), "\n")
  
  rhs_terms <- c(
    paste0("factor(", period_var, ")"),
    tenure_var,
    demographic_vars,
    if (!is.null(extra_controls)) paste0("factor(", extra_controls, ")")
  )
  formula_str <- paste(outcome_var, "~", paste(rhs_terms, collapse = " + "))
  cat("Formula:", formula_str, "\n")
  
  model <- lm(as.formula(formula_str), data = subset_data, weights = subset_data[[weight_var]])
  model_summary <- summary(model)
  print(model_summary)
  
  coef_table <- coef(model_summary)
  
  # Pull out coefficients for tenure + each demographic variable
  # (skip period/wave-position fixed-effect dummies)
  vars_of_interest <- c(tenure_var, demographic_vars)
  
  coefs <- map_dfr(vars_of_interest, function(v) {
    matching_rows <- rownames(coef_table)[startsWith(rownames(coef_table), v)]
    if (length(matching_rows) == 0) {
      warning(paste("No coefficient found for", v))
      return(NULL)
    }
    map_dfr(matching_rows, function(rn) {
      tibble(
        variable = v,
        term = rn,
        estimate = coef_table[rn, "Estimate"],
        se = coef_table[rn, "Std. Error"],
        t_value = coef_table[rn, "t value"],
        p_value = coef_table[rn, "Pr(>|t|)"]
      )
    })
  })
  
  list(
    country = country_name,
    coefs = coefs,
    residual_sd = model_summary$sigma,
    n_obs = nrow(subset_data),
    formula = formula_str
  )
}

# ============================================================
# STEP 3: CONVERT x* AND EACH COEFFICIENT INTO INDEX-POINT TERMS
# ============================================================
# x_star                  : threshold decline needed, in index points
# index_point_coefficient : each control's coefficient, rescaled onto the
#                           same index-point scale, for direct comparison
# swings_needed           : x_star / index_point_coefficient -- how many
#                           years / waves / full compositional swings of
#                           that variable would be needed to close the
#                           gap. Above 1 for a binary variable means even
#                           a complete compositional swing isn't enough.

compute_all_equivalents <- function(x_star_result, sensitivity_result) {
  
  sensitivity_result$coefs %>%
    mutate(
      country = x_star_result$country,
      R_hat = x_star_result$R_hat,
      x_star = x_star_result$x_star * 100,
      initial_within_var = x_star_result$initial_within_var,
      index_point_coefficient = (estimate / initial_within_var) * 100,
      index_point_se = (se / initial_within_var) * 100,
      swings_needed = x_star / index_point_coefficient,
      residual_sd = sensitivity_result$residual_sd,
      n_obs = sensitivity_result$n_obs,
      formula = sensitivity_result$formula
    ) %>%
    select(country, variable, term,
           estimate, se, t_value, p_value,
           index_point_coefficient, index_point_se,
           x_star, swings_needed,
           R_hat, initial_within_var, residual_sd, n_obs, formula)
}

# ============================================================
# MAIN EXECUTION: GERMANY (LIFE SATISFACTION) AND UK (POLITICS)
# ============================================================

all_vline_lsat <- read_csv(file.path(modeloutput_path, "vline_life_satisfaction.csv"), show_col_types = FALSE)
all_vline_pol <- read_csv(file.path(modeloutput_path, "vline_political.csv"), show_col_types = FALSE)

# --- Germany: life satisfaction ---
# Period FE = year; tenure = panel_tenure (smooth); demographics = age, sex, migback_collapsed
x_star_germany <- calculate_x_star(all_vline_lsat, "Germany", grouping_type = "natural")

sens_germany <- estimate_sensitivity_model(
  data = ger_final,
  outcome_var = "abs_d_lfsato",
  period_var = "year",
  tenure_var = "panel_tenure",
  demographic_vars = c("age", "sex", "migback_collapsed"),
  weight_var = "norm_weight",
  country_name = "Germany"
)

equiv_germany <- compute_all_equivalents(x_star_germany, sens_germany)

# --- UK: political ideology (BES) ---
# Period FE = election; extra control = wave_pos (pre/campaign/post, factor);
# tenure = panel_tenure (smooth); demographics = age, female
x_star_uk <- calculate_x_star(all_vline_pol, "UK (BES Elections)", grouping_type = "election_cycle")

sens_uk <- estimate_sensitivity_model(
  data = bes_elections_final,
  outcome_var = "abs_d_lr",
  period_var = "election",
  tenure_var = "panel_tenure",
  demographic_vars = c("age", "female"),
  weight_var = "election_weight",
  extra_controls = "wave_pos",
  country_name = "UK (BES)"
)

equiv_uk <- compute_all_equivalents(x_star_uk, sens_uk)

# ============================================================
# FULL SENSITIVITY TABLE -- EVERYTHING NEEDED FOR THE PARAGRAPH
# ============================================================

sensitivity_summary <- bind_rows(equiv_germany, equiv_uk)

cat("\n")
cat("########################################################\n")
cat("# FULL SENSITIVITY SUMMARY\n")
cat("########################################################\n")

print(sensitivity_summary, width = Inf, n = Inf)



# # ============================================================
# # ============================================================
# # Main Sections:
# # 8) Extra Bits - placebo tests, and raw distributions
# # ============================================================
# # ============================================================


