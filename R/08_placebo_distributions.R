# ============================================================================
# 08 - Health placebo, response distributions, nominal SD
#
# V-LINE replication package. Extracted verbatim from
# model_volatilities_091126.R lines 3730-3906 (9 Nov 2026 version).
# Paths and libraries come from R/00_config.R; source via run_all.R.
# ============================================================================

# ============================================================
# ============================================================
# 8a) HAPPINESS INPUTS VOLATILITY (placebo tests
# ============================================================
# ============================================================

input_labels <- c(
  "healthstatus" = "Health Status"
)

# ------------------------------------------------------------
# STEP 1: healthstatus — all Model 5
# ------------------------------------------------------------

results_inputs_all <- bind_rows(
  process_country_analysis(
    processed_data = ger_health,
    sat_vars = c("healthstatus"),
    country_name = "Germany",
    weight_var = "norm_weight",
    time_var = "year",
    control_vars = c("factor(panel_tenure)", demo_controls_standard)
  ),
  process_country_analysis(
    processed_data = aus_health,
    sat_vars = c("healthstatus"),
    country_name = "Australia",
    weight_var = "norm_weight",
    time_var = "year",
    control_vars = c("factor(panel_tenure)", demo_controls_standard)
  )
) %>%
  mutate(model_id = "model_5", model_label = "Panel Tenure (Factor) + Demographics")

# ------------------------------------------------------------
# STEP 2: Add variable_label + domain, match column order
# ------------------------------------------------------------

results_inputs <- results_inputs_all %>%
  mutate(
    variable_label = input_labels[variable],
    domain = "Happiness Inputs"
  )

column_order <- c("domain", "country", "model_id", "model_label",
                  "variable", "variable_label", "time_period", "time_var_name",
                  "mean_within_var", "mean_within_var_interpolated",
                  "indexed_within_var", "indexed_within_var_interpolated",
                  "n_obs", "controls_used")

results_inputs <- results_inputs %>%
  select(any_of(column_order), everything())

write_csv(results_inputs, file.path(modeloutput_path, "volatility_inputs.csv"))

cat("\n✓ Saved volatility_inputs.csv (all inputs, Model 5)\n")






# ============================================================
# 7b) Distributions and Nominal Inequality -- DATA
# svysd() already defined in Section 3; not redefined here.
# ============================================================

distribution_specs <- tribble(
  ~country,             ~domain,             ~variable,  ~data_name,             ~weight_var,        ~time_var,   ~tenure_var,
  "Germany",             "Life Satisfaction", "lfsato",   "ger_final",            "norm_weight",      "year",      "panel_tenure",
  "Australia",           "Life Satisfaction", "lfsato",   "aus_final",            "norm_weight",      "year",      "panel_tenure",
  "Switzerland",         "Life Satisfaction", "sat_life", "shp_final",            "norm_weight",      "year",      "panel_tenure",
  "UK (BES Elections)",  "Political",         "lr",       "bes_elections_final",  "election_weight",  "election",  "panel_tenure",
  "Switzerland (SHP)",   "Political",         "lrself",   "shp_politics",            "norm_weight",      "year",      "panel_tenure"
)

nominal_sd_specs <- tribble(
  ~country,             ~domain,             ~variable,  ~data_name,             ~weight_var,        ~time_var,   ~tenure_var,     ~base_year,  ~period_type,
  "Germany",             "Life Satisfaction", "lfsato",   "ger_final",            "norm_weight",      "year",      "panel_tenure",  1984,        "year_2period",
  "Australia",           "Life Satisfaction", "lfsato",   "aus_final",            "norm_weight",      "year",      "panel_tenure",  2001,        "year_2period",
  "Switzerland",         "Life Satisfaction", "sat_life", "shp_final",            "norm_weight",      "year",      "panel_tenure",  2000,        "year_2period",
  "UK (BES Elections)",  "Political",         "lr",       "bes_elections_final",  "election_weight",  "election",  "panel_tenure",  1,           "election",
  "Switzerland (SHP)",   "Political",         "lrself",   "shp_politics",            "norm_weight",      "year",      "panel_tenure",  1999,        "year_2period"
)

distribution_data_lookup <- list(
  ger_final = ger_final, aus_final = aus_final, shp_final = shp_final,
  bes_elections_final = bes_elections_final, shp_politics = shp_politics
)
distribution_data_lookup_new_resp <- list(
  ger_final = ger_final %>% filter(panel_tenure <= 1),
  aus_final = aus_final %>% filter(panel_tenure <= 1),
  shp_final = shp_final %>% filter(panel_tenure <= 1),
  bes_elections_final = bes_elections_final %>% filter(panel_tenure <= 1),
  shp_politics = shp_politics %>% filter(panel_tenure <= 1)
)

nominal_sd_data_lookup <- distribution_data_lookup
nominal_sd_data_lookup_new_resp <- distribution_data_lookup_new_resp

get_weighted_category_shares <- function(data, var, weight_var, time_var, period_value, period_label, scale_min = 0, scale_max = 10) {
  d <- data %>% filter(!!sym(time_var) == period_value, !is.na(!!sym(var)), !is.na(!!sym(weight_var)), !!sym(weight_var) > 0)
  if (nrow(d) == 0) { warning(paste("No data for", var, "in period", period_value)); return(NULL) }
  design <- d %>% as_survey_design(weights = !!sym(weight_var))
  shares <- svytable(as.formula(paste0("~", var)), design = design) %>%
    as.data.frame() %>% rename(category = 1, freq = Freq) %>%
    mutate(category = as.numeric(as.character(category)), proportion = freq / sum(freq)) %>%
    filter(category >= scale_min, category <= scale_max)
  tibble(category = scale_min:scale_max) %>%
    left_join(shares %>% select(category, proportion), by = "category") %>%
    mutate(proportion = replace_na(proportion, 0), period = period_label)
}

# "Valid" period is per-variable, not per-dataset (e.g. Germany's
# healthstatus only starts 1994). Period labels stay generic
# ("First"/"Last") so downstream fill scales always have 2 levels;
# actual years/elections are kept in first_period/last_period.
get_first_last_distribution <- function(data, var, weight_var, time_var, country_name, domain_name, scale_min = 0, scale_max = 10) {
  valid_periods <- data %>% filter(!is.na(!!sym(var))) %>% pull(!!sym(time_var)) %>% unique() %>% sort()
  if (length(valid_periods) < 2) { warning(paste("Not enough periods for", var, "in", country_name)); return(NULL) }
  first_period <- min(valid_periods); last_period <- max(valid_periods)
  bind_rows(
    get_weighted_category_shares(data, var, weight_var, time_var, first_period, "First", scale_min, scale_max),
    get_weighted_category_shares(data, var, weight_var, time_var, last_period, "Last", scale_min, scale_max)
  ) %>%
    mutate(country = country_name, domain = domain_name, variable = var,
           first_period = first_period, last_period = last_period,
           period = factor(period, levels = c("First", "Last")))
}

get_nominal_sd_series <- function(data, var, weight_var, time_var, period_type, base_year) {
  d <- data %>% filter(!is.na(!!sym(var)), !is.na(!!sym(weight_var)), !!sym(weight_var) > 0)
  d <- if (period_type == "year_2period") {
    d %>% filter(!!sym(time_var) >= base_year) %>% mutate(period = base_year + ((!!sym(time_var) - base_year) %/% 2) * 2)
  } else {
    d %>% mutate(period = !!sym(time_var))
  }
  map_dfr(sort(unique(d$period)), function(p) {
    dp <- d %>% filter(period == p)
    if (nrow(dp) < 2) return(NULL)
    design <- dp %>% as_survey_design(weights = !!sym(weight_var))
    tibble(period = p, sd_level = svysd(as.formula(paste0("~", var)), design = design, na.rm = TRUE), n_obs = nrow(dp))
  }) %>% arrange(period) %>% mutate(indexed_sd = (sd_level / first(sd_level)) * 100)
}

compute_all_distributions <- function(specs_table, data_lookup) {
  pmap_dfr(specs_table, function(country, domain, variable, data_name, weight_var, time_var, tenure_var) {
    get_first_last_distribution(data_lookup[[data_name]], variable, weight_var, time_var, country, domain)
  })
}

compute_all_nominal_sd <- function(specs_table, data_lookup) {
  pmap_dfr(specs_table, function(country, domain, variable, data_name, weight_var, time_var, tenure_var, base_year, period_type) {
    get_nominal_sd_series(data_lookup[[data_name]], variable, weight_var, time_var, period_type, base_year) %>%
      mutate(country = country, domain = domain, variable = variable)
  })
}

all_distributions          <- compute_all_distributions(distribution_specs, distribution_data_lookup)
all_distributions_new_resp <- compute_all_distributions(distribution_specs, distribution_data_lookup_new_resp)
all_nominal_sd          <- compute_all_nominal_sd(nominal_sd_specs, nominal_sd_data_lookup)
all_nominal_sd_new_resp <- compute_all_nominal_sd(nominal_sd_specs, nominal_sd_data_lookup_new_resp)

all_distributions <- all_distributions %>% select(any_of(c("domain", "country", "variable", "period", "category", "proportion", "first_period", "last_period")), everything())
write_csv(all_distributions, file.path(modeloutput_path, "distribution_first_vs_last.csv"))
all_distributions_new_resp <- all_distributions_new_resp %>% select(any_of(c("domain", "country", "variable", "period", "category", "proportion", "first_period", "last_period")), everything())
# write_csv(all_distributions_new_resp, file.path(modeloutput_path, "distribution_first_vs_last_new_respondents.csv"))

all_nominal_sd <- all_nominal_sd %>% select(any_of(c("domain", "country", "variable", "period", "sd_level", "indexed_sd", "n_obs")), everything())
write_csv(all_nominal_sd, file.path(modeloutput_path, "nominal_sd.csv"))
all_nominal_sd_new_resp <- all_nominal_sd_new_resp %>% select(any_of(c("domain", "country", "variable", "period", "sd_level", "indexed_sd", "n_obs")), everything())
# write_csv(all_nominal_sd_new_resp, file.path(modeloutput_path, "nominal_sd_new_respondents.csv"))





