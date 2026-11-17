# ============================================================================
# 01 - Load and clean the four panels (SOEP, HILDA, SHP, BES)
#
# V-LINE replication package. Extracted verbatim from
# model_volatilities_091126.R lines 65-484 (9 Nov 2026 version).
# Paths and libraries come from R/00_config.R; source via run_all.R.
#
# One change from the source: days_gap is no longer selected from the Swiss
# data. It was built by a separate interview-date step that is not part of
# this pipeline, and no model uses it outside the BES, which builds its own
# days_gap in stata/BES_get.do.
# ============================================================================



# Load datasets
germany <- read_dta(file.path(data_path, "Germany.dta"))
australia <- read_dta(file.path(data_path, "Australia.dta"))
switzerland <- read_dta(file.path(data_path, "Switzerland.dta"))
bes_elections <- read_dta(file.path(data_path, "bes_elections.dta"))
bes_yearly <- read_dta(file.path(data_path, "bes_yearly.dta"))


# Create categorical variables
make_cats <- function(df) {
  df <- df %>%
    mutate(
      migback_collapsed = factor(migback_collapsed, levels = c(0, 1)),  # 0 = native-born
      agecat = case_when(
        age < 25             ~ "Under 25",
        age >= 25 & age < 35 ~ "25–34",
        age >= 35 & age < 45 ~ "35–44",
        age >= 45 & age < 55 ~ "45–54",
        age >= 55 & age < 65 ~ "55–64",
        age >= 65 & age < 75 ~ "65–74",
        age >= 75            ~ "75+",
        TRUE                 ~ NA_character_
      ),
      agecat = factor(agecat, levels = c(
        "45–54", "Under 25", "25–34", "35–44", "55–64", "65–74", "75+"
      ))
    )
  return(df)
}

# Create survey design object
create_survey_design <- function(data, weight_var = "norm_weight") {
  design <- data %>%
    filter(!is.na(!!sym(weight_var))) %>%  # Remove missing weights
    as_survey_design(weights = !!sym(weight_var))
  return(design)
}

recode_negative_satisfaction <- function(data, sat_vars) {
  # Loop through each satisfaction variable
  for (sat_var in sat_vars) {
    # Check if variable exists in the dataset
    if (!(sat_var %in% names(data))) {
      warning(paste("Variable", sat_var, "not found in data"))
      next
    }
    
    # Recode negative satisfaction values as NA for the specified satisfaction variable
    data <- data %>%
      mutate(
        !!sat_var := if_else(get(sat_var) < 0, NA_real_, get(sat_var))
      )
  }
  
  return(data)
}



# --- Data Cleaning for Each Dataset ---

# Define variables upfront

# Not looking at domain satisfactions now
# satisfaction_vars <- c("lfsato", "flatsat", "jobsat", "healthsat", "familysat")

satisfaction_vars <- c("lfsato")

exog_controls_standard <- c("agecat", "migback_collapsed", "sex")

# Function to remove haven labels from satisfaction variables
remove_labels <- function(data, vars) {
  for (var in vars) {
    if (var %in% names(data)) {
      data[[var]] <- as.numeric(data[[var]])
    }
  }
  return(data)
}


# ============================================================================
# GERMANY DATA CLEANING
# ============================================================================
germany_cleaned <- germany %>%
  # Apply categorical variables
  make_cats() %>%
  # Rename for consistency
  rename(owninc = owninceq) %>%
  # Remove migration samples
  filter(!(psample >= 15 & psample <= 19),   # Remove M1-M5
         !(psample >= 24 & psample <= 26)) %>% # Remove M6-M8
  # Recode satisfaction variables
  recode_negative_satisfaction(satisfaction_vars) %>%
  mutate(healthstatus = ifelse(year < 1994, NA_real_, healthstatus)) %>%
  # Remove haven labels
  remove_labels(c(satisfaction_vars, "pid", "year"))  %>%
  rename(hh_income = hhyneti_defl) %>%
  mutate(hh_size = nadult + nkids_collapsed)
  

# ============================================================================
# AUSTRALIA DATA CLEANING
# ============================================================================
australia_cleaned <- australia %>%
  # Standardize satisfaction variable name
  rename(lfsato = losat) %>%
  # Apply categorical variables
  make_cats() %>%
  # Recode satisfaction variables
  recode_negative_satisfaction(satisfaction_vars) %>%
  # Remove haven labels
  remove_labels(c(satisfaction_vars, "pid", "year")) %>%
  mutate(hh_size = nadult + nkids)


# ============================================================
# BES DATA CLEANING
# ============================================================

# Ensure factors
bes_elections <- bes_elections %>%
  # Drop Stata value labels (e.g. on lr): labelled vectors break survey::svyvar arithmetic
  haven::zap_labels() %>%
  mutate(
    election = as.numeric(as.character(election)),
    wave_pos = as.numeric(as.character(wave_pos)),
    lr_count = as.numeric(as.character(lr_count)),
    age = as.numeric(as.character(age)),
    agecat = case_when(
      age < 25             ~ "Under 25",
      age >= 25 & age < 35 ~ "25–34",
      age >= 35 & age < 45 ~ "35–44",
      age >= 45 & age < 55 ~ "45–54",
      age >= 55 & age < 65 ~ "55–64",
      age >= 65 & age < 75 ~ "65–74",
      age >= 75            ~ "75+",
      TRUE                 ~ NA_character_
    ),
    agecat = factor(agecat, levels = c(
      "45–54", "Under 25", "25–34", "35–44", "55–64", "65–74", "75+"
    ))
  )

# Ensure factors
bes_yearly <- bes_yearly %>%
  # Drop Stata value labels (e.g. on lr): labelled vectors break survey::svyvar arithmetic
  haven::zap_labels() %>%
  mutate(
    year = as.numeric(as.character(year)),
    lr_count = as.numeric(as.character(lr_count)),
    age = as.numeric(as.character(age)),
    agecat = case_when(
      age < 25             ~ "Under 25",
      age >= 25 & age < 35 ~ "25–34",
      age >= 35 & age < 45 ~ "35–44",
      age >= 45 & age < 55 ~ "45–54",
      age >= 55 & age < 65 ~ "55–64",
      age >= 65 & age < 75 ~ "65–74",
      age >= 75            ~ "75+",
      TRUE                 ~ NA_character_
    ),
    agecat = factor(agecat, levels = c(
      "45–54", "Under 25", "25–34", "35–44", "55–64", "65–74", "75+"
    ))
  )



# ============================================================
# SWITZERLAND DATA CLEANING - a bit longer. No data-cleaning in Stata
# ============================================================

shp_cleaned <- switzerland %>%
  mutate(
    # Controls
    sex = as.numeric(sex),
    year = as.numeric(year),
    age = as.numeric(age),
    # Weights
    norm_weight = as.numeric(wicss),
    norm_weight = ifelse(is.na(norm_weight) | norm_weight <= 0, 1, norm_weight)
  ) %>%
  make_cats() %>%
  # Keep only necessary variables
  select(
    # Identifiers and structure
    pid, year, norm_weight,
    # Controls
    age, agecat, sex, migback_collapsed,
    # Life satisfaction variables
    sat_life, sat_job, sat_health, sat_finances,
    # Political variables
    lrself, pol_interest, trust_gov, sat_democracy, hh_income
  )


# ============================================================
# ============================================================
# Main Sections:
# 2) Volatility
# ============================================================
# ============================================================


create_within_person_differences <- function(data, 
                                             sat_vars,
                                             id_var = "pid",
                                             time_var = "year") {
  
  require(data.table)
  require(dplyr)
  
  cat("\n========== Creating Within-Person Differences ==========\n")
  
  setDT(data)
  setorderv(data, c(id_var, time_var))
  
  for (sat_var in sat_vars) {
    if (!(sat_var %in% names(data))) {
      cat("  ✗", sat_var, "- Variable not found in data\n")
      next
    }
    
    abs_diff_name <- paste0("abs_d_", sat_var)
    diff_name <- paste0("d_", sat_var)
    
    # Calculate difference using LEAD (future - current)
    data[, (diff_name) := shift(get(sat_var), 1L, type = "lead") - get(sat_var), by = id_var]
    
    # Calculate absolute difference
    data[, (abs_diff_name) := abs(get(diff_name))]
    
    # NEW
    # After calculating differences, set to NA if years aren't consecutive
    data[, year_gap := shift(get(time_var), 1L, type = "lead") - get(time_var), by = id_var]
    data[year_gap != 1, (diff_name) := NA]
    data[year_gap != 1, (abs_diff_name) := NA]
    
    cat("  ✓", sat_var, "- Created", abs_diff_name, "and", diff_name, "\n")
  }
  
  setDF(data)
  
  return(data)
}


# create_panel_tenure <- function(data, id_var = "pid", time_var = "year") {
#   # Ensure data is ordered by ID and time
#   data <- data %>%
#     arrange(!!sym(id_var), !!sym(time_var))
#   
#   # Create panel_tenure variable (survey response number: 1st, 2nd, 3rd, etc.)
#   data <- data %>%
#     group_by(!!sym(id_var)) %>%
#     mutate(panel_tenure = row_number() - 1) %>%  # Count of Survey Responses. 1st Response = 0 (we want this as the reference category!)
#     ungroup()
#   
#   return(data)
# }

# Only counts valid responses to a particular variable
create_panel_tenure <- function(data, response_var, id_var = "pid", time_var = "year",
                                tenure_name = "panel_tenure") {
  data %>%
    arrange(!!sym(id_var), !!sym(time_var)) %>%
    group_by(!!sym(id_var)) %>%
    # Earlier valid responses to THIS question (first valid response = 0)
    mutate(!!tenure_name := cumsum(!is.na(.data[[response_var]])) - 1,
           !!tenure_name := if_else(is.na(.data[[response_var]]), NA_real_, .data[[tenure_name]])) %>%
    ungroup()
}


# ============================================================
# Create volatilities
# ============================================================

# Define all variables to analyze

input_vars <- c("healthstatus")
all_vars <- c(satisfaction_vars, input_vars)
shp_vars <- c("lrself", "sat_life")

# Create within-person differences for ALL variables at once
processed_ger <- create_within_person_differences(
  data = germany_cleaned,
  sat_vars = all_vars,
  id_var = "pid",
  time_var = "year"
)

processed_aus <- create_within_person_differences(
  data = australia_cleaned,
  sat_vars = all_vars,
  id_var = "pid",
  time_var = "year"
)

shp_processed <- create_within_person_differences(
  data = shp_cleaned,
  sat_vars = shp_vars,
  id_var = "pid",
  time_var = "year"
)

# within differences is already created for BESIP in Stata

# ger_final <- create_panel_tenure(processed_ger)
# aus_final <- create_panel_tenure(processed_aus)
# shp_final <- create_panel_tenure(shp_processed)
ger_final <- create_panel_tenure(processed_ger, "lfsato")
aus_final <- create_panel_tenure(processed_aus, "lfsato")
shp_final <- create_panel_tenure(shp_processed, "sat_life")

# bes_elections$panel_tenure <- bes_elections$lr_count
# bes_yearly$panel_tenure <- bes_yearly$lr_count

# BES: same function, run on ALL 30 waves (bes_lr_allwaves.dta, saved by BES_get.do),
# so earlier answers in non-election waves are counted too; then attached by id + wave.
bes_allwaves <- read_dta(file.path(data_path, "bes_lr_allwaves.dta")) %>%
  mutate(across(c(id, wave, lr, lr_count), as.numeric))

bes_tenure <- create_panel_tenure(bes_allwaves, "lr", id_var = "id", time_var = "wave")

# Sanity check: on rows with a valid LR answer, the new count must equal Stata's lr_count
bes_chk <- bes_tenure %>% filter(!is.na(lr))
cat("\nBES tenure check: rows where panel_tenure != lr_count:",
    sum(bes_chk$panel_tenure != bes_chk$lr_count), "of", nrow(bes_chk), "\n")

bes_tenure <- bes_tenure %>% select(id, wave, panel_tenure)
bes_elections <- bes_elections %>%
  mutate(id = as.numeric(id), wave = as.numeric(wave)) %>%
  left_join(bes_tenure, by = c("id", "wave"))
bes_yearly <- bes_yearly %>%
  mutate(id = as.numeric(id), wave = as.numeric(wave)) %>%
  left_join(bes_tenure, by = c("id", "wave"))


bes_elections_final <- bes_elections %>%
  mutate(
    election = as.numeric(election),
    wave_pos = as.numeric(wave_pos),
    lr_count = as.numeric(lr_count)
  ) %>%
  rename(pid = id)



# Checking on refresher samples in SHP
shp_final %>%
  filter(year %in% c(2019, 2020)) %>%
  group_by(year) %>%
  summarise(
    n_total          = n(),
    n_valid_lrself   = sum(!is.na(lrself)),
    pct_missing_lr   = mean(is.na(lrself)),
    n_valid_sat_life = sum(!is.na(sat_life)),
    pct_missing_sat  = mean(is.na(sat_life))
  )

# Sample size doubles in 2020!
# Identify pids first observed in 2020 to drop them
new_2020 <- shp_final %>%
  filter(year == 2020) %>%
  pull(pid) %>%
  setdiff(shp_final %>% filter(year == 2019) %>% pull(pid))

length(new_2020)
# Many people joined SHP in 2020

shp_final <- shp_final %>%
  filter(
    !(pid %in% new_2020)
  )

# SHP politics: own dataset, so panel_tenure counts earlier valid LEFT-RIGHT answers.
# Same sample rule as shp_final (2020 refresher entrants dropped); no year cap.
shp_politics <- create_panel_tenure(shp_processed, "lrself") %>%
  filter(!(pid %in% new_2020))

# COVID-era (robustness check only) — uncapped, deduped, full year range
ger_covid <- ger_final
shp_covid <- shp_final


# Main LS analysis samples — capped pre-COVID
ger_final <- ger_final %>% filter(year <= 2019)
shp_final <- shp_final %>% filter(year <= 2019)

# Health placebo: own datasets, so panel_tenure counts earlier valid HEALTH answers.
# Germany capped as ger_final; Australia uncapped, as aus_final.
ger_health <- create_panel_tenure(processed_ger, "healthstatus") %>% filter(year <= 2019)
aus_health <- create_panel_tenure(processed_aus, "healthstatus")

# ---- Tenure checks ----
# (1) No survey missing-value codes left: a negative code would be counted as a valid answer.
cat("\nMinimum values (none should be negative):\n")
print(sapply(list(ger_lfsato = ger_final$lfsato, aus_lfsato = aus_final$lfsato,
                  ger_health = ger_health$healthstatus, aus_health = aus_health$healthstatus,
                  shp_ls = shp_final$sat_life, shp_lr = shp_politics$lrself),
             function(x) min(x, na.rm = TRUE)))

# (2) Share of rows where the new tenure equals the old row-count definition
#     (row_number() - 1 within the frame). Not expected to be 1: one early missing answer
#     shifts all of that person's later rows by one. Expect it higher for SOEP/HILDA than SHP.
old_vs_new <- function(d) {
  d %>% group_by(pid) %>% arrange(year, .by_group = TRUE) %>%
    mutate(old_tenure = row_number() - 1) %>% ungroup() %>%
    summarise(share_same = mean(panel_tenure == old_tenure, na.rm = TRUE)) %>% pull()
}
cat("\nShare of rows with new tenure == old row-count tenure:\n")
print(c(ger = old_vs_new(ger_final), aus = old_vs_new(aus_final),
        shp_ls = old_vs_new(shp_final), shp_lr = old_vs_new(shp_politics)))



