# ============================================================================
# 05 - Elite polarisation from CHES (UK and Switzerland)
#
# V-LINE replication package. Extracted verbatim from
# model_volatilities_091126.R lines 2576-3165 (9 Nov 2026 version).
# Paths and libraries come from R/00_config.R; source via run_all.R.
# ============================================================================

# ============================================================
# ============================================================
# Main Section
# 5a) UK ELITE POLARISATION (CHES)
# Main series: lrgen (general left-right).
# Supplementary: lrecon (economic), galtan (cultural).
# ============================================================
# ============================================================

# ---- Path ----
# Assumes a "Raw_Data" folder sibling to Processed_Data, matching
# the capitalisation convention of data_path / modeloutput_path /
# Tables_dir. Adjust if the folder is actually named differently.
ches_path <- file.path(dirname(data_path), "Raw_Data", "1999-2024_CHES_dataset_meansV2.dta")

# ---- Load, filter to UK, 2014 onwards ----
ches <- read_dta(ches_path)

ches_uk <- ches %>%
  filter(as_factor(country) == "uk", year >= 2014)

# ---- Weighted SD helper ----
# Drops (x, w) pairs where either is missing. The previous version kept
# the weights of missing x in the denominator; this only went unnoticed
# because missing positions were filtered out beforehand.
weighted_sd <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  wm <- sum(w * x) / sum(w)
  sqrt(sum(w * (x - wm)^2) / sum(w))
}

# ---- Main: general left-right, own complete-case sample ----
ches_uk_lrgen <- ches_uk %>%
  filter(!is.na(vote), !is.na(lrgen)) %>%
  group_by(year) %>%
  summarise(
    lrgen_spread = weighted_sd(lrgen, vote),
    n_parties_lrgen = n(),
    .groups = "drop"
  )

# ---- Supplementary: economic and cultural (sample unchanged) ----
ches_uk_supp <- ches_uk %>%
  filter(!is.na(vote), !is.na(lrecon), !is.na(galtan)) %>%
  group_by(year) %>%
  summarise(
    lrecon_spread = weighted_sd(lrecon, vote),
    galtan_spread = weighted_sd(galtan, vote),
    n_parties = n(),
    .groups = "drop"
  )

ches_uk_spread <- full_join(ches_uk_lrgen, ches_uk_supp, by = "year") %>%
  arrange(year) %>%
  mutate(
    indexed_lrgen_spread  = (lrgen_spread  / first(lrgen_spread))  * 100,
    indexed_lrecon_spread = (lrecon_spread / first(lrecon_spread)) * 100,
    indexed_galtan_spread = (galtan_spread / first(galtan_spread)) * 100,
    country = "UK (CHES Elite)",
    party_sample = "All Parties"
  ) %>%
  select(country, party_sample, year, n_parties_lrgen, n_parties,
         lrgen_spread, indexed_lrgen_spread,
         lrecon_spread, indexed_lrecon_spread,
         galtan_spread, indexed_galtan_spread)

cat("\n✓ CHES elite polarisation computed —", nrow(ches_uk_spread), "years\n")
print(ches_uk_spread, n = Inf)

write_csv(ches_uk_spread, file.path(modeloutput_path, "elite_polarisation_ches.csv"))

cat("✓ Saved elite_polarisation_ches.csv\n")





# ============================================================
# Swiss CHES elite polarisation, 2010/2014/2019 waves,
# vote-share-weighted using BFS (opendata.swiss) party strengths.
# Main series: lrgen. Supplementary: lrecon, galtan.
# ============================================================

ches_path_2010 <- file.path(dirname(data_path), "Raw_Data", "2010_CHES_dataset_means.dta")
ches_path_2014 <- file.path(dirname(data_path), "Raw_Data", "2014_CHES_dataset_means-2.dta")
ches_path_2019 <- file.path(dirname(data_path), "Raw_Data", "CHES2019V3.dta")
bfs_path       <- file.path(dirname(data_path), "Raw_Data", "swiss_vote_shares.csv")

ches_2010 <- read_dta(ches_path_2010)
ches_2014 <- read_dta(ches_path_2014)
ches_2019 <- read_dta(ches_path_2019)

bfs <- read_delim(bfs_path, delim = ";", show_col_types = FALSE) %>%
  filter(geoLevelName == "Schweiz")


# ---- Crosswalks: CHES party label -> BFS partei_bezeichnung_de ----
# Built by hand per wave, since CHES spells the same party differently
# across its own waves (e.g. SPS/PSS in 2010 vs SP/PS in 2014).

crosswalk_2010 <- tribble(
  ~party_name,     ~bfs_party,
  "SVP/UDC",       "SVP",
  "SPS/PSS",       "SP",
  "FDP/PLR",       "FDP",
  "CVP/PVC",       "CVP",
  "GPS/PES",       "GRÜNE",
  "GLP/PVL",       "GLP",
  "EVP/PEV",       "EVP",
  "EDU/UDF",       "EDU",
  "PdA/PST-POP",   "PdA/Sol.",
  "LdT",           "Lega",
  "CSP/PCS",       "CSP",
  "BDP",           "BDP"
)

crosswalk_2014 <- tribble(
  ~party_name,     ~bfs_party,
  "SVP/UDC",       "SVP",
  "SP/PS",         "SP",
  "FDP/PLR",       "FDP",
  "CVP/PVC",       "CVP",
  "GPS/PES",       "GRÜNE",
  "GLP/PVL",       "GLP",
  "EVP/PEV",       "EVP",
  "EDU/UDF",       "EDU",
  "LdT",           "Lega",
  "CSP/PCS",       "CSP",
  "BDP",           "BDP"
)

crosswalk_2019 <- tribble(
  ~party,          ~bfs_party,
  "SVP/UDC",       "SVP",
  "SP/PS",         "SP",
  "FDP/PLR",       "FDP",
  "CVP/PVC",       "CVP",
  "GPS/PES",       "GRÜNE",
  "GPL/PVL",       "GLP",
  "EVP/PEV",       "EVP",
  "BDP/PBD",       "BDP"
)

# ---- Build one Swiss row-per-party-per-wave table ----

swi_2010 <- ches_2010 %>%
  filter(cname == "swi") %>%
  select(party_name, lrgen, lrecon, galtan) %>%
  left_join(crosswalk_2010, by = "party_name") %>%
  left_join(bfs %>% filter(wahl_jahr == 2007) %>% select(partei_bezeichnung_de, partei_staerke),
            by = c("bfs_party" = "partei_bezeichnung_de")) %>%
  mutate(ches_wave = 2010, election_year = 2007)

swi_2014 <- ches_2014 %>%
  filter(cname == "swi") %>%
  select(party_name, lrgen, lrecon, galtan) %>%
  left_join(crosswalk_2014, by = "party_name") %>%
  left_join(bfs %>% filter(wahl_jahr == 2011) %>% select(partei_bezeichnung_de, partei_staerke),
            by = c("bfs_party" = "partei_bezeichnung_de")) %>%
  mutate(ches_wave = 2014, election_year = 2011)

swi_2019 <- ches_2019 %>%
  filter(country == 36) %>%
  select(party, lrgen, lrecon, galtan) %>%
  left_join(crosswalk_2019, by = "party") %>%
  left_join(bfs %>% filter(wahl_jahr == 2019) %>% select(partei_bezeichnung_de, partei_staerke),
            by = c("bfs_party" = "partei_bezeichnung_de")) %>%
  mutate(ches_wave = 2019, election_year = 2019)

swi_all <- bind_rows(swi_2010, swi_2014, swi_2019)

# BDP has no 2007 BFS row (didn't exist as a party until 2008) — that's
# the one expected NA here; inspect before dropping in case of others.
swi_all %>% filter(is.na(partei_staerke)) %>% print(n = Inf)

swi_all <- swi_all %>% filter(!is.na(partei_staerke))

# ---- Vote-share-weighted spread per wave ----
swi_spread <- swi_all %>%
  group_by(ches_wave) %>%
  summarise(
    lrgen_spread  = weighted_sd(lrgen,  partei_staerke),
    lrecon_spread = weighted_sd(lrecon, partei_staerke),
    galtan_spread = weighted_sd(galtan, partei_staerke),
    n_parties_lrgen = sum(!is.na(lrgen)),
    n_parties = n(),
    .groups = "drop"
  ) %>%
  arrange(ches_wave) %>%
  mutate(
    indexed_lrgen_spread  = (lrgen_spread  / first(lrgen_spread))  * 100,
    indexed_lrecon_spread = (lrecon_spread / first(lrecon_spread)) * 100,
    indexed_galtan_spread = (galtan_spread / first(galtan_spread)) * 100,
    country = "Switzerland (CHES Elite)",
    party_sample = "All Parties",
    year = ches_wave
  ) %>%
  select(country, party_sample, year, n_parties_lrgen, n_parties,
         lrgen_spread, indexed_lrgen_spread,
         lrecon_spread, indexed_lrecon_spread,
         galtan_spread, indexed_galtan_spread)

print(swi_spread, n = Inf)

write_csv(swi_spread, file.path(modeloutput_path, "elite_polarisation_ches_switzerland.csv"))



# # ============================================================
# # ============================================================
# # Main Section
# # 5a) UK ELITE POLARISATION (CHES)
# # ============================================================
# # ============================================================
# 
# # ---- Path ----
# # Assumes a "Raw_Data" folder sibling to Processed_Data, matching
# # the capitalisation convention of data_path / modeloutput_path /
# # Tables_dir. Adjust if the folder is actually named differently.
# ches_path <- file.path(dirname(data_path), "Raw_Data", "1999-2024_CHES_dataset_meansV2.dta")
# 
# # ---- Load, filter to UK, 2014 onwards ----
# ches <- read_dta(ches_path)
# 
# ches_uk <- ches %>%
#   filter(as_factor(country) == "uk", year >= 2014)
# 
# # ---- Weighted SD helper ----
# weighted_sd <- function(x, w) {
#   wm <- weighted.mean(x, w, na.rm = TRUE)
#   sqrt(sum(w * (x - wm)^2, na.rm = TRUE) / sum(w, na.rm = TRUE))
# }
# 
# # ---- Vote-share-weighted spread by year, all parties ----
# ches_uk_spread <- ches_uk %>%
#   select(year, party, vote, lrecon, galtan) %>%
#   filter(!is.na(vote), !is.na(lrecon), !is.na(galtan)) %>%
#   group_by(year) %>%
#   summarise(
#     lrecon_spread = weighted_sd(lrecon, vote),
#     galtan_spread = weighted_sd(galtan, vote),
#     n_parties = n(),
#     .groups = "drop"
#   ) %>%
#   arrange(year) %>%
#   mutate(
#     indexed_lrecon_spread = (lrecon_spread / first(lrecon_spread)) * 100,
#     indexed_galtan_spread = (galtan_spread / first(galtan_spread)) * 100,
#     country = "UK (CHES Elite)",
#     party_sample = "All Parties"
#   ) %>%
#   select(country, party_sample, year, n_parties,
#          lrecon_spread, indexed_lrecon_spread,
#          galtan_spread, indexed_galtan_spread)
# 
# cat("\n✓ CHES elite polarisation computed —", nrow(ches_uk_spread), "years\n")
# print(ches_uk_spread, n = Inf)
# 
# write_csv(ches_uk_spread, file.path(modeloutput_path, "elite_polarisation_ches.csv"))
# 
# cat("✓ Saved elite_polarisation_ches.csv\n")
# 
# 
# 
# 
# 
# # ============================================================
# # Swiss CHES elite polarisation, 2010/2014/2019 waves,
# # vote-share-weighted using BFS (opendata.swiss) party strengths
# # ============================================================
# 
# ches_path_2010 <- file.path(dirname(data_path), "Raw_Data", "2010_CHES_dataset_means.dta")
# ches_path_2014 <- file.path(dirname(data_path), "Raw_Data", "2014_CHES_dataset_means-2.dta")
# ches_path_2019 <- file.path(dirname(data_path), "Raw_Data", "CHES2019V3.dta")
# bfs_path       <- file.path(dirname(data_path), "Raw_Data", "swiss_vote_shares.csv")
# 
# ches_2010 <- read_dta(ches_path_2010)
# ches_2014 <- read_dta(ches_path_2014)
# ches_2019 <- read_dta(ches_path_2019)
# 
# bfs <- read_delim(bfs_path, delim = ";", show_col_types = FALSE) %>%
#   filter(geoLevelName == "Schweiz")
# 
# 
# # ---- Crosswalks: CHES party label -> BFS partei_bezeichnung_de ----
# # Built by hand per wave, since CHES spells the same party differently
# # across its own waves (e.g. SPS/PSS in 2010 vs SP/PS in 2014).
# 
# crosswalk_2010 <- tribble(
#   ~party_name,     ~bfs_party,
#   "SVP/UDC",       "SVP",
#   "SPS/PSS",       "SP",
#   "FDP/PLR",       "FDP",
#   "CVP/PVC",       "CVP",
#   "GPS/PES",       "GRÜNE",
#   "GLP/PVL",       "GLP",
#   "EVP/PEV",       "EVP",
#   "EDU/UDF",       "EDU",
#   "PdA/PST-POP",   "PdA/Sol.",
#   "LdT",           "Lega",
#   "CSP/PCS",       "CSP",
#   "BDP",           "BDP"
# )
# 
# crosswalk_2014 <- tribble(
#   ~party_name,     ~bfs_party,
#   "SVP/UDC",       "SVP",
#   "SP/PS",         "SP",
#   "FDP/PLR",       "FDP",
#   "CVP/PVC",       "CVP",
#   "GPS/PES",       "GRÜNE",
#   "GLP/PVL",       "GLP",
#   "EVP/PEV",       "EVP",
#   "EDU/UDF",       "EDU",
#   "LdT",           "Lega",
#   "CSP/PCS",       "CSP",
#   "BDP",           "BDP"
# )
# 
# crosswalk_2019 <- tribble(
#   ~party,          ~bfs_party,
#   "SVP/UDC",       "SVP",
#   "SP/PS",         "SP",
#   "FDP/PLR",       "FDP",
#   "CVP/PVC",       "CVP",
#   "GPS/PES",       "GRÜNE",
#   "GPL/PVL",       "GLP",
#   "EVP/PEV",       "EVP",
#   "BDP/PBD",       "BDP"
# )
# 
# # ---- Build one Swiss row-per-party-per-wave table ----
# 
# swi_2010 <- ches_2010 %>%
#   filter(cname == "swi") %>%
#   select(party_name, lrecon, galtan) %>%
#   left_join(crosswalk_2010, by = "party_name") %>%
#   left_join(bfs %>% filter(wahl_jahr == 2007) %>% select(partei_bezeichnung_de, partei_staerke),
#             by = c("bfs_party" = "partei_bezeichnung_de")) %>%
#   mutate(ches_wave = 2010, election_year = 2007)
# 
# swi_2014 <- ches_2014 %>%
#   filter(cname == "swi") %>%
#   select(party_name, lrecon, galtan) %>%
#   left_join(crosswalk_2014, by = "party_name") %>%
#   left_join(bfs %>% filter(wahl_jahr == 2011) %>% select(partei_bezeichnung_de, partei_staerke),
#             by = c("bfs_party" = "partei_bezeichnung_de")) %>%
#   mutate(ches_wave = 2014, election_year = 2011)
# 
# swi_2019 <- ches_2019 %>%
#   filter(country == 36) %>%
#   select(party, lrecon, galtan) %>%
#   left_join(crosswalk_2019, by = "party") %>%
#   left_join(bfs %>% filter(wahl_jahr == 2019) %>% select(partei_bezeichnung_de, partei_staerke),
#             by = c("bfs_party" = "partei_bezeichnung_de")) %>%
#   mutate(ches_wave = 2019, election_year = 2019)
# 
# swi_all <- bind_rows(swi_2010, swi_2014, swi_2019)
# 
# # BDP has no 2007 BFS row (didn't exist as a party until 2008) — that's
# # the one expected NA here; inspect before dropping in case of others.
# swi_all %>% filter(is.na(partei_staerke)) %>% print(n = Inf)
# 
# swi_all <- swi_all %>% filter(!is.na(partei_staerke))
# 
# # ---- Vote-share-weighted spread per wave ----
# swi_spread <- swi_all %>%
#   group_by(ches_wave) %>%
#   summarise(
#     lrecon_spread = weighted_sd(lrecon, partei_staerke),
#     galtan_spread = weighted_sd(galtan, partei_staerke),
#     n_parties = n(),
#     .groups = "drop"
#   ) %>%
#   arrange(ches_wave) %>%
#   mutate(
#     indexed_lrecon_spread = (lrecon_spread / first(lrecon_spread)) * 100,
#     indexed_galtan_spread = (galtan_spread / first(galtan_spread)) * 100,
#     country = "Switzerland (CHES Elite)",
#     party_sample = "All Parties",
#     year = ches_wave
#   ) %>%
#   select(country, party_sample, year, n_parties,
#          lrecon_spread, indexed_lrecon_spread,
#          galtan_spread, indexed_galtan_spread)
# 
# 
# write_csv(swi_spread, file.path(modeloutput_path, "elite_polarisation_ches_switzerland.csv"))





# Checking on number of experts:
# CHES expert-level counts: UK and Switzerland respondents to lrecon/galtan
# by wave (2010, 2014, 2019, 2024)
#
# Assumes the four raw CSVs are sitting in ~/Downloads with their original
# filenames, e.g.:
#   2010_CHES_dataset_expert-level.csv
#   2014_CHES_dataset_expert-level.csv
#   CHES2019_experts.csv
#   CHES_2024_ALL_Stacked_Expert.csv
#

library(dplyr)
library(readr)

downloads <- path.expand("~/Downloads")

# ---- 1. Load each wave --------------------------------------------------

d2010 <- read_csv(file.path(downloads, "2010_CHES_dataset_expert-level.csv"), show_col_types = FALSE)
d2014 <- read_csv(file.path(downloads, "2014_CHES_dataset_expert-level.csv"), show_col_types = FALSE)
d2019 <- read_csv(file.path(downloads, "CHES2019_experts.csv"), show_col_types = FALSE)
d2024 <- read_csv(file.path(downloads, "CHES_2024_ALL_Stacked_Expert.csv"), show_col_types = FALSE)

# ---- 2. Sanity-check the country label for each file --------------------
# Different waves spell the country field differently (numeric code, 3-letter
# abbreviation, or full name). Inspect before filtering so you don't silently
# match zero rows.

cat("2010 cname values:\n"); print(sort(unique(d2010$cname)))
cat("\n2014 cname values:\n"); print(sort(unique(d2014$cname)))
cat("\n2019 cname values:\n"); print(sort(unique(d2019$cname)))
cat("\n2024 cname values:\n"); print(sort(unique(d2024$cname)))

# ---- 3. Helper: count distinct experts answering each variable ----------

count_experts <- function(df, id_col, country_col, country_val,
                          vars = c("lrgen", "lrecon", "galtan")) {
  sub <- df %>% filter(.data[[country_col]] == country_val)
  
  out <- tibble(
    country = country_val,
    n_rows = nrow(sub),
    n_experts_any = n_distinct(sub[[id_col]])
  )
  
  for (v in vars) {
    if (!v %in% names(sub)) {                 # item not in this wave's file
      out[[paste0(v, "_answered")]] <- NA_integer_
      next
    }
    out[[paste0(v, "_answered")]] <- sub %>%
      filter(!is.na(.data[[v]])) %>%
      summarise(n = n_distinct(.data[[id_col]])) %>%
      pull(n)
  }
  out
}

# ---- 4. Run for UK and Switzerland, each wave ----------------------------

results <- bind_rows(
  count_experts(d2010, "expert_id", "cname", "swi") %>% mutate(wave = 2010),
  
  count_experts(d2014, "id", "cname", "uk")  %>% mutate(wave = 2014),
  count_experts(d2014, "id", "cname", "swi") %>% mutate(wave = 2014),
  
  count_experts(d2019, "id", "cname", "uk")  %>% mutate(wave = 2019),
  count_experts(d2019, "id", "cname", "swi") %>% mutate(wave = 2019),
  
  count_experts(d2024, "id", "cname", "United Kingdom") %>% mutate(wave = 2024)
) %>%
  relocate(wave, country)

print(results, width = Inf)

# ---- 5. Export LaTeX table body (Table E: paper's tab:ches_expert_counts) --

ches_tex <- results %>%
  mutate(Country = recode(country, swi = "Switzerland", uk = "UK", `United Kingdom` = "UK")) %>%
  arrange(Country, wave) %>%
  transmute(Country, Wave = wave,
            lrgen = lrgen_answered, lrecon = lrecon_answered, galtan = galtan_answered) %>%
  mutate(across(c(lrgen, lrecon, galtan),
                ~ ifelse(is.na(.x), "---", as.character(.x))))

lines <- paste0(ifelse(duplicated(ches_tex$Country), "", ches_tex$Country), " & ",
                ches_tex$Wave, " & ", ches_tex$lrgen, " & ",
                ches_tex$lrecon, " & ", ches_tex$galtan, " \\\\")

first_uk <- which(ches_tex$Country == "UK")[1]          # rule between countries
lines <- append(lines, "\\midrule", after = first_uk - 1)

writeLines(lines, file.path(Tables_dir, "Table_E_body.tex"))





# ============================================================
# ============================================================
# Main Section
# 5b) Trends in Household Income Gini (Germany, Australia, Switzerland)
# Used eventually for 'paradox plots'
# ============================================================
# ============================================================

# ------------------------------------------------------------
# Weighted Gini (Lorenz-curve / trapezoidal-rule formulation).
# Self-contained — no new package dependency.
# ------------------------------------------------------------

weighted_gini <- function(x, w) {
  
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2) return(NA_real_)
  
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  
  W <- sum(w)
  cum_w  <- cumsum(w) / W
  cum_xw <- cumsum(x * w) / sum(x * w)
  
  cum_w_lag  <- c(0, head(cum_w, -1))
  cum_xw_lag <- c(0, head(cum_xw, -1))
  
  1 - sum((w / W) * (cum_xw + cum_xw_lag))
}

# ------------------------------------------------------------
# Gini by 2-year period (mirrors calculate_cross_sectional_stats_income)
# ------------------------------------------------------------

calculate_gini_series <- function(data, income_var, weight_var = "norm_weight", time_var = "year") {
  
  min_year <- min(data[[time_var]], na.rm = TRUE)
  
  data <- data %>%
    mutate(
      years_since_start = !!sym(time_var) - min_year,
      time_period = min_year + (years_since_start %/% 2) * 2
    )
  
  data %>%
    filter(!is.na(!!sym(income_var)), !is.na(!!sym(weight_var)), !!sym(weight_var) > 0) %>%
    group_by(time_period) %>%
    summarise(
      gini = weighted_gini(!!sym(income_var), !!sym(weight_var)),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    arrange(time_period) %>%
    mutate(indexed_gini = (gini / first(gini)) * 100)
}

# ------------------------------------------------------------
# Compute and combine
# ------------------------------------------------------------

cat("\n========== HOUSEHOLD INCOME GINI: GERMANY, AUSTRALIA & SWITZERLAND ==========\n")

gini_ger <- calculate_gini_series(germany_income_data, "hh_income", weight_var = "norm_weight", time_var = "year") %>%
  mutate(country = "Germany")

gini_aus <- calculate_gini_series(australia_income_data, "hh_income", weight_var = "norm_weight", time_var = "year") %>%
  mutate(country = "Australia")

# SHP's LS data starts in 2000; so we also want our 'paradox' income data to be indexed to 2000
gini_che <- calculate_gini_series(
  switzerland_income_data %>% filter(year >= 2000),
  "hh_income", weight_var = "norm_weight", time_var = "year"
) %>%
  mutate(country = "Switzerland")

income_gini <- bind_rows(gini_ger, gini_aus, gini_che) %>%
  select(country, time_period, gini, indexed_gini, n_obs)

print(income_gini, n = Inf)

write_csv(income_gini, file.path(modeloutput_path, "income_gini.csv"))

cat("\n✓ Saved income_gini.csv\n")












