# ============================================================================
# 10 - World Inequality Database cleaning (Top 10 / Bottom 50 ratios)
#
# V-LINE replication package. Copied verbatim from wid_cleaning_091126.R
# (9 Nov 2026 version).
#
# In the original this had to run immediately after model_volatilities, from
# which it inherited data_path and modeloutput_path through the global
# environment. Those now come from R/00_config.R, so the ordering trap is
# gone; run_all.R still runs it in the same position.
#
# Set DOWNLOAD_WID <- TRUE in R/00_config.R to refresh the series from the
# World Inequality Database. That needs network access and overwrites the
# CSVs in data/Raw_Data/WID. Default is FALSE: the shipped CSVs reproduce
# the paper's numbers.
# ============================================================================

# ============================================================
# ============================================================
# Charlie Harrison
# Written: 9th November 2026
#
# Main Section
# 5b) WID INEQUALITY: TOP 10 / BOTTOM 50 RATIOS
#
# Supplementary data-cleaning section. Paste after Section 5a
# (CHES elite polarisation) in the cleaning script. It uses the
# same objects: data_path, modeloutput_path.
#
# Replaces the within-panel household income Gini
# (income_gini.csv) as the "objective" inequality series in the
# life satisfaction plots.
#
# Series: Top 10 / Bottom 50 ratio, as in the slides, for
#   - pre-tax national income   (WID concept "ptinc")
#   - net personal wealth       (WID concept "hweal")
# Adults aged 20+ ("992"), equal-split between spouses ("j").
# Countries: Germany (DE), Australia (AU), Switzerland (CH).
#
# The ratio is the average income (wealth) of the top 10%
# divided by that of the bottom 50%:
#   ratio = (share_top10 / 0.10) / (share_bottom50 / 0.50)
# WID publishes this directly (indicator type "r"). We download
# both the published ratio and the two shares, use the published
# ratio where it exists, and check that the two agree.
#
# Output:
#   Raw_Data/WID/WID_<ISO>_<income|wealth>.csv   (6 raw files)
#   Model_Outputs/wid_inequality.csv            (cleaned, used by plotting)
# ============================================================
# ============================================================

# ---- One-off install (uncomment the first time) ----
# install.packages("remotes")
# remotes::install_github("world-inequality-database/wid-r-tool")

library(wid)
library(tidyverse)

# ---- Paths ----
# Same convention as 5a: Raw_Data is a sibling of Processed_Data.
wid_raw_dir <- file.path(dirname(data_path), "Raw_Data", "WID")
dir.create(wid_raw_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Set REFRESH_WID <- TRUE to re-download from WID.world ----
# FALSE re-uses the raw CSVs already in Raw_Data/WID, so the
# cleaned file can be rebuilt offline and always from the same vintage.
# Honours DOWNLOAD_WID from R/00_config.R when sourced through run_all.R;
# falls back to the original default (TRUE) if run standalone.
# NOTE: the default via config is FALSE, which differs from the original
# script's TRUE. A replication package should rebuild from the shipped
# vintage rather than silently pick up a newer WID release.
REFRESH_WID <- if (exists("DOWNLOAD_WID")) isTRUE(DOWNLOAD_WID) else TRUE

# ---- Specification ----
wid_countries <- c(DE = "Germany", AU = "Australia", CH = "Switzerland")
wid_concepts  <- c(ptinc = "income", hweal = "wealth")

# Download a wide window; the plotting script trims to each panel's years.
wid_download_years <- 1980:2023

# Each panel's life-satisfaction coverage. The first year is the
# index base (= 100), matching the base years in plot_specs.
wid_panel_windows <- tribble(
  ~country,      ~first_year, ~last_year,
  "Germany",     1984,        2019,
  "Australia",   2001,        2019,
  "Switzerland", 2000,        2019
)


# ============================================================
# STEP 1: DOWNLOAD AND SAVE RAW CSVs (one per country x concept)
# ============================================================

download_wid_series <- function(iso, concept) {

  # (a) published Top 10 / Bottom 50 ratio, with extrapolations
  ratio_all <- download_wid(
    indicators = paste0("r", concept), areas = iso,
    years = wid_download_years, ages = "992", pop = "j",
    include_extrapolations = TRUE
  )

  # (b) the same, excluding WID's interpolations/extrapolations --
  #     used only to flag which years are estimates on thin data
  ratio_core <- download_wid(
    indicators = paste0("r", concept), areas = iso,
    years = wid_download_years, ages = "992", pop = "j",
    include_extrapolations = FALSE
  )

  # (c) top 10% and bottom 50% shares, to rebuild and check the ratio
  shares <- download_wid(
    indicators = paste0("s", concept), areas = iso,
    perc = c("p90p100", "p0p50"),
    years = wid_download_years, ages = "992", pop = "j",
    include_extrapolations = TRUE
  )

  # download_wid returns NULL (with a warning) when nothing matches
  tag <- function(df, label) {
    if (is.null(df)) return(NULL)
    as_tibble(df) %>% mutate(download = label)
  }

  raw <- bind_rows(
    tag(ratio_all,  "ratio_with_extrapolations"),
    tag(ratio_core, "ratio_no_extrapolations"),
    tag(shares,     "shares_with_extrapolations")
  )

  if (nrow(raw) == 0) {
    stop("WID returned no data for ", iso, " / ", concept, " -- check the codes.")
  }

  raw
}

if (REFRESH_WID) {
  for (iso in names(wid_countries)) {
    for (concept in names(wid_concepts)) {
      raw <- download_wid_series(iso, concept)
      out_file <- file.path(wid_raw_dir,
                            paste0("WID_", iso, "_", wid_concepts[[concept]], ".csv"))
      write_csv(raw, out_file)
      cat("Saved raw:", basename(out_file), "-", nrow(raw), "rows\n")
    }
  }

  # Record when the vintage was downloaded (WID revises every year)
  writeLines(paste("WID downloaded:", Sys.time()),
             file.path(wid_raw_dir, "WID_download_date.txt"))
}


# ============================================================
# STEP 2: CLEAN -- one row per country x concept x year
# ============================================================

read_wid_raw <- function(iso, concept) {
  read_csv(file.path(wid_raw_dir,
                     paste0("WID_", iso, "_", wid_concepts[[concept]], ".csv")),
           show_col_types = FALSE) %>%
    mutate(iso = iso, concept = wid_concepts[[concept]])
}

wid_raw <- map_dfr(names(wid_countries), function(iso) {
  map_dfr(names(wid_concepts), function(concept) read_wid_raw(iso, concept))
})

# Published ratio (with extrapolations). WID codes non-distributional
# series as p0p100; keep whatever single percentile it returns.
wid_ratio <- wid_raw %>%
  filter(download == "ratio_with_extrapolations") %>%
  group_by(iso, concept, year) %>%
  summarise(ratio_published = first(value), .groups = "drop")

# Years that survive without WID's interpolations/extrapolations
wid_core_years <- wid_raw %>%
  filter(download == "ratio_no_extrapolations") %>%
  distinct(iso, concept, year) %>%
  mutate(is_core_estimate = TRUE)

# Shares -> rebuilt ratio
wid_shares <- wid_raw %>%
  filter(download == "shares_with_extrapolations") %>%
  group_by(iso, concept, year) %>%
  summarise(
    share_top10    = value[percentile == "p90p100"][1],
    share_bottom50 = value[percentile == "p0p50"][1],
    .groups = "drop"
  ) %>%
  mutate(ratio_from_shares = (share_top10 / 0.10) / (share_bottom50 / 0.50))

wid_clean <- full_join(wid_ratio, wid_shares, by = c("iso", "concept", "year")) %>%
  left_join(wid_core_years, by = c("iso", "concept", "year")) %>%
  mutate(
    country          = unname(wid_countries[iso]),
    is_core_estimate = coalesce(is_core_estimate, FALSE),
    # Published ratio where available, otherwise rebuilt from shares
    t10b50_ratio     = coalesce(ratio_published, ratio_from_shares),
    ratio_source     = if_else(!is.na(ratio_published), "WID published", "Rebuilt from shares")
  )


# ============================================================
# STEP 3: CHECKS
# ============================================================

# (i) Published and rebuilt ratios should agree (rounding only)
ratio_check <- wid_clean %>%
  filter(!is.na(ratio_published), !is.na(ratio_from_shares)) %>%
  mutate(rel_diff = abs(ratio_published - ratio_from_shares) / ratio_published) %>%
  group_by(country, concept) %>%
  summarise(max_rel_diff = max(rel_diff), .groups = "drop")

cat("\nPublished vs rebuilt Top10/Bottom50 ratio (max relative difference):\n")
print(ratio_check)
if (any(ratio_check$max_rel_diff > 0.01)) {
  warning("Published and rebuilt ratios differ by >1% somewhere -- inspect before using.")
}

# (ii) A bottom-50 wealth share at or below zero makes the ratio meaningless
bad_bottom <- wid_clean %>% filter(!is.na(share_bottom50), share_bottom50 <= 0)
if (nrow(bad_bottom) > 0) {
  warning("Non-positive bottom-50 share in ", nrow(bad_bottom), " rows -- ratio undefined there.")
  print(bad_bottom)
}

# (iii) Full coverage of each panel window
coverage <- wid_clean %>%
  inner_join(wid_panel_windows, by = "country") %>%
  filter(year >= first_year, year <= last_year, !is.na(t10b50_ratio)) %>%
  group_by(country, concept, first_year, last_year) %>%
  summarise(n_years = n(), share_core = mean(is_core_estimate), .groups = "drop") %>%
  mutate(expected = last_year - first_year + 1)

cat("\nCoverage within each panel window (share_core = share of years not interpolated/extrapolated):\n")
print(coverage)
if (any(coverage$n_years < coverage$expected)) {
  warning("Missing WID years inside a panel window -- see coverage table.")
}


# ============================================================
# STEP 4: INDEX TO EACH PANEL'S FIRST YEAR AND SAVE
# ============================================================

wid_inequality <- wid_clean %>%
  left_join(wid_panel_windows, by = "country") %>%
  group_by(country, concept) %>%
  mutate(
    in_panel_window = year >= first_year & year <= last_year,
    base_value      = t10b50_ratio[year == first_year][1],
    indexed_ratio   = 100 * t10b50_ratio / base_value
  ) %>%
  ungroup() %>%
  arrange(country, concept, year) %>%
  select(country, iso, concept, year, in_panel_window,
         t10b50_ratio, indexed_ratio, ratio_source, is_core_estimate,
         ratio_published, ratio_from_shares, share_top10, share_bottom50)

write_csv(wid_inequality, file.path(modeloutput_path, "wid_inequality.csv"))

cat("\n✓ Saved wid_inequality.csv -", nrow(wid_inequality), "rows\n")
