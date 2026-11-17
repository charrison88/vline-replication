# ============================================================================
# run_all.R — the whole R side of the V-LINE pipeline, in order
#
# Prerequisites
#   1. Stata steps have been run (see main.do), so data/Processed_Data holds
#      the panel .dta files. Not needed if you only want the figures and
#      tables from the shipped CSVs — see "Figures only" below.
#   2. Working directory is the repository root. Open vline-replication.Rproj
#      or setwd() there first.
#
# Usage
#   source("run_all.R")                      # everything
#   VLINE_STEPS <- c("figures"); source("run_all.R")   # figures only
#
# R 4.4.1. Package versions are pinned in README.md.
# ============================================================================

source("R/00_config.R")

# Which steps to run. Edit, or set VLINE_STEPS before sourcing this file.
# "figures" and "tables" need only the CSVs in data/model_outputs, which are
# shipped with the repository. Every other step needs the panel .dta files.
if (!exists("VLINE_STEPS")) {
  VLINE_STEPS <- c(
    "load",            # 01  read and clean the four panels
    "volatility",      # 02  within-person volatility models
    "vline",           # 03  V-LINE adjustments
    "income",          # 04  income proof of concept
    "elite",           # 05  CHES elite polarisation
    "nonlinearity",    # 06  robustness: non-linear reporting
    "homoskedasticity",# 07  robustness: Assumption 4
    "placebo",         # 08  health placebo, distributions, nominal SD
    "tables",          # 09  LaTeX table bodies
    "wid",             # 10  World Inequality Database series
    "figures"          # 11  all figures, in paper order
  )
}

.vline_script <- c(
  load             = "R/01_load_clean.R",
  volatility       = "R/02_volatility_models.R",
  vline            = "R/03_vline.R",
  income           = "R/04_income_validation.R",
  elite            = "R/05_elite_polarisation.R",
  nonlinearity     = "R/06_robustness_nonlinearity.R",
  homoskedasticity = "R/07_robustness_homoskedasticity.R",
  placebo          = "R/08_placebo_distributions.R",
  tables           = "R/09_tables.R",
  wid              = "R/10_wid_cleaning.R",
  figures          = "R/11_figures.R"
)

# The model steps assume the panels are present; say so plainly rather than
# failing several hundred lines into a read.
.needs_panels <- setdiff(names(.vline_script), c("tables", "figures"))
if (any(VLINE_STEPS %in% .needs_panels)) {
  if (!length(list.files(data_path, pattern = "\\.dta$"))) {
    stop("No .dta files in ", data_path, "\n",
         "Run the Stata steps first (see main.do and README.md), or run\n",
         'the figures only:  VLINE_STEPS <- c("figures"); source("run_all.R")')
  }
}

.t0 <- Sys.time()
for (step in VLINE_STEPS) {
  f <- .vline_script[[step]]
  cat("\n\n############################################################\n")
  cat("# ", step, "  ->  ", f, "\n", sep = "")
  cat("############################################################\n")
  source(f, echo = FALSE)
}

cat("\n\nPipeline finished in ",
    round(as.numeric(difftime(Sys.time(), .t0, units = "mins")), 1),
    " minutes.\n", sep = "")
cat("Figures: output/Figures_for_Publication and output/Figures_for_Slides\n")
cat("Tables:  output/Tables_091126\n")
