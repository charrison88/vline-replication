# ============================================================================
# 00_config.R — paths, packages and the figure-saving helper
#
# Everything downstream reads its paths from here. Nothing else in the
# repository contains an absolute path. Source this first, always via
# run_all.R, with the repository root as the working directory.
# ============================================================================

# ---------------------------------------------------------------------------
# Repository root
# ---------------------------------------------------------------------------
# Resolved from the working directory. The check below fails loudly rather
# than letting a wrong working directory produce a half-written output tree.

VLINE_ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

if (!file.exists(file.path(VLINE_ROOT, "R", "00_config.R"))) {
  stop(
    "Working directory is not the repository root.\n",
    "  Current: ", VLINE_ROOT, "\n",
    "  Expected a directory containing R/00_config.R.\n",
    "  In RStudio, open vline-replication.Rproj, or run setwd() first."
  )
}

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
# data_path must stay a sibling of Raw_Data: 05_elite_polarisation.R and
# 10_wid_cleaning.R both locate raw inputs as file.path(dirname(data_path),
# "Raw_Data", ...). That is the original convention and is left untouched.

data_path        <- file.path(VLINE_ROOT, "data", "Processed_Data")
rawdata_path     <- file.path(VLINE_ROOT, "data", "Raw_Data")
modeloutput_path <- file.path(VLINE_ROOT, "data", "model_outputs")

Tables_dir              <- file.path(VLINE_ROOT, "output", "Tables_091126")
Figures_for_Publication <- file.path(VLINE_ROOT, "output", "Figures_for_Publication")
Figures_for_Slides      <- file.path(VLINE_ROOT, "output", "Figures_for_Slides")

for (d in c(modeloutput_path, Tables_dir, Figures_for_Publication, Figures_for_Slides)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ---------------------------------------------------------------------------
# Switches
# ---------------------------------------------------------------------------
# GENERATE_UNUSED: the plotting script can produce roughly thirty figures
# that appear in neither the paper nor any slide deck (see NOT_USED.md).
# They are skipped by default. Set TRUE to produce them as well.

GENERATE_UNUSED <- FALSE

# DOWNLOAD_WID: TRUE re-downloads World Inequality Database series through
# the wid package, which needs network access and overwrites the CSVs in
# data/Raw_Data/WID. FALSE re-uses the shipped CSVs. Default FALSE so a
# clean clone reproduces the paper's numbers rather than a later vintage.

DOWNLOAD_WID <- FALSE

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------
# Versions are those used for the results in the paper; see README.md.
# Other versions will probably work, but have not been checked.

.vline_packages <- c(
  "tidyverse", "dplyr", "ggplot2", "readr", "tidyr", "purrr",
  "haven", "zoo", "data.table", "scales", "patchwork",
  "survey", "srvyr", "plm", "broom", "emmeans",
  "gt", "knitr", "kableExtra", "webshot2", "BMisc"
)

.missing <- .vline_packages[!vapply(.vline_packages, requireNamespace,
                                    logical(1), quietly = TRUE)]
if (length(.missing)) {
  stop("Missing packages: ", paste(.missing, collapse = ", "),
       "\nInstall them, then re-run. See README.md for pinned versions.")
}
invisible(lapply(.vline_packages, library, character.only = TRUE))

# RStata is needed only to drive the Stata steps from R; main.do is the
# supported route, so RStata is optional and not loaded here.

# ---------------------------------------------------------------------------
# Figure saving
# ---------------------------------------------------------------------------
# Differs from the original safe_save in one respect: the original removed
# the destination file before calling ggsave, so a plot that errored left no
# file at all. This writes to a temporary name and moves it into place only
# on success, so a failed render leaves the previous figure intact.

safe_save <- function(plot, filename, width, height, folder) {
  if (isTRUE(.vline_defer_saves)) return(invisible(NULL))

  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  full_path <- file.path(folder, filename)
  tmp_path  <- file.path(folder, paste0(".tmp_", filename))

  ggplot2::ggsave(filename = tmp_path, plot = plot,
                  width = width, height = height, bg = "white", dpi = 300)

  if (!file.rename(tmp_path, full_path)) {
    file.copy(tmp_path, full_path, overwrite = TRUE)
    unlink(tmp_path)
  }
  cat("Saved:", filename, "->", basename(folder), "\n")
}

# Set TRUE while the figure-construction code runs, so that save calls
# embedded in it do nothing and every write happens in one ordered block.
# 11_figures.R manages this; see the comments there.
.vline_defer_saves <- FALSE

cat("Config loaded. Root:", VLINE_ROOT, "\n")
