# V-LINE

Replication code for **"Why surveys can hide rising inequality (and how to
correct for it)"**, Charlie Harrison and Alberto Prati.

Dispersion statistics computed from self-reported ordinal scales are not
comparable over time if respondents' use of the scale changes. If the scale
stretches — if the subjective distance between adjacent response categories
grows — then a genuine rise in the dispersion of the underlying quantity can
show up as a fall in the dispersion of reported answers.

The **V-LINE** (volatility-corrected LINE) estimates how far a scale has
stretched, using within-person volatility in panel data, and rescales
dispersion estimates accordingly. This repository applies it to life
satisfaction (SOEP, HILDA, SHP), left-right self-placement (BES, SHP), and
household income, where the latent quantity is observed and the method can be
validated against it.

The paper is a working draft. There is no citation or DOI yet.

## Contents

```
main.do                  Stata: build analysis panels from raw survey files
run_all.R                R: estimation, tables and figures
R/                       00_config.R, then 01-11 in run order
stata/                   seven data-building do-files
data/model_outputs/      15 CSVs of estimates (included)
data/Processed_Data/     analysis panels (not included; see Data)
data/Raw_Data/           survey and benchmark inputs (partly included)
output/                  figures and LaTeX table bodies, generated
FIGURES.md               which file is which figure
NOT_USED.md              figures the code can produce that the paper omits
```

## Running it

### Figures and tables only

The estimates the figures and tables are built from are included, so this
path needs neither Stata nor the survey microdata. It takes under a minute.

```r
# open vline-replication.Rproj, then
VLINE_STEPS <- c("figures")
source("run_all.R")
```

Add `"tables"` to rebuild the LaTeX table bodies as well.

### The full pipeline

Requires the four survey panels (see Data), the Chapel Hill Expert Survey,
and Stata 19.

```
do main.do            # Stata, from the repository root
```
```r
source("run_all.R")   # R
```

Output goes to `output/Figures_for_Publication/`,
`output/Figures_for_Slides/` and `output/Tables_091126/`. The folder names
match those the manuscript's LaTeX project expects, so the tree can be copied
over it directly.

Re-running the full pipeline rewrites the CSVs in `data/model_outputs/` in
place. This is deliberate: `git diff` then shows whether a re-run reproduces
the included estimates.

## Software

| | |
|---|---|
| Stata | 19 MP |
| R | 4.4.1 (2024-06-14) |

R packages, at the versions used for the results reported in the paper:

| | | | | |
|---|---|---|---|---|
| tidyverse | 2.0.0 | | survey | 4.4.2 |
| dplyr | 1.1.4 | | srvyr | 1.3.0 |
| ggplot2 | 3.5.2 | | plm | 2.6.4 |
| readr | 2.1.5 | | broom | 1.0.7 |
| tidyr | 1.3.1 | | emmeans | 1.11.1 |
| purrr | 1.0.4 | | gt | 0.11.1 |
| haven | 2.5.4 | | knitr | 1.48 |
| zoo | 1.8.12 | | kableExtra | 1.4.0 |
| data.table | 1.17.8 | | webshot2 | 0.1.2 |
| scales | 1.4.0 | | BMisc | 1.4.6 |
| patchwork | 1.3.0 | | wid | 0.0.1 |

`ggplot2` 3.5.2 is the one version worth matching. The figures use secondary
axes and `patchwork` compositions whose behaviour has changed across minor
releases.

`wid` is required only when `DOWNLOAD_WID` is set, and is not on CRAN:
install from <https://github.com/WIDworld/wid-r-tool>.

## Data

### Analysis panels — not included

`data/Processed_Data/` is empty. Individual-level derived files from these
surveys cannot be redistributed under their data agreements, and several
exceed GitHub's file-size limit. Each panel must be obtained from its
provider:

| Panel | Source |
|---|---|
| SOEP (Germany) | DIW Berlin, <https://www.diw.de/soep> |
| HILDA (Australia) | Melbourne Institute, via the ADA Dataverse |
| SHP (Switzerland) | FORS, <https://forscenter.ch> |
| BES Internet Panel (UK) | <https://www.britishelectionstudy.com> |

`main.do` builds the analysis panels from the raw files.

### Benchmark inputs — partly included

`data/Raw_Data/` holds the public benchmark series:

- `WID/WID_<ISO>_<income|wealth>.csv` — World Inequality Database top 10 to
  bottom 50 ratios for Germany, Australia and Switzerland, at the vintage
  used in the paper. `R/10_wid_cleaning.R` reads these by default, so results
  reproduce exactly; set `DOWNLOAD_WID <- TRUE` in `R/00_config.R` to fetch
  the current series instead.
- `swiss_vote_shares.csv` — National Council election results from the Swiss
  Federal Statistical Office, used to weight party positions.

The Chapel Hill Expert Survey is **not included**: it requires registration.
Download from <https://www.chesdata.eu> and place in `data/Raw_Data/`:

```
1999-2024_CHES_dataset_meansV2.dta
2010_CHES_dataset_means.dta
2014_CHES_dataset_means-2.dta
CHES2019V3.dta
```

Without these, `R/05_elite_polarisation.R` cannot run. Everything else does,
including the figures-only path, because the elite-polarisation estimates it
produces are included.

### Estimates — included

`data/model_outputs/` holds 15 CSVs totalling under 500 KB: volatility
coefficients by model and period, V-LINE adjustments, the income validation
series, elite polarisation, World Inequality Database series, and response
distributions. These are aggregates, not microdata, and are included so that
every figure and table can be rebuilt without first obtaining four
restricted datasets.

## Configuration

All paths and switches live in `R/00_config.R`; no other R file contains a
path. It must be sourced with the repository root as the working directory,
which opening `vline-replication.Rproj` arranges.

| Switch | Default | Effect |
|---|---|---|
| `GENERATE_UNUSED` | `FALSE` | Also produce figures the paper does not use (see NOT_USED.md) |
| `DOWNLOAD_WID` | `FALSE` | Fetch World Inequality Database series instead of reading the included CSVs |

`data/Processed_Data/` and `data/Raw_Data/` must stay siblings:
`R/05_elite_polarisation.R` and `R/10_wid_cleaning.R` locate raw inputs
relative to the panel directory.

## Code layout

| Script | What it does |
|---|---|
| `00_config.R` | Paths, packages, figure-saving helper |
| `01_load_clean.R` | Read and clean the four panels |
| `02_volatility_models.R` | Within-person volatility, Models 1–8 |
| `03_vline.R` | V-LINE rescaling adjustments |
| `04_income_validation.R` | Income proof of concept |
| `05_elite_polarisation.R` | Party-system polarisation from CHES |
| `06_robustness_nonlinearity.R` | Non-linear reporting functions |
| `07_robustness_homoskedasticity.R` | Sensitivity to Assumption 4 |
| `08_placebo_distributions.R` | Health placebo, response distributions |
| `09_tables.R` | LaTeX table bodies |
| `10_wid_cleaning.R` | World Inequality Database series |
| `11_figures.R` | All figures |

`11_figures.R` builds every plot first and writes them in a single block
afterwards, ordered so that the five numbered figures of the main text come
first, in the order the text presents them. Appendix and slide figures follow,
grouped but unnumbered, since appendix numbering shifts whenever a float is
added.

## Contact

Charlie Harrison — <c.t.harrison@lse.ac.uk>

## Licence

No licence is granted. All rights reserved by the authors. The code is
published so that the results in the paper can be inspected and checked; any
other use requires permission. Survey microdata remains governed by each
provider's own agreement.
