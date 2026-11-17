# Figures

`R/11_figures.R` writes the five numbered figures of the main text first, in
the order the text presents them, then the appendix figures grouped by
appendix, then the slide figures. Only the main-text figures carry numbers:
appendix numbering shifts whenever a float is added or removed.

All figures are written to `output/Figures_for_Publication/`, except the
slide figures, which go to `output/Figures_for_Slides/`. Some appear in both.

## Main text

| Figure | Panel | File |
|---|---|---|
| 1 | (a) | `inc_cat_ineq_valid_minus_VLINE.png` |
| 1 | (b) | `inc_cat_stretching.png` |
| 2 | — | `inc_cat_ineq_valid.png` |
| 3 | (a) | `mechanism_lsat_nominal_vs_volatility.png` |
| 3 | (b) | `rescaling_between_natural.png` |
| 4 | (a) | `mechanism_political_nominal_vs_volatility.png` |
| 4 | (b) | `VLINE_political_combined_SD.png` |
| 5 | — | `placebo_health_status_volatility.png` |

Figures 1 and 2 are the income validation exercise; 3 and 4 the two main
applications, each pairing the nominal-versus-volatility mechanism with the
V-LINE-adjusted series; 5 the self-rated health placebo.

## Appendix

**A — Additional figures**

- `LS_overall_new_respondents.png` + `POL_overall_new_respondents.png` — volatility among new respondents only (Model 6)
- `LS_overall_balanced_panel.png` + `POL_overall_balanced_panel.png` — volatility in balanced panels (Model 7)
- `comparison_between_groupings.png` — sensitivity to where the two-year periods start
- `Distribution_FirstLast_LifeSatisfaction.png` + `Distribution_FirstLast_Political.png` — raw response distributions, first against last period

**B — External benchmarks**

- `wid_2a_separate_panels_levels.png` — World Inequality Database series
- `elite_E3_separate_panels_indexed.png` — party-system polarisation

**C — BES with an alternative period definition**

- `POL_Robustness_UK_PreVsSwing.png`

**D — Levels analysis**

- `inc_cat_mean_valid.png` — income, mean validation
- `rescaling_mean_natural.png` — mean life satisfaction, nominal and adjusted
- `COVID_VLINE_Mean_Combined.png`, `COVID_VLINE_SD_Combined.png`

**E — Robustness to linearity violations**

- `nonlinearity_transformations_by_shape.png` — the simulated transformations themselves
- `nonlinearity_summary_sd.png`, `nonlinearity_convex_sd.png`, `nonlinearity_concave_sd.png`, `nonlinearity_inv_s_sd.png` — Germany and Australia
- `nonlinearity_summary_sd_uk.png`, `nonlinearity_convex_sd_uk.png`, `nonlinearity_concave_sd_uk.png`, `nonlinearity_inv_s_sd_uk.png` — Britain

**F — Robustness to smooth rescaling trends**: no figures.

**G — Robustness to violations of homoskedasticity**

- `income_homoskedasticity_appendix.png`

## Slide figures

Produced for the presentation decks rather than the paper, and not numbered.
Single-country cuts of the main figures, per-model volatility series, and
alternative axis ranges. Produced by default; `NOT_USED.md` lists the few
members of each family that no current deck uses.
