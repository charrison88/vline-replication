# Figures not used in the paper

`R/11_figures.R` can produce about thirty figures that appear neither in the
manuscript nor in any presentation deck. They are retained because most are
alternative cuts of figures that are used — a different index base, a
different axis range, one country instead of three — and are cheap to
regenerate.

They are skipped by default. To produce them, set `GENERATE_UNUSED <- TRUE`
in `R/00_config.R`.

## Volatility, specifications not plotted

The paper plots Models 6 and 7 in Appendix A and reports the remaining
specifications in the trend tables instead.

- `LS_overall_no_controls.png`, `LS_overall_demographic_controls.png`, `LS_overall_panel_tenure_linear.png`, `LS_overall_panel_tenure_factor.png`, `LS_overall_panel_tenure_factor_demographic.png`
- `POL_overall_no_controls.png`, `POL_overall_demographic_controls.png`, `POL_overall_panel_tenure_linear.png`, `POL_overall_panel_tenure_factor.png`, `POL_overall_panel_tenure_factor_demographic.png`

## Volatility overlay

Plots the uncontrolled and preferred volatility series together. The main
figures show the same comparison alongside nominal dispersion.

- `LS_volatility_nc_vs_model5.png`, `POL_volatility_nc_vs_model5.png`

## Nominal dispersion alone

- `nominal_between_natural_combined.png`, `nominal_political_combined_SD.png`

## Income category volatility without the stretching parameter

Figure 1(b) overlays the stretching parameter on the same series.

- `inc_cat_volatility_only.png`

## Paradox figures

An earlier presentation of the central comparison, pairing nominal and
adjusted dispersion against an external inequality or polarisation benchmark.
Fifteen files.

- `paradox_lsat_plot1_model5_wid_income.png`, `paradox_lsat_plot1_model5_nc_wid_income.png`, `paradox_lsat_plot2_latent_vs_wid_income.png`
- `paradox_political_plot1_model5_{lrgen,all3,econ_cult}.png`
- `paradox_political_plot1_model5_nc_{lrgen,all3,econ_cult}.png`
- `paradox_political_plot2_latent_vs_elite_{lrgen,all3,econ_cult}.png`

The related `paradox_political_uk_elite_economic.png` and
`paradox_political_uk_elite_cultural.png` *are* used in the decks, so the
loop producing that family runs by default.

## Alternative cuts of figures that are used

The period-grouping sensitivity figure in Appendix A plots dispersion; this is
the same comparison for the mean. The transformation curves in Appendix E are
arranged by shape; this is the same curves arranged by cost level.

- `comparison_mean_groupings.png`
- `nonlinearity_transformations_by_cost.png`

## Alternative cuts of the Appendix B figures

Appendix B uses the levels version of the inequality panels and the indexed
version of the polarisation panels. These are the other combinations.

- `wid_2a_separate_panels_indexed.png`
- `elite_E1_lrgen_indexed.png`, `elite_E1_lrgen_levels.png`
- `elite_E2_all3_same_panel_indexed.png`, `elite_E2_all3_same_panel_levels.png`
- `elite_E3_separate_panels_levels.png`

## Swiss mass polarisation, full panel

Swiss self-placement from 1999, beyond the 2010 start of Swiss expert-survey
coverage. Discussed in the text but not plotted.

- `swiss_alltime_political.png`

## Slide-family members with no current slide

These come from loops that otherwise produce figures the decks use. The loops
run whole rather than being split to exclude one member, so these are written
by default.

- `TS_Model_2_Demographic_ger_life.png`
- `POL_Model_2_Demographic_uk.png`, `POL_Model_6_NewRespondents_uk.png`, `POL_Model_7_BalancedPanel_uk.png`
- `WID_{GER,AUS,CHE}_2a_income_wealth_{levels,indexed}.png` — only some are used
- `paradox_political_che_elite_{lrgen,economic,cultural}.png`, `paradox_political_uk_elite_lrgen.png`

Behind the switch:

- `Rescaling_Natural_AUS_Mean.png`, `Nominal_GER_Mean_LS.png`, `Nonlinearity_Summary_SD_Australia.png`

## Estimates produced but not read

Two of the 15 CSVs in `data/model_outputs/` are written by the pipeline and
read by nothing. They are included regardless, at a few kilobytes each.

- `income_gini.csv` — a within-panel Gini coefficient, superseded by the
  World Inequality Database top 10 to bottom 50 ratios
- `nominal_sd.csv` — nominal standard deviations, which the figures instead
  take from the `sd_level` column of the V-LINE output files
