# data/Raw_Data

Raw survey downloads and benchmark series. Must stay a sibling of
`Processed_Data/`: the scripts locate this folder as
`file.path(dirname(data_path), "Raw_Data", ...)`.

## Shipped

```
WID/WID_DE_income.csv   WID/WID_DE_wealth.csv
WID/WID_AU_income.csv   WID/WID_AU_wealth.csv
WID/WID_CH_income.csv   WID/WID_CH_wealth.csv
swiss_vote_shares.csv
```

World Inequality Database Top 10 / Bottom 50 ratios, as downloaded for the
paper, plus Swiss National Council election results from the Federal
Statistical Office. `R/10_wid_cleaning.R` re-uses the WID CSVs by default so
the paper's numbers reproduce; set `DOWNLOAD_WID <- TRUE` in
`R/00_config.R` to refresh them from the live series instead.

## You must add

The Chapel Hill Expert Survey requires registration, so it is not
redistributed. Download from <https://www.chesdata.eu> and place here:

```
1999-2024_CHES_dataset_meansV2.dta
2010_CHES_dataset_means.dta
2014_CHES_dataset_means-2.dta
CHES2019V3.dta
```

Without these, `R/05_elite_polarisation.R` will fail and Figure 12 cannot be
built. Everything else runs.

## Survey microdata

The raw SOEP, HILDA, SHP and BES downloads also belong here, in the layout
the do-files expect (`soep/pgen.dta`, `soep/hgen.dta`, `soep/hl.dta`,
`soep/pbrutto.dta`, `shplong_p_user.dta`, the BES panel file, and so on).
See the top-level README.
