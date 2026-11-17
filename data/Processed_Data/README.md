# data/Processed_Data

Empty on purpose. `main.do` writes the panel `.dta` files here.

These are individual-level derived files from SOEP, HILDA, SHP and BES. They
are not redistributable under those data agreements, and four of them exceed
GitHub's 100 MB per-file limit in the authors' copy (`Switzerland.dta` is
492 MB). `.gitignore` excludes everything in this folder except this file.

See the Data section of the top-level README for where to obtain each panel.

Expected contents after a successful `main.do` run:

```
Germany.dta        Australia.dta      Switzerland.dta
bes_elections.dta  bes_yearly.dta     bes_lr_allwaves.dta
```

These six are exactly what the R scripts read; `main.do` also leaves some
intermediate files here, which nothing downstream uses.

`R/00_config.R` points `data_path` here, and
`R/05_elite_polarisation.R` and `R/10_wid_cleaning.R` locate raw inputs as
`file.path(dirname(data_path), "Raw_Data", ...)`. That is why `Raw_Data` must
stay a sibling of this folder — do not move either one.
