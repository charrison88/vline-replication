
* Set global paths
global data "/Users/charlieharrison/Google Drive/My Drive/Stretching_Volatility/Processed_Data"
global rawdata "/Users/charlieharrison/Google Drive/My Drive/Stretching_Volatility/Raw_Data"

* Load the data
import delimited "${rawdata}/gdp-vs-happiness.csv", clear

* Keep only 2020 data
keep if year == 2020

* Drop missing values
drop if missing(gdppercapita) | missing(lifesatisfaction) | missing(code)

* Generate log GDP
gen log_gdp = ln(gdppercapita)

* Calculate correlation between log(GDP) and life satisfaction
corr lifesatisfaction log_gdp
local corr = string(r(rho), "%4.3f")

* Fit regression in log space and generate predicted values
quietly reg lifesatisfaction log_gdp
predict yhat

* Select subset of countries to label (every 2nd country when sorted by GDP)
sort gdppercapita
gen row = _n
gen label_country = code if mod(row, 2) == 1


// twoway (scatter lifesatisfaction gdppercapita, ///
//         msymbol(O) msize(vsmall) mcolor(gs8) ///
//         mlabel(label_country) mlabsize(vsmall) mlabcolor(gs8) mlabposition(0)) ///
//        (line yhat gdppercapita, ///
//         sort lcolor(gs8) lpattern(dash) lwidth(small)), ///
//        xscale(log) ///
//        xlabel(1000 "$1k" 5000 "$5k" 25000 "$25k" 125000 "$125k", ///
//               labsize(m) nogrid) ///
//        ylabel(, labsize(m) angle(0) nogrid) ///
//        xtitle("GDP per capita (international-$)", size(medium)) ///
//        ytitle("Life Satisfaction (0-10)", size(medium)) ///
//        legend(off) ///
// 	   plotregion(lcolor(black) lwidth(thin)) ///
//        note("Correlation=`corr'. Source: Wellbeing Research Centre (2025). Labels for 1/2 of countries. Year 2020", ///
//             size(small) pos(6))
	
	
twoway (scatter lifesatisfaction gdppercapita, ///
        msymbol(O) msize(vsmall) mcolor(gs8) ///
        mlabel(label_country) mlabsize(vsmall) mlabcolor(black) mlabposition(12) mlabgap(*1.5)) ///
       (line yhat gdppercapita, ///
        sort lcolor(gs8) lpattern(dash) lwidth(small)), ///
       xscale(log) ///
       xlabel(1000 "$1k" 5000 "$5k" 25000 "$25k" 125000 "$125k", ///
              labsize(m) nogrid) ///
       ylabel(, labsize(m) angle(0) nogrid) ///
       xtitle("GDP per capita (international-$)", size(medium)) ///
       ytitle("Life Satisfaction (0-10)", size(medium)) ///
       legend(off) ///
       plotregion(lcolor(black) lwidth(thin)) ///
       note("Correlation=`corr'. Source: Wellbeing Research Centre (2025). Labels for 1/2 of countries. Year 2020", ///
            size(small) pos(6))
