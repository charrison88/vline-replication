********************************************************************************
* Volatility and Stretching
* File: Swiss_volatility_get
* Author: Charlie Harrison
* Date: December 12th
********************************************************************************
* SCRIPT OVERVIEW:
* Section 0: Set paths
* Section 1: Build CPI deflator (OWID, 2010 = 100)
* Section 2: Build CPI-deflated household income (shplong_h_user)
* Section 3: Clean individual-level covariates and merge in household income
* Section 4: Label variables and save
********************************************************************************


*=====================================
*	0. Set Paths
*=====================================

clear all
set more off

* Set paths
global data "/Users/charlieharrison/Google Drive/My Drive/Stretching_Volatility/Processed_Data"
global rawdata "/Users/charlieharrison/Google Drive/My Drive/Stretching_Volatility/Raw_Data"


*=====================================
*	1. CPI Deflator (OWID, 2010 = 100)
*=====================================

* From: https://ourworldindata.org/grapher/consumer-price-index?time=1999..2024&country=~CHE

clear
input year cpi
1999 90.34148
2000 91.74947
2001 92.6569
2002 93.25241
2003 93.84762
2004 94.60113
2005 95.70981
2006 96.72386
2007 97.43222
2008 99.79597
2009 99.31647
2010 100
2011 100.231346
2012 99.53719
2013 99.32088
2014 99.30776
2015 98.171776
2016 97.7451
2017 98.26685
2018 99.18696
2019 99.5469
2020 98.82431
2021 99.399284
2022 102.217285
2023 104.40003
2024 105.50912
end

label var cpi "Swiss CPI, 2010=100 (Our World in Data)"
save "${rawdata}/cpi_switzerland.dta", replace


*=====================================
*	2. Household Income (CPI-deflated)
*=====================================

use "${rawdata}/shplong_h_user.dta", clear

* Confirmed present: idhous (obs 142,623, min 41, max 1.64e+07) and
* year (1999-2023), matching the person file — see summ output above.
keep idhous year ihtyn ihtyg idispy ieqsg ieqsn ieqog ieqon

* One row per household-year expected — if this isn't 0 duplicates, stop
* and figure out why before merging anything onto it.
duplicates report idhous year

* Many household-year rows -> one CPI-year row.
merge m:1 year using "${rawdata}/cpi_switzerland.dta"
tab _merge
* Expect _merge==3 for every row (CPI table spans 1999-2024, matching
* the panel). Any _merge==1 means a year outside the CPI table —
* check `list idhous year if _merge==1` if that happens.
drop if _merge == 2
drop _merge

replace idispy = . if inlist(idispy, -1, -2, -3, -4, -5, -6, -7, -8)

gen hh_income_disp_real  = idispy * (100 / cpi)

label var hh_income_disp_real  "Disposable household income, CPI-deflated (2010 CHF)"

gen hh_income = hh_income_disp_real
label var hh_income "Disposable household income, CPI-deflated (2010 CHF) — chosen series"

tempfile hh_deflated
save `hh_deflated'


*=====================================
*	3. Clean Individual Covariates + Merge Household Income
*=====================================

* Load data
use "${rawdata}/shplong_p_user.dta", clear

* Rename ID
rename idpers pid

* Merge in CPI-deflated household income built in Section 2
merge m:1 idhous year using `hh_deflated'
tab _merge
* Expect close to 100% _merge==3. _merge==1 (person, no household match)
* can happen for proxy-only or incomplete household records — decide
* deliberately whether to drop those rather than let them ride along
* with missing income.
drop if _merge == 2
drop _merge

* Political attitudes (0-10 scales)
* ============================================================

* Left-right self-placement
gen lrself = pp10
replace lrself = . if lrself < 0 | lrself > 10

* Left-right father
gen lrfather = pp46
replace lrfather = . if lrfather < 0 | lrfather > 10

* Left-right mother
gen lrmother = pp47
replace lrmother = . if lrmother < 0 | lrmother > 10

* Political interest
gen pol_interest = pp01
replace pol_interest = . if pol_interest < 0

* Trust in government
gen trust_gov = pp04
replace trust_gov = . if trust_gov < 0 | trust_gov > 10

* Satisfaction variables (0-10 scales)
* ============================================================

* Life satisfaction
gen sat_life = pc44
replace sat_life = . if sat_life < 0 | sat_life > 10

* Job satisfaction
gen sat_job = pw228
replace sat_job = . if sat_job < 0 | sat_job > 10

* Health satisfaction
gen sat_health = pc02
replace sat_health = . if sat_health < 0 | sat_health > 10

* Partner satisfaction
gen sat_partner = pf54
replace sat_partner = . if sat_partner < 0 | sat_partner > 10

* Relationship satisfaction
gen sat_relationships = pql04
replace sat_relationships = . if sat_relationships < 0 | sat_relationships > 10

* Free time satisfaction
gen sat_freetime = pa05
replace sat_freetime = . if sat_freetime < 0 | sat_freetime > 10

* Leisure satisfaction
gen sat_leisure = pa06
replace sat_leisure = . if sat_leisure < 0 | sat_leisure > 10

* Financial satisfaction
gen sat_finances = pi01
replace sat_finances = . if sat_finances < 0 | sat_finances > 10

* Other variables
* ============================================================

* Health status (reverse coded to 1-5, higher = better)
gen health_status = pc01
replace health_status = . if health_status < 0
replace health_status = (health_status * -1) + 6

* Satisfaction with democracy
gen sat_democracy = pp02
replace sat_democracy = . if sat_democracy < 0 | sat_democracy > 10

* Political influence
gen political_influence = pp03
replace political_influence = . if political_influence < 0

* General trust
gen general_trust = pp45
replace general_trust = . if general_trust < 0

* Risk attitude
gen risk_attitude = pp48
replace risk_attitude = . if risk_attitude < 0 | risk_attitude > 10

* Living situation satisfaction
gen living_sit_sat = pf03
replace living_sit_sat = . if living_sit_sat < 0 | living_sit_sat > 10

* Controls
* ============================================================

* Migration background: 1 = not born in Switzerland, 0 = born in Switzerland
gen migback_collapsed = .
replace migback_collapsed = 1 if pd160 == 2
replace migback_collapsed = 0 if pd160 == 1

* Normalised weight
gen norm_weight = wicss
replace norm_weight = 1 if norm_weight == . | norm_weight <= 0


*=====================================
*	4. Label Variables and Save
*=====================================

save "${data}/Switzerland.dta", replace

* Summary
describe lrself sat_life migback_collapsed norm_weight hh_income
sum lrself sat_life migback_collapsed norm_weight hh_income
