********************************************************************************
* Volatility and Stretching
* File: GER_volatility_get
* Author: Charlie Harrison
* Date: November 28th Novemeber
********************************************************************************
* SCRIPT OVERVIEW:
* Section 0: Set paths
* Section 1: Merge raw data - Combines multiple SOEP data files (pgen, hgen, hl, pl, pequiv, ppathl, biol)
* Section 2: Initial preparation of variables - Prepares wave, demographics, employment, household, and education variables
* Section 3: Prepare income variables - Creates equivalized income, CPI-adjusted income, and reference income
* Section 4: More preparations - Finalizes migration, education, tenure, and household size variables
* Section 5: Clean satisfaction variables
* Section 6: (NEW) Calculating survey weights - Normalizes survey weights (phrf) by year
* Section 7: Label variables and save
********************************************************************************

*=====================================
*	0. Set Paths
*=====================================

global data "/Users/charlieharrison/Google Drive/My Drive/Stretching_Volatility/Processed_Data"
global rawdata "/Users/charlieharrison/Google Drive/My Drive/Stretching_Volatility/Raw_Data"

*=====================================
*	1. Merge raw data
*=====================================

use "$rawdata/soep/pgen.dta", replace
keep pid syear hid  ///
pgcasmin /// education
imonth // month of interview (renamed from pgmonth)
save "$data/pgen_small.dta", replace

use "$rawdata/soep/hgen.dta", replace
label language EN

keep hgowner syear hid ///
hgmoveyr // year of moving in (household) 
save "$data/hgen_small.dta", replace


use "$rawdata/soep/hl.dta", replace
keep syear hid ///
h_pnr_v1 h_pnr_v2 // 
save "$data/hl_small.dta", replace


use "$rawdata/soep/pl.dta", replace
keep pid syear hid pnr ///
plh0171 /// health satisfaction (0-10)
plh0173 plj0454 /// job satisfaction
plh0321 /// satisfaction with partnership
plh0180 /// satisfaction with family life
plh0177 /// satisfaction with flat
plh0183 /// satisfaction in 5 years
plh0244 /// optimisim about future
plh0204_v2 // risk aversion
			
save "$data/pl_small.dta", replace

use "$rawdata/soep/pequiv.dta" , replace
keep pid syear hid ///
p11101 /// life satisfaction
d11101 /// age
d11102 /// gender
d11106 /// HH size
d11107 /// number of children
l11101 /// Bundesland
h11103 /// Number of hh members age 0-1
i11102 /// HH Post-Government Income
y11101 /// Consumer Price Index
d11109 /// years of education
m11126 // current self-rated health status
save "$data/pequiv_small.dta", replace



use "$data/pl_small.dta", replace

joinby pid syear using "$data/pgen_small.dta", update unmatched(both) _merge(_mergePGEN)
joinby pid syear using "$data/pequiv_small.dta", update unmatched(both) _merge(_mergePEQUIV)
joinby hid syear using "$data/hgen_small.dta", update unmatched(both) _merge(_mergeHGEN)
joinby hid syear using "$data/hl_small.dta", update unmatched(both) _merge(_mergehl)


// On investigation ... 
gen missing_pid = missing(pid)
drop if missing_pid


// merge 1:1 pid syear using "$rawdata/soep/ppathl.dta", keepusing(migback) nogen // add migration info

merge 1:1 pid syear using "$rawdata/soep/ppathl.dta", keepusing(migback psample phrf todjahr) nogen // add migration AND **sample** info AND survey weights AND year of death (useful for attrition)
merge 1:1 pid syear using "$rawdata/soep/biol.dta", keepusing(lb0285) nogen // add children info


**Clean up**
erase "$data/pgen_small.dta"
erase "$data/pl_small.dta"
erase "$data/pequiv_small.dta"
erase "$data/hgen_small.dta"
erase "$data/hl_small.dta"


*=====================================
*	2. Initial preparation of variables
*=====================================

gen wave = syear-1983 
drop if imonth < 1 | imonth==.
xtset pid wave

rename p11101 lfsato
replace lfsato = . if lfsato < 0

rename i11102 hhyneti
replace hhyneti = . if hhyneti < 0

rename y11101 cpi_2011 
replace cpi_2011 = . if cpi_2011 < 0

rename d11101 age
replace age = . if age < 0
gen agesq = age^2
gen agecu = age^3

rename d11102 sex
recode sex (-1 = .)
bys pid: egen tmp = max(sex)
replace sex = tmp if sex==. 
recode sex (1=0) (2=1)

rename d11106 hhsize
rename d11107 nkids
gen nadult = hhsize - nkids

gen nkids_collapsed = nkids
replace nkids_collapsed = 6 if nkids_collapsed >= 6
gen nadult_collapsed = nadult
replace nadult_collapsed = 6 if nadult_collapsed >=6

rename pgcasmin casmin
recode casmin (-1 = 10) //retain keine angabe as missing category
gen casmin_collapsed = casmin
recode casmin_collapsed (1 2 = 1) (3 4 = 2) (5 6 7 = 3) (8 9 = 4) (-1 . 0 10 = 5) 

rename l11101 govt_reg
replace govt_reg = . if govt_reg == -1 
recode govt_reg (21=0) (22=1) // 0=West Germany, 1=East Germany

gen birth = 0
replace birth = 1 if h11103 > 0

rename syear year 

*=====================================
*	3. Prepare income variables
*=====================================

*-------------------------------------
*	3.1 Generate variables
*-------------------------------------

*Prepare CPI with base 2005
sum cpi_2011 if year==2005
gen cpi_2005 = cpi_2011/`r(mean)'

*Create equivalence scale
gen eqOWN_1 = (nadult - 1) * 0.5
gen eqOWN_2 = (nkids) * 0.3 
gen eqOWN3 = 1 + eqOWN_1 + eqOWN_2
gen lneqOWN3 = ln(eqOWN3)
drop eqOWN_1 eqOWN_2

*Generate own income variables
gen hhyneti_defl = hhyneti/cpi_2005
gen hhyneti_defl_eqOWN3 = hhyneti_defl/eqOWN3

*-------------------------------------
*	3.2 Generate LNs
*-------------------------------------

gen lnhhyneti_defl = ln(hhyneti_defl)
gen lnhhyneti_defl_eqOWN3 = ln(hhyneti_defl_eqOWN3)

*-------------------------------------
*	3.3  Reference Incomes
*-------------------------------------

keep if age >= 18 

local INCOMES "hhyneti_defl_eqOWN3"
sum age
loc MAX_AGE = `r(max)'
loc MIN_AGE = `r(min)'
foreach i of local INCOMES {
	gen ref_`i' = .
	forvalues j=`MIN_AGE'/`MAX_AGE' {
// 		dis "This is age `j'" – A little noisy.
		qui {
			gen age_range=1 if age<=`j'+5 & age>=`j'-5
			
			egen identifier = group(wave  age_range casmin_collapsed) 
			bys identifier: egen temp = mean(`i') if identifier !=.
			replace ref_`i' = temp if age==`j'
			drop identifier temp
			drop age_range
		}
	}
	gen ref_ln`i' = ln(ref_`i')	
}

*Some renames
rename ref_lnhhyneti_defl_eqOWN3 	refinc
rename lnhhyneti_defl_eqOWN3 		owninceq

save "$data/stage3_GER.dta", replace


*=====================================
*	4. More preparations 
*=====================================

use "$data/stage3_GER.dta", clear


*Simplify migration background
cap drop migback_collapsed
gen 	migback_collapsed = 1 if migback==1
replace migback_collapsed = 2 if inlist(migback,2,3)
recode migback_collapsed (1=0) (2=1)


*Simplify education
cap drop tertiary
gen tertiary=0 
replace tertiary=1 if inlist(casmin, 8,9)
gen eduyrs= d11109 if  d11109> 0
gen lneduyrs=ln(eduyrs)


*Household size
cap drop nkids
rename lb0285 nkids
replace nkids = 0 if nkids < 0

cap drop lnnkids 
gen lnnkids = ln(1+nkids_collapsed)
cap drop lnnadult
gen lnnadult = ln(nadult)

gen hhsizec = hhsize if hhsize<6
replace hhsizec=5 if hhsize>5

* Risk Aversion
rename plh0204_v2 risk_aversion
replace risk_aversion =. if risk_aversion<0

*=====================================
* 5. Clean Satisfaction Variables
*=====================================

xtset pid wave

rename plh0173 jobsat
replace jobsat = . if jobsat<0
replace jobsat = plj0454 if jobsat==. & plj0454 > 0
gen ljobsat = l.jobsat


rename plh0321 sat_partner 
replace sat_partner = . if sat_partner < 0
rename plh0180 familysat
replace familysat = . if familysat < 0


rename plh0171 healthsat
replace healthsat = . if healthsat < 0

rename m11126 healthstatus
replace healthstatus = . if healthstatus < 0
replace healthstatus = healthstatus*-1 + 6
replace healthstatus = healthstatus-1

rename plh0183 predicted_sat
replace predicted_sat = . if predicted_sat < 0

rename plh0244 optimism
replace optimism = . if optimism < 0


cap drop flatsat
rename plh0177 flatsat
replace flatsat = . if flatsat < 0
cap drop lflatsat
gen lflatsat = l.flatsat
cap drop changedflat


cap drop high_educ
gen high_educ = 0
replace high_educ =1 if inlist(casmin, 7,8,9)

*=====================================
*	6. Calculating survey weights
*=====================================

// Create variable to store total weights by year
gen total_weight = .

// Loop over years to calculate total weights within each year
quietly levelsof year, local(years)
foreach y of local years {
    quietly su phrf if year == `y', meanonly
    replace total_weight = r(sum) if year == `y'
}

// Normalize phrf weights to sum to 10,000 within each year
gen norm_weight = phrf / total_weight * 10000

*=====================================
*	7. Label variables and save
*=====================================

label variable age "Age"
label variable agesq "Age-squared"
label variable agecu "Age-cubed"
label variable owninc "ln(HH income)"
label variable refinc "ln(ref income)"
label variable lnnadult "ln(# adults in HH)"
label variable lnnkids "ln(# children in HH)"
label variable birth "Child birth"
label variable high_educ "Higher education"
label variable migback_collapsed "Migrant"

label define JOBSAT 0 "Job sat. = 0" 1 "Job sat. = 1" 2 "Job sat. = 2" 3 "Job sat. = 3" 4 "Job sat. = 4" 5 "Job sat. = 5" 6 "Job sat. = 6" 7 "Job sat. = 7" 8 "Job sat. = 8" 9 "Job sat. = 9" 10 "Job sat. = 10" 
label values jobsat JOBSAT
label define FlatSAT 0 "Flat sat. = 0" 1 "Flat sat. = 1" 2 "Flat sat. = 2" 3 "Flat sat. = 3" 4 "Flat sat. = 4" 5 "Flat sat. = 5" 6 "Flat sat. = 6" 7 "Flat sat. = 7" 8 "Flat sat. = 8" 9 "Flat sat. = 9" 10 "Flat sat. = 10" 
label values flatsat FlatSAT
label define FamilySAT 0 "Family sat. = 0" 1 "Family sat. = 1" 2 "Family sat. = 2" 3 "Family sat. = 3" 4 "Family sat. = 4" 5 "Family sat. = 5" 6 "Family sat. = 6" 7 "Family sat. = 7" 8 "Family sat. = 8" 9 "Family sat. = 9" 10 "Family sat. = 10" 
label values familysat FamilySAT
label define HealthSAT 0 "Health sat. = 0" 1 "Health sat. = 1" 2 "Health sat. = 2" ///
    3 "Health sat. = 3" 4 "Health sat. = 4" 5 "Health sat. = 5" ///
    6 "Health sat. = 6" 7 "Health sat. = 7" 8 "Health sat. = 8" ///
    9 "Health sat. = 9" 10 "Health sat. = 10" 
label values healthsat HealthSAT
label define HealthSTATUS 0 "Health status = 0" 1 "Health status = 1" 2 "Health status = 2" ///
    3 "Health status = 3" 4 "Health status = 4" 5 "Health status = 5"
label values healthstatus HealthSTATUS
label define RiskAversion 0 "Risk aversion = 0" 1 "Risk aversion = 1" 2 "Risk aversion = 2" 3 "Risk aversion = 3" 4 "Risk aversion = 4" 5 "Risk aversion = 5" 6 "Risk aversion = 6" 7 "Risk aversion = 7" 8 "Risk aversion = 8" 9 "Risk aversion = 9" 10 "Risk aversion = 10" 
label values risk_aversion RiskAversion


xtset pid year

save "$data/Germany.dta", replace

// erase "$data/stage3_GER.dta"

use "$data/Germany.dta", clear

