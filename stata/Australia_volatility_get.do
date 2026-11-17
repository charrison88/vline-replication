********************************************************************************
* Volatility and Stretching
* File: GER_volatility_get
* Author: Charlie Harrison
* Date: November 28th Novemeber
********************************************************************************
* SCRIPT OVERVIEW:
* Section 0: Set paths
* Section 1: Merge and save - Combines 19 waves of HILDA data (2001-2019)
* Section 2: Get covariates right - Prepares demographics, employment, household, and income variables
* Section 3: Clean Satisfaction Variables
* Section 4: (NEW) Calculating survey weights - Cleans and normalizes survey weights for analysis
* Section 5: Label variables and save
********************************************************************************

*=====================================
*	0. Set Paths
*=====================================

global data "/Users/charlieharrison/Google Drive/My Drive/Stretching_Volatility/Processed_Data"
global rawdata "/Users/charlieharrison/Google Drive/My Drive/Stretching_Volatility/Raw_Data"

*=====================================
*	1. Merge and$ save
*=====================================

clear

global keepvars ///
xwaveid ///
///
hgsex /// sex
hhiage /// age at interview date
jbmsall /// job satisfaction
lsrelsp /// partner satisfaction
losatyh /// health satisfaction
losathl /// home satisfaction 
losatnl /// neighbourhood satisfaction
losat   /// life satisfaction
gh1		/// self-assessed health status
hifditp /// Household financial year disposable total income personal (positive values)
hhadult /// DV: Number of persons aged 15+ 
hh0_4 /// Number of children 1
hh5_9 /// Number of children 2
hh10_14 /// Number of children 3
edhigh1 /// History: Highest education level achieved
hhmsr /// major statistical area  
anbcob /// country of birth
hhhqivw /// date of interview 
hhwtrp /// weight(1)
hhwtrps /// weight(2)
firisk /// financial risk


local n=2001
local letters  a b c d e f g h i j k l m n o p q r s
foreach w of local letters {
	use "${rawdata}/HILDA/HILDA_v19_combined/Combined_`w'190c.dta", clear
	rename `w'* *
	
	foreach var in varlist ${keepvars} {
			cap gen `var'  = .
	}

	keep $keepvars 
	gen year = `n'
	local ++n
	save tmp`w'.dta, replace
}

use tmpa.dta, replace
erase tmpa.dta
local letters  b c d e f g h i j k l m n o p q r s
foreach w of local letters {
	append using tmp`w'.dta
	erase tmp`w'.dta
}

encode xwaveid, gen(pid)



*=====================================
*	2. Get covariates right
*=====================================

*-------------------------------------
*	2.1 Only keep things in a 10 to 14 month window (not currently used)
*-------------------------------------

gen wave = year - 2000
drop year // that year variable was always just a wave variable

gen year = substr(hhhqivw,7,4)
destring year, replace
replace year = . if year<0

*-------------------------------------
*	2.2 Sort out age and sex
*-------------------------------------

rename hgsex sex
rename hhiage age
gen agesq = age^2
gen agecu = age^3
keep if age >= 18

recode sex (1=0) (2=1)
label define SEX 0 "Male" 1 "Female", replace
label values sex SEX
xtset pid wave

*-------------------------------------
*	2.3 Prepare all others (except income)
*-------------------------------------


*nadult
rename  hhadult nadult
gen lnnadult = log(nadult)

*nkids 
gen  nkids = hh0_4 + hh5_9 + hh10_14
gen lnnkids = log(1+nkids)

*birth
xtset pid wave
gen birth = 0
replace birth = 1 if hh0_4>l.hh0_4

*high_educ
rename edhigh1 educ
gen high_educ = 0 if inlist(educ,5,8,9,10)
replace high_educ = 1 if inlist(educ,1,2,3,4)
gen casmin_collapsed = educ
recode casmin_collapsed (9 10 = 1) (5 8 = 2) (1 2 3 4 = 3) 

*migback_collapsed
gen 	migback_collapsed = 0 if anbcob==1
replace migback_collapsed = 1 if anbcob!=1

*govt_reg
rename hhmsr govt_reg
replace govt_reg = . if govt_reg==-7

*-------------------------------------
*	2.5 Prepare income
*-------------------------------------

*owninc
gen expowninc = hifditp/(1+(nadult-1)*0.5+nkids*0.3)
gen owninc = ln(1+expowninc)

bys wave: egen wavemedian = pctile(owninc), p(50)


* Create CPI deflator (indexed to 2001 = 1.0)
gen cpi_index = .
replace cpi_index = 1.000000000 if year == 2001
replace cpi_index = 1.028187919 if year == 2002
replace cpi_index = 1.055033557 if year == 2003
replace cpi_index = 1.081879195 if year == 2004
replace cpi_index = 1.108724832 if year == 2005
replace cpi_index = 1.153020134 if year == 2006
replace cpi_index = 1.177181208 if year == 2007
replace cpi_index = 1.229530201 if year == 2008
replace cpi_index = 1.246979866 if year == 2009
replace cpi_index = 1.285906040 if year == 2010
replace cpi_index = 1.331543624 if year == 2011
replace cpi_index = 1.347651007 if year == 2012
replace cpi_index = 1.379865772 if year == 2013
replace cpi_index = 1.421476512 if year == 2014
replace cpi_index = 1.442953022 if year == 2015
replace cpi_index = 1.457718121 if year == 2016
replace cpi_index = 1.485906042 if year == 2017
replace cpi_index = 1.516778523 if year == 2018
replace cpi_index = 1.540939597 if year == 2019


gen hh_income = hifditp / cpi_index
gen personal_income = hifditp / cpi_index


*refinc
gen exprefinc = .
sum age
loc MAX_AGE = `r(max)'
loc MIN_AGE = `r(min)'
forvalues j=`MIN_AGE'/`MAX_AGE' {
	dis "This is age `j'"
	qui {
		gen age_range=1 if age<=`j'+5 & age>=`j'-5
		egen identifier = group(year sex age_range casmin_collapsed) 
		bys identifier: egen temp = mean(expowninc) if identifier !=.
		replace exprefinc = temp if age==`j'
		drop identifier temp
		drop age_range
	}
}
gen refinc = ln(1+exprefinc)	


* risk aversion
rename firisk risk_aversion
replace risk_aversion =. if risk_aversion<0


*=====================================
*	3. Clean Satisfaction Variables
*=====================================

replace lsrelsp = . if lsrelsp < 0
rename lsrelsp partnersat
rename partnersat familysat

replace losatyh = . if losatyh < 0
rename losatyh healthsat

replace losatnl = . if losatnl < 0
rename losatnl flatsat

rename jbmsall jobsat
replace jobsat = . if jobsat < 0

rename gh healthstatus
replace healthstatus = . if healthstatus < 0
replace healthstatus = healthstatus*-1 + 6
replace healthstatus = healthstatus-1



*=====================================
*	4. Calculating survey weights
*=====================================

foreach v in hhwtrp hhwtrps {
    capture confirm variable `v'
    if !_rc {
        replace `v' = . if `v' < 0
    }
}

gen hhwtrps_clean = hhwtrps
replace hhwtrps_clean = 0 if missing(hhwtrps)
gen total_weight = .
quietly levelsof wave, local(waves)
foreach w of local waves {
    su hhwtrps_clean if wave == `w'
    replace total_weight = r(sum) if wave == `w'
}
gen norm_weight = hhwtrps_clean / total_weight * 10000

// Standardize year variable for HILDA (Australia)
rename year year_old
rename wave year
replace year = 2000 + year

*=====================================
*	5. Label variables and save
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
label define RiskAversion 0 "Risk aversion = 0" 1 "Risk aversion = 1" 2 "Risk aversion = 2" 3 "Risk aversion = 3" 4 "Risk aversion = 4" 5 "Risk aversion = 5"
label values risk_aversion RiskAversion


xtset pid year

save "${data}/Australia.dta", replace
