********************************************************************************
* Volatility and Stretching
* File: BES_get
* Author: Charlie Harrison
* Date: Nov 9th 2026
********************************************************************************
* SCRIPT OVERVIEW:
* Section 0: Set paths
* Section 1: Load British Election Study W1-W30 (2014-2025)
* Section 2: Reshape to long format
* Section 3: Clean Variables, Create Panel Tenure
* Section 4: Get By-Election Dataset
* Section 5: Get 'Yearly' Dataset
********************************************************************************

*=====================================
*	0. Set Paths
*=====================================



clear all
set more off
* Paths. main.do sets VLINE_ROOT before calling this file. The fallback lets
* the file run on its own, with the repository root as the working directory.
if "$VLINE_ROOT" == "" global VLINE_ROOT "`c(pwd)'"
global data    "$VLINE_ROOT/data/Processed_Data"
global rawdata "$VLINE_ROOT/data/Raw_Data"



* ============================================================
* 1. LOAD DATA
* ============================================================

// use "${rawdata}/BES2024_W30_Panel_v30.1.dta", clear
//
//
// keep id leftRightW* starttimeW* age* gender waves_taken likeConW* likeLabW* fresh_sampleW16 ///
//      wt_full_W1 wt_full_W2 wt_full_W3 wt_full_W4 wt_full_W5 wt_new_W6 wt_new_W7 wt_new_W8 wt_new_W9 ///
//      wt_new_W10 wt_new_W11 wt_new_W12 wt_new_W13_result wt_new_W14 wt_new_W15 wt_new_W16 wt_new_W17 wt_new_W18 wt_new_W19_result ///
//      wt_new_W20 ///
//      wt_new_W21 wt_new_W22 wt_new_W23 wt_new_W24 wt_new_W25 wt_new_W26 wt_new_W27 wt_new_W28 wt_new_W29 wt_new_W30 ///
//      wt_full_W4W5W6 wt_full_W7W8W9 wt_new_W11_W13 wt_new_W17W18W19 wt_new_W27W28W29


// Does not break StataMP now.
use id leftRightW* starttimeW* age* gender waves_taken likeConW* likeLabW* fresh_sampleW16 ///
    wt_full_W1 wt_full_W2 wt_full_W3 wt_full_W4 wt_full_W5 wt_new_W6 wt_new_W7 wt_new_W8 wt_new_W9 ///
    wt_new_W10 wt_new_W11 wt_new_W12 wt_new_W13_result wt_new_W14 wt_new_W15 wt_new_W16 wt_new_W17 wt_new_W18 wt_new_W19_result ///
    wt_new_W20 ///
    wt_new_W21 wt_new_W22 wt_new_W23 wt_new_W24 wt_new_W25 wt_new_W26 wt_new_W27 wt_new_W28 wt_new_W29 wt_new_W30 ///
    wt_full_W4W5W6 wt_full_W7W8W9 wt_new_W11_W13 wt_new_W17W18W19 wt_new_W27W28W29 ///
    using "${rawdata}/BES2024_W30_Panel_v30.1.dta", clear
	 
* ============================================================
* 2. RESHAPE TO LONG
* ============================================================

* Rename weights to common stub
forvalues w = 1/5 {
    rename wt_full_W`w' weight`w'
}
forvalues w = 6/12 {
    rename wt_new_W`w' weight`w'
}
rename wt_new_W13_result weight13
forvalues w = 14/18 {
    rename wt_new_W`w' weight`w'
}
rename wt_new_W19_result weight19

forvalues w = 20/30 {
    rename wt_new_W`w' weight`w'
}

forvalues w = 1/30 {
    capture rename leftRightW`w' lr`w'
    capture rename starttimeW`w' starttime`w'
    capture rename ageGroupW`w' agegroup`w'
    capture rename likeConW`w' likecon`w'
    capture rename likeLabW`w' likelab`w'
	capture rename ageW`w' age`w'

}

reshape long lr starttime agegroup likecon likelab weight age, i(id) j(wave)

* Create election weight variable
gen election_weight = .
replace election_weight = wt_full_W4W5W6   if inlist(wave, 4, 5, 6)
replace election_weight = wt_full_W7W8W9   if inlist(wave, 7, 8, 9)
replace election_weight = wt_new_W11_W13   if inlist(wave, 11, 12, 13)
replace election_weight = wt_new_W17W18W19 if inlist(wave, 17, 18, 19)
replace election_weight = wt_new_W27W28W29 if inlist(wave, 27, 28, 29)

* Drop the wide-format election weights
drop wt_full_W4W5W6 wt_full_W7W8W9 wt_new_W11_W13 wt_new_W17W18W19 wt_new_W27W28W29

* ============================================================
* 3. CLEAN VARIABLES, Create Panel TEnure
* ============================================================

* Left-right
replace lr = . if lr >= 9999 | lr < 0
replace lr = lr

* Party likes (assuming 0-10 scale, 9999 missing)
replace likecon = . if likecon >= 9999 | likecon < 0
replace likecon = likecon

replace likelab = . if likelab >= 9999 | likelab < 0
replace likelab = likelab

* Age group
replace agegroup = . if agegroup >= 9999 | agegroup < 0

* Gender
gen female = (gender == 2) if !missing(gender)

* Time gap placeholder (will compute within each dataset)
gen days_gap = .
gen weeks_gap = .

* Sort by person and wave
sort id wave

* Flag valid LR responses
gen lr_valid = !missing(lr)

* Cumulative count of valid LR responses within person
by id: gen lr_count = sum(lr_valid) - 1

label var lr_valid "Valid LR response this wave"
label var lr_count "Cumulative valid LR responses (including this wave)"

* Clean up
drop lr_valid

rename weight norm_weight

label var norm_weight "Cross-Sectional Wave Weight"
label var lr "Left-Right (0-10)"
label var likecon "Like Conservatives (0-10)"
label var likelab "Like Labour (0-10)"
label var election_weight "Election Cycle Panel Weight"

save "${data}/bes_long_base.dta", replace




* ============================================================
* 4. Get By-Election Dataset
* ============================================================

use "${data}/bes_long_base.dta", clear

* Keep only election-relevant waves
keep if inlist(wave, 4, 5, 6, 7, 8, 9, 11, 12, 13, 17, 18, 19, 27, 28, 29)

* Create election cycle
gen election = .
replace election = 1 if inlist(wave, 4, 5, 6)
replace election = 2 if inlist(wave, 7, 8, 9)
replace election = 3 if inlist(wave, 11, 12, 13)
replace election = 4 if inlist(wave, 17, 18, 19)
replace election = 5 if inlist(wave, 27, 28, 29)

label define elec_lbl 1 "2015_GE" 2 "2016_Brexit" 3 "2017_GE" 4 "2019_GE" 5 "2024_GE"
label values election elec_lbl

* Create wave position within cycle (1, 2, 3)
gen wave_pos = .
replace wave_pos = 1 if inlist(wave, 4, 7, 11, 17, 27)
replace wave_pos = 2 if inlist(wave, 5, 8, 12, 18, 28)
replace wave_pos = 3 if inlist(wave, 6, 9, 13, 19, 29)

* Create lead via self-merge (future - current)
preserve
keep if wave_pos != 1
rename lr lr_lead
rename starttime starttime_lead          // Add this line
gen wave_pos_merge = wave_pos - 1
keep id election wave_pos_merge lr_lead starttime_lead
rename wave_pos_merge wave_pos
tempfile lead
save `lead'
restore

merge m:1 id election wave_pos using `lead', nogen

gen d_lr = lr_lead - lr
gen abs_d_lr = abs(d_lr)

* Days gap - use merged variable directly
replace days_gap = (starttime_lead - starttime) / (1000 * 60 * 60 * 24)


save "${data}/bes_elections.dta", replace



* ============================================================
* 5. Get Yearly Dataset
* ============================================================

use "${data}/bes_long_base.dta", clear

* Keep only yearly waves
keep if inlist(wave, 3, 6, 9, 13, 14, 19, 20, 21, 23, 25, 29, 30)

* Create year variable
gen year = .
replace year = 2014 if wave == 3
replace year = 2015 if wave == 6
replace year = 2016 if wave == 9
replace year = 2017 if wave == 13
replace year = 2018 if wave == 14
replace year = 2019 if wave == 19
replace year = 2020 if wave == 20
replace year = 2021 if wave == 21
replace year = 2022 if wave == 23
replace year = 2023 if wave == 25
replace year = 2024 if wave == 29
replace year = 2025 if wave == 30


xtset id year

gen lr_lead1 = .
gen abs_d_lr = .
replace lr_lead1 = F1.lr
replace abs_d_lr = abs(lr_lead1-lr)

gen lead_starttime = F1.starttime

* Days gap to next observation
replace days_gap = (lead_starttime - starttime) / (1000 * 60 * 60 * 24)

save "${data}/bes_yearly.dta", replace


// erase "${data}/bes_long_base.dta"

use "${data}/bes_long_base.dta", clear
keep id wave lr lr_count
save "${data}/bes_lr_allwaves.dta", replace
erase "${data}/bes_long_base.dta"
