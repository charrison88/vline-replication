*! ===========================================================================
*! main.do -- the Stata half of the V-LINE pipeline
*!
*! Stata 19 MP (StataNow). Runs the four data-building do-files,
*! turning the raw survey downloads in data/Raw_Data into the panel .dta
*! files in data/Processed_Data that the R scripts then read.
*!
*! Usage, from the repository root:
*!     do main.do
*!
*! Then run the R side:  source("run_all.R")   -- see README.md
*!
*! Paths
*! -----
*! main.do sets VLINE_ROOT from the working directory and each do-file in
*! stata/ derives $data and $rawdata from it, so nothing needs editing. Run
*! this from the repository root.
*! ===========================================================================

clear all
set more off
version 19

* --- Where this repository lives -------------------------------------------
* Taken from the working directory, so run main.do from the repository root.
* Every path downstream derives from this.

local root "`c(pwd)'"

if !fileexists("`root'/main.do") {
    display as error "main.do not found in `root'."
    display as error "Run this from the repository root, or set the local" ///
                     " macro root by hand."
    exit 601
}

global VLINE_ROOT   "`root'"
global VLINE_DATA   "`root'/data/Processed_Data"
global VLINE_RAW    "`root'/data/Raw_Data"

display _n "{hline 76}"
display "V-LINE: Stata data build"
display "Root:    $VLINE_ROOT"
display "Raw:     $VLINE_RAW"
display "Output:  $VLINE_DATA"
display "{hline 76}" _n

capture mkdir "$VLINE_DATA"

* --- Build ------------------------------------------------------------------
* Each file builds one panel from its raw survey downloads, independently of
* the others, so the order below is not load-bearing.

local steps                     ///
    "Germany_volatility_get"    ///
    "Australia_volatility_get"  ///
    "Swiss_volatility_get"      ///
    "BES_get"

foreach s of local steps {
    display _n "{hline 76}"
    display ">>> `s'.do"
    display "{hline 76}"
    do "`root'/stata/`s'.do"
}

* GDP plot.do produces a slide-only figure and needs no upstream step.
* It is not part of the paper; uncomment to run it.
* do "`root'/stata/GDP plot.do"

display _n "{hline 76}"
display "Stata build complete. Panels written to $VLINE_DATA"
display "Next:  open vline-replication.Rproj and  source(\"run_all.R\")"
display "{hline 76}"
