*! ===========================================================================
*! main.do -- the Stata half of the V-LINE pipeline
*!
*! Stata 19 MP (StataNow). Runs the seven data-building do-files in order,
*! turning the raw survey downloads in data/Raw_Data into the panel .dta
*! files in data/Processed_Data that the R scripts then read.
*!
*! Usage, from the repository root:
*!     do main.do
*!
*! Then run the R side:  source("run_all.R")   -- see README.md
*!
*! IMPORTANT, read before first use
*! --------------------------------
*! The do-files in stata/ are byte-for-byte copies of the originals and have
*! NOT been rewritten to use relative paths. Each one sets its own
*!     global data    "..."
*!     global rawdata "..."
*! in its first 25 lines, pointing at the original author's Google Drive.
*! A global set here would be overwritten by those lines, so pointing this
*! pipeline at your own machine means editing those two lines in each of the
*! seven files, once. The check below tells you if you have not.
*! ===========================================================================

clear all
set more off
version 19

* --- Where this repository lives -------------------------------------------
* Set this to the absolute path of the repository root, or run main.do with
* the root as the working directory and leave it as is.

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

* --- Warn if the do-files still point somewhere else ------------------------
tempname fh
file open `fh' using "`root'/stata/Germany_volatility_get.do", read text
local found_foreign 0
file read `fh' line
while r(eof) == 0 {
    if strpos(`"`macval(line)'"', "/Users/charlieharrison") > 0 {
        local found_foreign 1
    }
    file read `fh' line
}
file close `fh'

if `found_foreign' {
    display as error "{hline 76}"
    display as error "The do-files in stata/ still contain the original"
    display as error "author's absolute paths. Edit the two global lines at"
    display as error "the top of each file in stata/ to point at:"
    display as error "    global data    \"$VLINE_DATA\""
    display as error "    global rawdata \"$VLINE_RAW\""
    display as error "then re-run. See README.md, section \"Stata paths\"."
    display as error "{hline 76}"
    exit 198
}

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
