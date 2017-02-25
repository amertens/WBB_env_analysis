

*--------------------------------------------
* BD-Env-dm-World-Bank.do
*
* andrew mertens (amertens@berkeley.edu)
*
* process the Bangladesh Environmental substudy
* World Bank (semi-baseline) data
*
*--------------------------------------------

cd "C:/Users/andre/Dropbox/WASHB EML/Data/5. Final Data"
use "World Bank Env Full Data Clean.dta", clear
sort dataid


tempfile worldbank
save `worldbank'

*-----------------------------------------------
*Extract DOB and Sex from main trial diarrhea data
*-----------------------------------------------


*First, merge in main study DOB from anthro and diar datasets
cd "C:/Users/andre/Documents/WBB_env_analysis/Data/"
use washb-bangladesh-anthro, clear

duplicates tag dataid childid, generate(dup)
keep if childid=="T1" | childid=="T2"

gen childNo= substr(childid,2,1) //change childid to match EE
rename dob anthrodob
rename sex anthrosex
gen byte MainStudyDataset_anthro_DOB=1 
gen byte MainStudyDataset_anthro_sex=1 
keep dataid childNo svy anthrodob MainStudyDataset_anthro_DOB anthrosex MainStudyDataset_anthro_sex
sort dataid childNo svy

duplicates drop dataid childNo anthrodob, force
duplicates list dataid childNo

tempfile anthro
save `anthro'

use washb-bangladesh-diar, clear
keep if childid=="T1" | childid=="T2"

gen childNo= substr(childid,2,1)
rename dob diardob
rename sex diarsex
gen byte MainStudyDataset_diar_DOB=1 
gen byte MainStudyDataset_diar_sex=1 
keep dataid childNo svy diardob MainStudyDataset_diar_DOB diarsex MainStudyDataset_diar_sex
sort dataid childNo svy

duplicates tag dataid childNo, generate(dup)
duplicates drop dataid childNo diardob, force
duplicates list dataid childNo

merge dataid childNo svy using `anthro' 


tab _merge
drop _merge dup
sort dataid childNo svy
duplicates list dataid childNo
duplicates tag dataid childNo, generate(dup)
drop if dup==1 & svy!=1


replace diarsex=anthrosex if diarsex==. & anthrosex!=.
rename diarsex sex
tab sex
replace diardob=anthrodob if diardob==. & anthrodob!=.
rename diardob DOB
keep dataid sex DOB


duplicates list dataid
duplicates drop dataid, force //Investigate

sort dataid
tempfile main
save `main'

use `worldbank'
sort dataid
merge dataid using `main'
tab _merge 
keep if _merge!=2


*Check that there is World Bank data for any missing main study sex or dob
list dataid dobt dobc1 sext sexc1 _merge if _merge==2 | sex==. | DOB==. 
drop _merge



*If DOB and sex exist in the diar dataset, use these since these have been 
*triple-checked and used in all the other analyses.
 
*If DOB and sex are missing in the diar dataset and a target child has been captured 
*by the World Bank study, use their DOB and sex  (vars: dobt, sext).
replace sex=sext if sex==.
replace DOB=dobt if DOB==.

*If DOB and sex are missing in the diar dataset and no target child has been 
*captured by the World Bank study, that means this was a household where the 
*target child had died prior to the World Bank visit and we enrolled the next 
*youngest child instead. Use the DOB and sex of this child (vars: dobc1, sexc1).
replace sex=sexc1 if sex==.
replace DOB=dobc1 if DOB==.


*Drop unneeded variables
drop dobt dobc1 sext sexc1 



************************************
*Generate child ages
************************************
gen aged = date-DOB
	label var aged "Age in days (anthro meas)"
gen double agem = aged/30.4375
	label var agem "Age in months (anthro meas)"
gen double agey = aged/365.25
	label var agey "Age in years (anthro meas)"
codebook agey


*Clean variables
destring dataid, replace

cd "C:/Users/andre/Dropbox/WASHB EML/Analysis Datasets/Andrew"
saveold "Env_worldbank_clean.dta", replace version(12)
outsheet using "Env_worldbank_clean.csv", comma replace
