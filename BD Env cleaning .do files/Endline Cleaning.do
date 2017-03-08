

*--------------------------------------------
* BD-Env-dm-Endline-Cleaning.do
*
* andrew mertens (amertens@berkeley.edu)
*
* process the Bangladesh Environmental substudy
* Endline data
*
*--------------------------------------------

cd "C:/Users/andre/Documents/WBB_env_analysis/Data/"
use "washb-bangladesh-tr.dta", clear
sort clusterid

tempfile tr
save `tr'


cd "C:/Users/andre/Dropbox/WASHB EML/Data/5. Final Data"
use "WASHB Env Full Endline Clean.dta", clear
sort dataid


tempfile Endline
save `Endline'

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

use `Endline'
sort dataid
merge dataid using `main'
tab _merge 
keep if _merge!=2


*Check study data for any missing main study sex or dob
list dataid  sex DOB _merge if _merge==2 | sex==. | DOB==. 
drop _merge






************************************
*Generate child ages
************************************

*XXXXXXXXXXXXXXXX
gen date = date(EntryDate_d1, "YMD")  //Which should I use for survey date?
format date %td

*gen month=month(date)
*gen byte rain= month>5 & month<11
*replace rain=. if month==.
destring month_day1, replace
gen byte wet= month_day1>5 & month_day1<11
table wet
*XXXXXXXXXXXXXXXX





gen aged = date-DOB
	label var aged "Age in days (anthro meas)"
gen double agem = aged/30.4375
	label var agem "Age in months (anthro meas)"
gen double agey = aged/365.25
	label var agey "Age in years (anthro meas)"
codebook agey


*Clean variables
destring dataid, replace

rename  q3_6kit_fly_total numfly_kit
destring numfly_kit, replace
	replace numfly_kit=. if numfly_kit==99
gen byte flycaught_kit = numfly_kit>0
	replace flycaught_kit=. if numfly_kit==.

rename  q3_13lat_fly_total  numfly_lat
destring numfly_lat, replace
	replace numfly_lat=. if numfly_lat==99
gen byte flycaught_lat = numfly_lat>0
	replace flycaught_lat=. if numfly_lat==.

	

replace q3mo_L_Nail=. if q3mo_L_Nail>3
replace q3mo_L_Palm=. if q3mo_L_Palm>3
replace q3mo_L_Finger=. if q3mo_L_Finger>3
replace q3mo_R_Nail=. if q3mo_R_Nail>3
replace q3mo_R_Palm=. if q3mo_R_Palm>3
replace q3mo_R_Finger=. if q3mo_R_Finger>3

replace q3ch_L_Nail=. if q3ch_L_Nail>3
replace q3ch_L_Palm=. if q3ch_L_Palm>3
replace q3ch_L_Finger=. if q3ch_L_Finger>3
replace q3ch_R_Nail=. if q3ch_R_Nail>3
replace q3ch_R_Palm=. if q3ch_R_Palm>3
replace q3ch_R_Finger=. if q3ch_R_Finger>3
 
 
gen byte mhdirt = (q3mo_L_Nail==1 | q3mo_L_Palm==1 | q3mo_L_Finger==1 | q3mo_R_Nail==1 | q3mo_R_Palm==1 | q3mo_R_Finger==1)
	replace mhdirt = . if (q3mo_L_Nail==. & q3mo_L_Palm==. & q3mo_L_Finger==. & q3mo_R_Nail==. & q3mo_R_Palm==. & q3mo_R_Finger==.)
gen byte chdirt = (q3ch_L_Nail==1 | q3ch_L_Palm==1 | q3ch_L_Finger==1 | q3ch_R_Nail==1 | q3ch_R_Palm==1 | q3ch_R_Finger==1)
	replace chdirt = . if (q3ch_L_Nail==. & q3ch_L_Palm==. & q3ch_L_Finger==. & q3ch_R_Nail==. & q3ch_R_Palm==. & q3ch_R_Finger==.)

keep dataid clusterid MonthCollectedH logecH logfcH ecposH fcposH MonthCollectedT logecT logfcT ecposT fcposT MonthCollectedW logecW logfcW ecposW fcposW sex date agem mhdirt chdirt numfly_kit numfly_lat flycaught_kit flycaught_lat logecF logfcF ecposF fcposF wet

gen dataid_str= string(dataid, "%05.0f")
gen clusterid_new= substr(dataid_str, 1, 3)

gen clusterid_str= string(clusterid, "%03.0f")
replace clusterid_str=clusterid_new if clusterid_str==""
drop dataid_str clusterid
rename clusterid_str clusterid
sort clusterid




*Merge in treatment information
merge clusterid using `tr'
tab _merge
keep if _merge==3
codebook tr

cd "C:/Users/andre/Dropbox/WASHB EML/Analysis Datasets/Andrew"
saveold "Env_endline_clean.dta", replace version(12)
outsheet using "Env_endline_clean.csv", comma replace




