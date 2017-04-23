

*--------------------------------------------
* Midline Cleaning.do
*
* andrew mertens (amertens@berkeley.edu)
*
* process the Bangladesh Environmental substudy
* midline data
*
*--------------------------------------------

cd "C:/Users/andre/Documents/WBB_env_analysis/Data/"
use "washb-bangladesh-tr.dta", clear
sort clusterid

tempfile tr
save `tr'


cd "C:/Users/andre/Dropbox/WASHB EML/Data/5. Final Data"
use "WASHB Env Full Midline Clean.dta", clear
sort dataid


tempfile Midline
save `Midline'

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

use `Midline'
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
gen date= dofc(q2_2toytime) //Which should I use for survey date?
format date %d

*gen month=month(date)
*rename q1_3monthsd1 month
*destring month, replace
*gen byte rain= month>5 & month<11
*replace rain=. if month==.
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

destring q1_3monthsd1, replace
replace q1_3monthsd1=10 if dataid==42605

gen byte wet= q1_3monthsd1>5 & q1_3monthsd1<11
table wet

rename  q3_6kitnum numfly_kit
	replace numfly_kit=. if numfly_kit==99 | numfly_kit==999
gen byte flycaught_kit = numfly_kit>0
	replace flycaught_kit=. if numfly_kit==.

rename  q3_13latnum  numfly_lat
	replace numfly_lat=. if numfly_lat==99 | numfly_lat==999
gen byte flycaught_lat = numfly_lat>0
	replace flycaught_lat=. if numfly_lat==.


replace q3_2mlnail=. if q3_2mlnail>3
replace q3_2mlpalm=. if q3_2mlpalm>3
replace q3_2mlpad=. if q3_2mlpad>3
replace q3_2mrnail=. if q3_2mrnail>3
replace q3_2mrpalm=. if q3_2mrpalm>3
replace q3_2mrpad=. if q3_2mrpad>3

replace q3_2chlnail=. if q3_2chlnail>3
replace q3_2chlpalm=. if q3_2chlpalm>3
replace q3_2chlpad=. if q3_2chlpad>3
replace q3_2chrnail=. if q3_2chrnail>3
replace q3_2chrpalm=. if q3_2chrpalm>3
replace q3_2chrpad=. if q3_2chrpad>3  
	
	
gen byte mhdirt = (q3_2mlnail==1 | q3_2mlpalm==1 | q3_2mlpad==1 | q3_2mrnail==1 | q3_2mrpalm==1 | q3_2mrpad==1)
	replace mhdirt = . if (q3_2mlnail==. & q3_2mlpalm==. & q3_2mlpad==. & q3_2mrnail==. & q3_2mrpalm==. & q3_2mrpad==.)
gen byte chdirt = (q3_2chlnail==1 | q3_2chlpalm==1 | q3_2chlpad==1 | q3_2chrnail==1 | q3_2chrpalm==1 | q3_2chrpad==1)
	replace chdirt = . if (q3_2chlnail==. & q3_2chlpalm==. & q3_2chlpad==. & q3_2chrnail==. & q3_2chrpalm==. & q3_2chrpad==.)

keep dataid clusterid MonthCollectedH logecH logfcH ecposH fcposH MonthCollectedT logecT logfcT ecposT fcposT MonthCollectedW logecW logfcW ecposW fcposW sex date agem mhdirt chdirt numfly_kit numfly_lat flycaught_kit flycaught_lat wet

gen test= string(dataid, "%05.0f")
gen test2= substr(test, 1, 3)

replace clusterid=test2 if clusterid==""
drop test test2
sort clusterid


*Merge in treatment information
merge clusterid using `tr'
tab _merge
keep if _merge==3
codebook tr



cd "C:/Users/andre/Dropbox/WASHB EML/Analysis Datasets/Andrew"
saveold "Env_midline_clean.dta", replace version(12)
outsheet using "Env_midline_clean.csv", comma replace




