clear all

cd "C:\Users\nrodriguez\OneDrive - World Justice Project\Programmatic\EU Subnational\EU-S Data\eu-data-validation\ALL-valid\Outputs\datasets for replication\"

/*
*------ Importing data from R
import delimited "data_replication.csv", varnames(1) numericcols(6) clear
save "data_replication.dta", replace


*------ Importing data from web - ORIGINAL
import delimited "data_web.csv", varnames(1) numericcols(6) clear 
save "data_web.dta", replace

*/

use "data_web.dta"

*Remove A2J indicators: no validation for these

/*
foreach v in rp_time prevalence2 vulnerability1 vulnerability2 access2info access2rep access2drm rp_cost rp_fair rp_outcome { 
replace value=. if id=="`v'"
} 
*/

foreach v in rp_time vulnerability1 vulnerability2 access2info access2rep rp_cost rp_fair rp_outcome { 
replace value=. if id=="`v'"
} 


*duplicates report country level nuts_id demographic id value
*duplicates drop country level nuts_id demographic id value, force

merge m:1 country level nuts_id id demographic using "data_replication.dta"

gen flag=0
replace flag=1 if (value-value_comparison)>0.01 & (value-value_comparison)!=.

br if flag==1 

& level=="regional"