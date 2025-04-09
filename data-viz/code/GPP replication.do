/*=====================================================================================================================================
Project:		EU REPORT REPLICATION 2024
Author(s):		Natalia Rodriguez
Dependencies:  	World Justice Project
Creation Date:	January 2025
=======================================================================================================================================*/

clear all
cls


/*=====================================================================================================================================
					0. Settings and Data Loading
=====================================================================================================================================*/


*--- Required packages:
* NONE


*--- Defining directories paths:
// First, we define the path to the SharePoint in your local computer (This is different for EACH USER):

*------ (a) Natalia Rodriguez:
if (inlist("`c(username)'", "nrodriguez")) {
	global path2SP "C:\Users\nrodriguez\OneDrive - World Justice Project\Programmatic\EU Subnational\EU-S Data"
}

*------ (b) Any other user: PLEASE INPUT YOUR PERSONAL PATH TO THE SHAREPOINT DIRECTORY:
else {
	global path2SP ""
}

// Second, we define the path to the data and reference do-files (This is the same for ALL USERS):
// No need to change

global doFiles ""
global data "${path2SP}\eu-gpp\1. Data\3. Merge"
global exp "${path2SP}\eu-data-validation\ALL-valid\Outputs"


*------ Loading Data - region names and weights

import excel "${path2SP}\reports\eu-thematic-reports\data-viz\inputs\region_labels.xlsx", sheet("Hoja1") firstrow clear

save "${path2SP}\reports\eu-thematic-reports\data-viz\inputs\region_labels.dta", replace


*------ Loading Data - EU_GPP merged
use "$data\\EU_GPP_2024.dta", clear


/*=====================================================================================================================================
					1. Recoding variables
=====================================================================================================================================*/


*Attitudes towards corruption
recode ATC_* (1 2 3=0) (4=1) (98=0) (99=.)


*Perceptions of corruption
foreach v in "COR_parliament COR_govt_national COR_govt_local COR_judges COR_prosecutors COR_pda COR_police COR_landreg COR_carreg COR_pparties COR_inst_eu" {
	recode `v' (1 2=0) (3 4=1) (98=0) (99=.)
}


*Positive direction likert 4 - MODULES
global var_1 "TRT IPR IRE CPA CPB CJP LEP JSE CTZ PAB"
foreach v of global var_1 {
	recode `v'_* (1 2=1) (3 4=0) (98=0) (99=.)
}


*ORC
foreach v in ORC_corimpact ORC_citizen_fight ORC_govtefforts ORC_impartial_measures ORC_pconnections  {
	recode `v' (1 2=1) (3 4=0) (98=0) (99=.)
}


*Corruption 3 years
recode COR_3year_change (1 2=1) (3 4 5=0) (98=0) (99=.)


*Security
recode SEC_walking (1 2=1) (3 4=0) (98=0) (99=.)


*Discrimination
recode DIS_sex DIS_age DIS_health DIS_ethni DIS_migration DIS_ses DIS_location DIS_religion DIS_family DIS_gender DIS_politics ///
(2=0) (98=0) (99=.)

egen discr_t=rowtotal(DIS_sex DIS_age DIS_health DIS_ethni DIS_migration DIS_ses DIS_location DIS_religion DIS_family DIS_gender DIS_politics)

gen discrimination1=(discr_t>0)


*Civic Participation
recode CP_cso (2=0) (98=0) (99=.)


*Police and Community Safety

gen psafe1=.
replace psafe1=1 if LEP_safecom==1 & LEP_safefam==1 & LEP_policehelp==1 & LEP_kindpol==1 & LEP_polservcom==1 
replace psafe1=0 if LEP_safecom==0 | LEP_safefam==0 | LEP_policehelp==0 | LEP_kindpol==0 | LEP_polservcom==0 


***/ A2J variables

*Problem prevalence

recode AJP_A1_bin AJP_A2_bin AJP_A3_bin AJP_B1_bin AJP_B2_bin AJP_B3_bin AJP_B4_bin AJP_C1_bin AJP_C2_bin AJP_C3_bin AJP_C4_bin AJP_D1_bin AJP_D2_bin AJP_D3_bin AJP_D4_bin AJP_D5_bin AJP_D6_bin AJP_E1_bin AJP_E2_bin AJP_E3_bin AJP_F1_bin AJP_F2_bin AJP_G1_bin AJP_G2_bin AJP_G3_bin AJP_H1_bin AJP_H2_bin AJP_H3_bin AJP_I1_bin AJP_J1_bin AJP_J2_bin AJP_J3_bin AJP_J4_bin AJP_K1_bin AJP_K2_bin AJP_K3_bin AJP_L1_bin AJP_L2_bin (2=0)


foreach v in A1 A2 A3 B1 B2 B3 B4 C1 C2 C3 C4 D1 D2 D3 D4 D5 D6 E1 E2 E3 F1 F2 G1 G2 G3 H1 H2 H3 I1 J1 J2 J3 J4 K1 K2 K3 L1 L2 {
	gen AJP_`v'_bin_4=AJP_`v'_bin if AJP_`v'_sev>3 & AJP_`v'_sev<98
	replace AJP_`v'_bin_4=0 if AJP_`v'_sev<=3
	replace AJP_`v'_bin_4=. if AJP_`v'_sev>=98
}

egen prevalence1=rowtotal(AJP_A1_bin_4 AJP_A2_bin_4 AJP_A3_bin_4 AJP_B1_bin_4 AJP_B2_bin_4 AJP_B3_bin_4 AJP_B4_bin_4 AJP_C1_bin_4 AJP_C2_bin_4 AJP_C3_bin_4 AJP_C4_bin_4 AJP_D1_bin_4 AJP_D2_bin_4 AJP_D3_bin_4 AJP_D4_bin_4 AJP_D5_bin_4 AJP_D6_bin_4 AJP_E1_bin_4 AJP_E2_bin_4 AJP_E3_bin_4 AJP_F1_bin_4 AJP_F2_bin_4 AJP_G1_bin_4 AJP_G2_bin_4 AJP_G3_bin_4 AJP_H1_bin_4 AJP_H2_bin_4 AJP_H3_bin_4 AJP_I1_bin_4 AJP_J1_bin_4 AJP_J2_bin_4 AJP_J3_bin_4 AJP_J4_bin_4 AJP_K1_bin_4 AJP_K2_bin_4 AJP_K3_bin_4 AJP_L1_bin_4 AJP_L2_bin_4)

gen prevalence2=(prevalence1>0)


*No access

gen no_access=.
replace no_access=. if prevalence2==0
replace no_access=0 if AJR_noresol_reason==3 | AJR_noresol_reason==5 | AJR_noresol_reason==6 | AJR_noresol_reason==7 | AJR_noresol_reason==8
replace no_access=1 if AJR_noresol_reason==1 | AJR_noresol_reason==2 | AJR_noresol_reason==4 | AJR_noresol_reason==9 | AJR_noresol_reason==10
replace no_access=. if AJR_noresol_reason==11
replace no_access=. if AJR_noresol_reason>97
replace no_access=. if AJR_noresol_reason==.

gen no_access_v2=.
replace no_access_v2=. if prevalence2==0
replace no_access_v2=0 if AJR_noresol_reason==3 | AJR_noresol_reason==5 | AJR_noresol_reason==6 | AJR_noresol_reason==7 | AJR_noresol_reason==8
replace no_access_v2=1 if AJR_noresol_reason==1 | AJR_noresol_reason==2 | AJR_noresol_reason==4 | AJR_noresol_reason==9 | AJR_noresol_reason==10
replace no_access_v2=0 if AJR_noresol_reason==11
replace no_access_v2=0 if AJR_noresol_reason>97
replace no_access_v2=. if AJR_noresol_reason==.

*Access drm

gen access2drm=.
replace access2drm=0 if prevalence2==0
replace access2drm=1 if AJR_resolution==1
replace access2drm=0 if AJR_resolution==2 & (AJR_noresol_reason==3 | AJR_noresol_reason==5 | AJR_noresol_reason==6 | AJR_noresol_reason==7 | AJR_noresol_reason==8)  
replace access2drm=. if AJR_resolution==98 | AJR_resolution==99

gen access2drm_v2=.
replace access2drm_v2=0 if prevalence2==0
replace access2drm_v2=1 if AJR_resolution==1
replace access2drm_v2=0 if AJR_resolution==2 & (AJR_noresol_reason==3 | AJR_noresol_reason==5 | AJR_noresol_reason==6 | AJR_noresol_reason==7 | AJR_noresol_reason==8)  
replace access2drm_v2=0 if AJR_resolution==98 | AJR_resolution==99

*No access drm

gen no_access2drm=.
replace no_access2drm=0 if prevalence2==0
replace no_access2drm=1 if AJR_noresol_reason==3 | AJR_noresol_reason==5 | AJR_noresol_reason==6 | AJR_noresol_reason==7 | AJR_noresol_reason==8
replace no_access2drm=0 if AJR_noresol_reason==1 | AJR_noresol_reason==2 | AJR_noresol_reason==4 | AJR_noresol_reason==9 | AJR_noresol_reason==10
replace no_access2drm=. if AJR_noresol_reason==11
replace no_access2drm=. if AJR_noresol_reason>97


gen no_access2drm_v2=.
replace no_access2drm_v2=0 if prevalence2==0
replace no_access2drm_v2=1 if AJR_noresol_reason==3 | AJR_noresol_reason==5 | AJR_noresol_reason==6 | AJR_noresol_reason==7 | AJR_noresol_reason==8
replace no_access2drm_v2=0 if AJR_noresol_reason==1 | AJR_noresol_reason==2 | AJR_noresol_reason==4 | AJR_noresol_reason==9 | AJR_noresol_reason==10
replace no_access2drm_v2=0 if AJR_noresol_reason==11
replace no_access2drm_v2=0 if AJR_noresol_reason>97


#delimit ;

foreach v in 
rp_time 
vulnerability1
vulnerability2
access2info
access2rep
rp_cost
rp_fair
rp_outcome { ;

gen `v'=. ;

} ;

#delimit cr



/*=====================================================================================================================================
					2. Export data
=====================================================================================================================================*/

*----- A2J data for graph check

preserve

*Region

collapse (mean) prevalence2 no_access no_access_v2 access2drm access2drm_v2 no_access2drm no_access2drm_v2, by(country_name_ltn nuts_id)

foreach v in prevalence2 no_access no_access_v2 access2drm_v2 no_access2drm_v2 {
	format %10.0g `v'
}

save "$exp\datasets for replication\regional_a2j_graph.dta", replace


*Country

merge 1:1 nuts_id using "${path2SP}\reports\eu-thematic-reports\data-viz\inputs\region_labels.dta"
drop _merge

collapse (mean) prevalence2 no_access no_access_v2 access2drm access2drm_v2 no_access2drm no_access2drm_v2 [pw=regionpoppct], by(country_name_ltn)

foreach v in prevalence2 no_access no_access_v2 access2drm access2drm_v2 no_access2drm no_access2drm_v2 {
	replace `v'=. if country_name_ltn=="Ireland"
	
}

save "$exp\datasets for replication\national__a2j_graph.dta", replace

restore




*----- Indicator's list for collapse: From outline & output for dashboard

#delimit ;

global indicators "
LEP_accountability
LEP_bribesreq
LEP_bribesacc
LEP_accusation
CPA_cleanelec_local
CPA_freevote
CPA_media_freeop
CPB_freemedia
CPB_freexp_cso
CPB_freexp_pp
CPB_freexp
CPB_freeassem
PAB_censorvoices
PAB_blamesoc
PAB_attackopp
PAB_prosecuteopp
PAB_distract
PAB_freecourts
PAB_misinfo
PAB_credibility
PAB_attackmedia
CPA_freepolassoc
CPA_cons_cso
CPB_freeassoc
CPB_community
CPA_cons_citizen
CP_cso
discrimination1
CTZ_gendereq
CPB_unions
CTZ_consrights
CTZ_laborcond
IPR_rights
IPR_easy2read
LEP_rightsresp
LEP_exforce
LEP_pdaperformance
CJP_fairtrial
CJP_proofburden
JSE_rightsaware
JSE_access2info
JSE_access2assis
JSE_affordcosts
rp_time
JSE_fairoutcomes
JSE_equality
JSE_enforce
JSE_mediation
prevalence2
vulnerability1
vulnerability2
access2info
access2rep
access2drm
rp_cost
rp_fair
rp_outcome
LEP_investigations
LEP_indpolinv
CJP_effective
CJP_efficient
CJP_consistent
CJP_resprights
CJP_egalitarian
CJP_victimsupport
CJP_saferights
SEC_walking
psafe1
COR_parliament
COR_govt_national
COR_govt_local
COR_judges
COR_prosecutors
COR_pda
COR_police
COR_landreg
COR_carreg
COR_pparties
COR_inst_eu
ORC_corimpact
ORC_citizen_fight
ORC_govtefforts
ORC_impartial_measures
ORC_pconnections
ATC_embezz_priv
ATC_recruitment_public
IPR_easy2find
IPR_easy2find_online
PAB_censorinfo
DIS_age
DIS_health
DIS_ethni
DIS_migration
DIS_ses
DIS_location
DIS_religion
DIS_family
DIS_gender
DIS_politics
PAB_blameext
LEP_lawacts
PAB_emergpower
PAB_prosecutejourn
PAB_centralize
CPA_partdem_congress
CPA_partdem_localgvt
CPA_freemedia
DIS_sex
TRT_police
LEP_polservcom
TRT_prosecutors
TRT_judges
TRT_pda
LEP_safecom
LEP_safefam
LEP_policehelp
LEP_kindpol
COR_3year_change
IRE_govtbudget
IRE_govtcontracts
CTZ_envprotect
";
#delimit cr

foreach v in $indicators {
	format %10.0g `v'
}


*Create vars for sample sizes
foreach v in $indicators {
	g `v'_n=`v'
}

global indicators_n "LEP_accountability_n LEP_bribesreq_n LEP_bribesacc_n LEP_accusation_n CPA_cleanelec_local_n CPA_freevote_n CPA_media_freeop_n CPB_freemedia_n CPB_freexp_cso_n CPB_freexp_pp_n CPB_freexp_n CPB_freeassem_n PAB_censorvoices_n PAB_blamesoc_n PAB_attackopp_n PAB_prosecuteopp_n PAB_distract_n PAB_freecourts_n PAB_misinfo_n PAB_credibility_n PAB_attackmedia_n CPA_freepolassoc_n CPA_cons_cso_n CPB_freeassoc_n CPB_community_n CPA_cons_citizen_n CP_cso_n discrimination1_n CTZ_gendereq_n CPB_unions_n CTZ_consrights_n CTZ_laborcond_n IPR_rights_n IPR_easy2read_n LEP_rightsresp_n LEP_exforce_n LEP_pdaperformance_n CJP_fairtrial_n CJP_proofburden_n JSE_rightsaware_n JSE_access2info_n JSE_access2assis_n JSE_affordcosts_n rp_time_n JSE_fairoutcomes_n JSE_equality_n JSE_enforce_n JSE_mediation_n prevalence2_n vulnerability1_n vulnerability2_n access2info_n access2rep_n access2drm_n rp_cost_n rp_fair_n rp_outcome_n LEP_investigations_n LEP_indpolinv_n CJP_effective_n CJP_efficient_n CJP_consistent_n CJP_resprights_n CJP_egalitarian_n CJP_victimsupport_n CJP_saferights_n SEC_walking_n psafe1_n COR_parliament_n COR_govt_national_n COR_govt_local_n COR_judges_n COR_prosecutors_n COR_pda_n COR_police_n COR_landreg_n COR_carreg_n COR_pparties_n COR_inst_eu_n ORC_corimpact_n ORC_citizen_fight_n ORC_govtefforts_n ORC_impartial_measures_n ORC_pconnections_n ATC_embezz_priv_n ATC_recruitment_public_n IPR_easy2find_n IPR_easy2find_online_n PAB_censorinfo_n DIS_age_n DIS_health_n DIS_ethni_n DIS_migration_n DIS_ses_n DIS_location_n DIS_religion_n DIS_family_n DIS_gender_n DIS_politics_n PAB_blameext_n LEP_lawacts_n PAB_emergpower_n PAB_prosecutejourn_n PAB_centralize_n CPA_partdem_congress_n CPA_partdem_localgvt_n CPA_freemedia_n DIS_sex_n TRT_police_n LEP_polservcom_n TRT_prosecutors_n TRT_judges_n TRT_pda_n LEP_safecom_n LEP_safefam_n LEP_policehelp_n LEP_kindpol_n COR_3year_change_n IRE_govtbudget_n IRE_govtcontracts_n CTZ_envprotect_n"


*----- Total Sample

preserve


*Region

collapse (mean) $indicators (count) $indicators_n, by(country_name_ltn nuts_id)

foreach v in $indicators {
	format %10.0g `v'
}

*Removing low counts: Less than 30

foreach v in $indicators {
	replace `v'=. if `v'_n<30
}

drop $indicators_n

save "$exp\datasets for replication\regional_Total Sample.dta", replace


*Country

merge 1:1 nuts_id using "${path2SP}\reports\eu-thematic-reports\data-viz\inputs\region_labels.dta"
drop _merge

collapse (mean) $indicators [pw=regionpoppct], by(country_name_ltn)

save "$exp\datasets for replication\national_Total Sample.dta", replace


*EU average

gen level="European Union"

collapse (mean) $indicators, by(level)

save "$exp\datasets for replication\eu_Total Sample.dta", replace

restore


*----- Gender

gen gender=gend
recode gender (3 4=.)

preserve


*Region

collapse (mean) $indicators (count) $indicators_n, by(country_name_ltn nuts_id gender)

foreach v in $indicators {
	format %10.0g `v'
}

label define gender 1 "Male" 2 "Female"
label values gender gender

drop if gender==.

*Removing low counts: Less than 30

foreach v in $indicators {
	replace `v'=. if `v'_n<30
}

drop $indicators_n

save "$exp\datasets for replication\regional_Sex.dta", replace


*Country

merge m:1 nuts_id using "${path2SP}\reports\eu-thematic-reports\data-viz\inputs\region_labels.dta"
drop _merge

collapse (mean) $indicators [pw=regionpoppct], by(country_name_ltn gender)

save "$exp\datasets for replication\national_Sex.dta", replace


*EU average

gen level="European Union"

collapse (mean) $indicators, by(level gender)

save "$exp\datasets for replication\eu_Sex.dta", replace

restore


*----- Age

gen age_2=.
replace age_2=1 if age==1 & country_name_ltn=="Luxembourg"
replace age_2=2 if age==2 & country_name_ltn=="Luxembourg"
replace age_2=3 if age==3 & country_name_ltn=="Luxembourg"
replace age_2=4 if age==4 & country_name_ltn=="Luxembourg"
replace age_2=5 if age==5 & country_name_ltn=="Luxembourg"
replace age_2=6 if age==6 & country_name_ltn=="Luxembourg"

replace age_2=1 if age<=24 & country_name_ltn!="Luxembourg"
replace age_2=2 if age<=34 & age>=25 & country_name_ltn!="Luxembourg"
replace age_2=3 if age<=44 & age>=35 & country_name_ltn!="Luxembourg"
replace age_2=4 if age<=54 & age>=45 & country_name_ltn!="Luxembourg"
replace age_2=5 if age<=64 & age>=55 & country_name_ltn!="Luxembourg"
replace age_2=6 if age>=65 & country_name_ltn!="Luxembourg"

preserve


*Region

collapse (mean) $indicators (count) $indicators_n, by(country_name_ltn nuts_id age_2)

foreach v in $indicators {
	format %10.0g `v'
}

label define age 1 "18-24" 2 "25-34" 3 "35-44" 4 "45-54" 5 "55-64" 6 "+65"
label values age_2 age

*Removing low counts: Less than 30

foreach v in $indicators {
	replace `v'=. if `v'_n<30
}

drop $indicators_n

save "$exp\datasets for replication\regional_Age.dta", replace


*Country

merge m:1 nuts_id using "${path2SP}\reports\eu-thematic-reports\data-viz\inputs\region_labels.dta"
drop _merge

collapse (mean) $indicators [pw=regionpoppct], by(country_name_ltn age_2)

save "$exp\datasets for replication\national_Age.dta", replace


*EU average

gen level="European Union"

collapse (mean) $indicators, by(level age_2)

save "$exp\datasets for replication\eu_Age.dta", replace

restore


*----- Income

gen quintile=income_quintile
recode quintile (98 99=.)

preserve


*Region

collapse (mean) $indicators (count) $indicators_n, by(country_name_ltn nuts_id quintile)

foreach v in $indicators {
	format %10.0g `v'
}

drop if quintile==.

label define quintile 1 "Income Quintile 1" 2 "Income Quintile 2" 3 "Income Quintile 3" 4 "Income Quintile 4" 5 "Income Quintile 5" 
label values quintile quintile

*Removing low counts: Less than 30

foreach v in $indicators {
	replace `v'=. if `v'_n<30
}

drop $indicators_n

save "$exp\datasets for replication\regional_Income.dta", replace


*Country

merge m:1 nuts_id using "${path2SP}\reports\eu-thematic-reports\data-viz\inputs\region_labels.dta"
drop _merge

collapse (mean) $indicators [pw=regionpoppct], by(country_name_ltn quintile)

save "$exp\datasets for replication\national_Income.dta", replace


*EU average

gen level="European Union"

collapse (mean) $indicators, by(level quintile)

save "$exp\datasets for replication\eu_Income.dta", replace

restore


*----- Urban/Rural

preserve


*Region

collapse (mean) $indicators (count) $indicators_n, by(country_name_ltn nuts_id urban)

foreach v in $indicators {
	format %10.0g `v'
}

*Removing low counts: Less than 30

foreach v in $indicators {
	replace `v'=. if `v'_n<30
}

drop $indicators_n


save "$exp\datasets for replication\regional_Urban.dta", replace


*Country

merge m:1 nuts_id using "${path2SP}\reports\eu-thematic-reports\data-viz\inputs\region_labels.dta"
drop _merge

collapse (mean) $indicators [pw=regionpoppct], by(country_name_ltn urban)

save "$exp\datasets for replication\national_Urban.dta", replace


*EU average

gen level="European Union"

collapse (mean) $indicators, by(level urban)

save "$exp\datasets for replication\eu_Urban.dta", replace

restore






/*
*Negative direction likert 4
local var_2 ""
foreach v of local var_2 {
	recode `v'_* (1 2=0) (3 4=1) (98=0) (99=.)
}







gen sev_sel=.
foreach v in A1 A2 A3 B1 B2 B3 B4 C1 C2 C3 C4 D1 D2 D3 D4 D5 D6 E1 E2 E3 F1 F2 G1 G2 G3 H1 H2 H3 I1 J1 J2 J3 J4 K1 K2 K3 L1 L2 {
	replace sev_sel=AJP_`v'_sev if AJP_problem=="`v'"	
}

replace sev_sel=. if 

tab AJE_infosource if nuts_id=="AT3" & sev_sel>3 & AJE_infosource!=99 , nol

tab AJE_infosource if nuts_id=="LU00" & sev_sel>3 & AJE_infosource!=99 , nol


& AJE_infosource!=98 , nol