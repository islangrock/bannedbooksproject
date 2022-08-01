capture log close
clear
set more off

local path "C:\Users\marce\OneDrive\Desktop\Data_banned_books_project"

*CREATE DATASET WITH SOCIODEMOGRAPHIC DATA:

import delimited `path'/DP02_001_USSchoolDistrictAll_628102643796.txt, delimiter("|") 
drop *moe
save "`path'/temp1.dta", replace
clear
import delimited `path'/DP03_001_USSchoolDistrictAll_628103014242.txt, delimiter("|") 
drop *moe
save "`path'/temp2.dta", replace
clear
import delimited `path'/DP05_001_USSchoolDistrictAll_62810115257.txt, delimiter("|") 
drop *moe
save "`path'/temp3.dta", replace
clear
import delimited `path'/PDP02_201_USSchoolDistrictAll_628101840970.txt, delimiter("|") 
drop *moe
save "`path'/temp4.dta", replace
clear
import delimited `path'/PDP03_201_USSchoolDistrictAll_628101628181.txt, delimiter("|") 
drop *moe
save "`path'/temp5.dta", replace
clear
import delimited `path'/PDP05_201_USSchoolDistrictAll_628101241557.txt, delimiter("|") 
drop *moe
merge 1:1 geoid using "`path'/temp1.dta"
drop _m
merge 1:1 geoid using "`path'/temp2.dta"
drop _m
merge 1:1 geoid using "`path'/temp3.dta"
drop _m
merge 1:1 geoid using "`path'/temp4.dta"
drop _m
merge 1:1 geoid using "`path'/temp5.dta"
drop _m

*erase temporary files

erase "`path'/temp1.dta" 
erase "`path'/temp2.dta" 
erase "`path'/temp3.dta" 
erase "`path'/temp4.dta" 
erase "`path'/temp5.dta" 



*a few data management
split geoid , p("US")
destring geoid2, replace
ren geoid geoid_original
ren geoid2 geoid
drop geoid1
order geoid

*check for duplicates
sort geoid
quietly by geoid:  gen dup = cond(_N==1,0,_n)
sort dup
drop if dup!=0
drop dup

save "`path'/school_distric_sociodemographics.dta", replace
clear

*CREATE DATASET WITH BAN DATA
import excel "`path'/school_distric_bans.xls", sheet("Bookbans-crosswalk") firstrow
keep GEOID ban_counts inconsistency

ren GEOID geoid
save "`path'/bans.dta", replace

merge 1:1 geoid using "`path'/school_distric_sociodemographics.dta"
drop _merge


* Add the state names to all observations

gen stateid= int(geoid/100000)

merge m:1 stateid using "`path'/state_name_code.dta"
drop if _m!=3
drop _m
replace ban_counts=0 if ban_counts==.
lab var ban_counts "number of bans per district"


gen ban=0
replace ban=1 if ban_counts!=0
lab var ban "binary variable, indicates if the district has banned any book"

destring pdp* dp*, replace force

save "`path'/ban_and_demog.dta", replace


 graph bar (sum) ban , ///
	over(state_name, sort(1) descending label(labsize(*0.8) angle(90))) ///
	ytitle("number of district with at least one ban") graphregion(color(white)) 
	
graph save Graph "`path'/number_district_with_bans.gph", replace



*DEFINE BETTER LABELS FOR THE VARIABLES
*race
lab var dp05_37pct "Total population, one race, White (%)"
lab var pdp05_23pct "Parents, one race, White(%)"

*gender
lab var dp05_3pct	"Total population; Female(%)"
lab var pdp05_3pct	"Parents; Female(%)"

*income
lab var dp03_62est	"Total population; Median household income (1k dollars)"
replace dp03_62est=dp03_62est/1000
lab var pdp03_47est	"Parents; Median earnings for workers (1k dollars)"
replace pdp03_47est=pdp03_47est/1000

*education attainament
lab var dp02_65pct	"Total population, +25y, Bachelor's degree"
lab var pdp02_35pct	"parents, +25y, Bachelor's degree"





*GRAPHS

global aes "graphregion(color(white)) xtitle ("") xlab(2 "no bans" 5 "bans") note("")"
global opt "graphregion(color(white)) ycommon note("95% confidence intervals")" 

*Graph race
ciplot dp05_37pct, by(ban) name(graph1, replace) $aes
ciplot pdp05_23pct, by(ban) name(graph2, replace) $aes
graph combine graph1 graph2, $opt
graph save banrace, replace 

*Graph gender
ciplot dp05_3pct, by(ban) name(graph3, replace) $aes 
ciplot pdp05_3pct, by(ban) name(graph4, replace) $aes
graph combine graph3 graph4,  $opt
graph save bangender, replace 

*Graph income
ciplot dp03_62est, by(ban) name(graph5, replace) $aes
ciplot pdp03_47est, by(ban) name(graph6, replace) $aes
graph combine graph5 graph6,  $opt
graph save banincome, replace 

*Graph education attainment
ciplot dp02_65pct, by(ban) name(graph7, replace) $aes 
ciplot pdp02_35pct, by(ban) name(graph8, replace)  $aes 
graph combine graph7 graph8,  $opt
graph save baneducation, replace 





