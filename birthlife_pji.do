*Coded by: Brian Buh
*Created on: 10.02.2021
*Updated on: 11.02.2021
*Updated for new PJI model: 13.07.2021
/* This do file was created to use the lpi package created by Busetta and Mendola
The data was manipulated in R using the R files present here */

cd "S:\r_projects\Emp_Unc_Fertility_Birthlife\pji_busetta_mendola"

which lpi

use panel_pji.dta

label variable pidp "pidp"
label variable yr "year"
label variable mn_amt "Number of observed months in year"
label variable mn_unemp "Number of months unemployed"
label variable unemp "Binary year with jobless spell"
label variable emp_ratio "Ratio of months jobless"
label variable time "Spell number"

rename pidp id
rename unemp poor
rename emp_ratio pgap

lpi poor, se(se) ee(ee) se_ee(se_ee) pgap(pgap) id(id) time(yr) alpha(1)

save "S:\PJI Busetta Mendola\panel_pji_3yr_run.dta", replace
