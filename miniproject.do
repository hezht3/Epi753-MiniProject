***************** Epi 753 M/W Table 10 Mini Project *****************

log using "miniproject.log", replace


// set working directories
global root "D:\OneDrive - Johns Hopkins\Course\340.753.01 - Epidemiologic Methods 3\Lab\MiniProject\miniproject_stata"
* define path for data sources
global DATA "${root}/data"
* define path for output data
global OUT "${root}/output"


use "${DATA}/project_c_copy-3.dta", clear


* dichotomize anemia
gen anemia = .
replace anemia = 1 if hb <13
replace anemia = 0 if hb >= 13


***************** Table 1 *****************

foreach x in age bmi {
	tabstat `x', stat(mean sd)
}

foreach x in paids prior {
	tab `x' anemia, row
}


***************** crude RR *****************
glm cd4lt200 anemia, family(binomial) link(log) eform


***************** adjusted RR *****************
*glm cd4lt200 anemia age bmi paids prior, family(binomial) link(log) eform   /*fail to converge*/

glm cd4lt200 anemia age bmi paids prior, family(poisson) link(log) robust eform

log close