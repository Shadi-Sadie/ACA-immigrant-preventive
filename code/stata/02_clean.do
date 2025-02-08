* further cleaning

** create variable expansion year 

gen ex_year = cond(expansion ==1, year, . )
bysort f00011x: egen exp_year = min(ex_year)
replace exp_year = 0 if exp_year ==.

** create variable ever treated

gen ever_exp = (exp_year < .)
tab ever_exp
list f00011x expansion exp_year

** create variable for colon cancer screening

g cscreen = . 
replace cscreen =1  if clntst53==1 | sgmtst53==1
replace cscreen = 0 if clntst53 != 1 & sgmtst53 != 1 & !missing(clntst53) & !missing(sgmtst53)

** create variable for chronic health condition

gen chrnc_cond = 0
replace chrnc_cond = 1 if diabdx ==1 | asthdx==1 | hibpdx ==1 | chddx==1 | arthdx==1


** change the value of -15 representing missing to missing 


foreach var in  educ empst31 rthlth forgnborn yrsinus eng {
replace `var' =. if `var'==-15
}

foreach var in  eng {
replace `var' =. if `var'==-1
}


** create dummy form categorical variables
g race_hp = racethx == 1
g race_wht = racethx == 2
g race_blk = racethx == 3
g race_asn = racethx == 4
g race_oth = racethx == 5

g edu_none = educ == 1
g edu_hs = educ == 2
g edu_ba = educ == 3
g edu_maphd = educ == 4

** fix the dummy vars 

replace unins = 0 if unins==2 
replace mcdev = 0 if mcdev==2 


destring ipc_index, replace


** create state variable 
encode f00011x, gen(state)


** for total expenditure works
 
 gen year_cpi = .
replace year_cpi = 241.213 if year == 2011
replace year_cpi = 253.563 if year == 2012
replace year_cpi = 265.448 if year == 2013
replace year_cpi = 278.754 if year == 2014
replace year_cpi = 290.132 if year == 2015
replace year_cpi = 303.260 if year == 2016
replace year_cpi = 318.170 if year == 2017
replace year_cpi = 332.237 if year == 2018
replace year_cpi = 338.814 if year == 2019

// Step 2: Define the base CPI for 2019
local base_cpi = 338.814

// Step 3: Calculate the adjusted expenditure
gen adj_texp = totexp * (`base_cpi' / year_cpi)



save "$data/rawmeps3", replace


