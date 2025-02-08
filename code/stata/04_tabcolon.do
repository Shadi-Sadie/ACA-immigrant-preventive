****
ssc install reghdfe
ssc install ftools

** This is the code for creating table 2 all regression for colonscopy with 

** for colonscopy

cap log close
clear 

use "$data/rawmeps3", clear

************************
* subset for colonscopy
************************
gen subpop = (age>49 & age<65 & povlev<139 & (exp_year==2014 | exp_year==0) & clntst53 != -1 & clntst53 != -15 & forgnborn ==1)

keep if age>49 & age<65
keep if povlev<139

drop if clntst53 == -1 // drop ineligible
drop if sgmtst53 == -1 
drop if clntst53 == -15 // drop missings

* replace clntst53=. if clntst53==-1 | clntst53==-15 // check if result would be different

*for all
*drop if f00011x == "B6"
*drop if f00011x == "A1"
*drop if f00011x == "E0" 
*drop if f00011x == "A6"

*for frgnborn 
*drop if f00011x == "B5"| f00011x == "D6" | f00011x=="F1"| f00011x == "D7" | f00011x == "C5" | f00011x=="D2" | f00011x=="B2" | f00011x=="C0" | f00011x == "A8" | f00011x == "C2" | f00011x =="E8" | f00011x == "E9"


/*drop if f00011x == "A0" | f00011x == "A1"| f00011x =="A6"|f00011x == "A8"| f00011x == "B4"| f00011x == "B5"| f00011x == "B6"|f00011x == "C1" | f00011x == "C3"|f00011x == "C9"|f00011x =="D1"| f00011x == "D6"| f00011x =="D8"|f00011x == "E0" | f00011x == "E1" | f00011x == "E5"| f00011x =="E6" */



************************
* simple 2did
************************

* data prep:

	** subset data for simple did
drop if year==2017 | year==2018 | year == 2019 // cancer screening var is not good for these years 
replace poolwt = poolwt * (9/7) 
drop if exp_year == 2015 | exp_year==2016 | exp_year == 2019 // drop state that adopted later on 

	** create variable for simple did 
gen post = 0
replace post = 1 if year >= 2014
gen  treat = 0 
replace treat = 1 if exp_year == 2014 
gen treated = 0 
replace treated =1 if exp_year == 2014 & year >= 2014
gen post_treat = post*treat
gen forgn_treat = forgnborn*treat
gen forgn_post = forgnborn*post
gen forgn_treat_post = post_treat*forgnborn

	** set survey weigth
svyset [pweight = poolwt], strata (varstr) psu (varpsu) vce(linearized) singleunit(center)
*bsweights bw, reps(5) n(0) seed(10209)

* Simple 2*2 regression 

	** all groups no control

quietly:
a


i.state i.year
svy: reg cscreen post_treat $lesvar i.state i.year
reghdfe cscreen expansion $lesvar [pweight = poolwt], absorb(state year) vce(cluster state)

reghdfe cscreen expansion $lesvar  [pweight = poolwt], noabsorb 
 vce(cluster state)


*reghdfe clntst53 expansion $lesvar , absorb(f00011x year) cluster (state)
*reghdfe clntst53 expansion $lesvar [pweight = poolwt], absorb(state year) vce(cluster state)

outreg2 using $output/table2.xls, replace ctitle (All population) title ( Effect of Medicad expansion on Coloerctal cancer screening with covariates) drop($lesvar) nocons addstat(Population size , e(N_pop)) dec(3)  fmt(gc)

/** these next regessions are all giving the same results as the one used, just for checking what other reg could've been used
svy: reg clntst53 post treat post_treat i.state i.year
svy: reg clntst53 expansion i.state i.year
reghdfe clntst53 expansion  [pweight = poolwt], absorb(state year) vce(cluster state)
reghdfe clntst53 expansion  [weight = poolwt], absorb(f00011x year) cluster (state)
 didregress(cscreen $lesvar) (expansion) [pweight = poolwt], group (state) time (year)
 estat trendplots
	estat ptrend
 **/
 

	** just on foreign-born sample
	**** with control ****
svy: reg cscreen post treat post_treat $lesvar ipc_index eng yrsinus if forgnborn ==1

 
* reghdfe clntst53 expansion $lesvar ipc_index if forgnborn==1 [pweight = poolwt], absorb(state year) vce(cluster state)

outreg2 using $output/table2.xls, ctitle (Foreign Borns)  append drop($lesvar) nocons addstat(Population size , e(N_pop)) dec(3)  fmt(gc)



/*svy, sub(subpop): reg clntst53 post treat post_treat same as previous one 
svy: reg clntst53 post_treat i.state i.year  if forgnborn ==1
didregress(cscreen $allvar ) (expansion)  if forgnborn ==1 [pweight = poolwt], group (state) time (year) wildbootstrap(rseed(111))
 */

	** just on US-born sample
svy: reg cscreen post treat post_treat $lesvar if forgnborn == 0


*reghdfe clntst53 expansion $lesvar if forgnborn==0  [pweight = poolwt], absorb(state year) vce(cluster state)

outreg2 using $output/table2.xls, ctitle (US Borns) dec(3)  append drop($lesvar) nocons addstat(Population size , e(N_pop)) dec(3)  fmt(gc)



/*svy: reg clntst53 post_treat i.state i.year  if forgnborn ==0
didregress(clntst53 ) (expansion)  if forgnborn ==0 [pweight = poolwt], group (state) time (year) */

	** with interaction term

cap gen forgn_expansion = forgnborn*expansion

svy: reg cscreen post treat forgnborn  post_treat forgn_post forgn_treat forgn_treat_post $lesvar ipc_index eng yrsinus
*svy: reg cscreen post treat forgnborn  post_treat forgn_post forgn_treat forgn_treat_post i.forgnborn##($lesvar ) ipc_index eng yrsinus


*svy: reg cscreen i.forgnborn##(i.post##i.treat) i.forgnborn##($lesvar c.ipc_index eng yrsinus) 

*svy: reg clntst53 post treat post_treat forgnborn forgn_treat_post  // (this might be wrong)

* svy: reg clntst53 i.forgnborn##(i.expansion i.state i.year c.ipc_index $lesvar ) yrsinus eng
* reg clntst53 i.forgnborn##(i.expansion i.state i.year) $lesvar

outreg2 using $output/table2.xls, drop(i.state i.year i.forgnborn##i.state i.forgnborn##i.year) ctitle (All population interacted-1) drop($lesvar) nocons addstat(Population size , e(N_pop)) dec(3)  fmt(gc)
  append


*reghdfe clntst53 expansion forgn_expansion forgnborn $lesvar  [pweight = poolwt], absorb(state year) vce(cluster state)

*reghdfe clntst53 expansion forgn_expansion forgnborn age forgnborn##(c.ipc_index i.racethx marryx empst31 educ), absorb(state year) vce(cluster state)

outreg2 using $output/table2.xls, ctitle (All population interacted-2) dec(3)  append


/*svy: reg clntst53 post treat forgnborn  post_treat forgn_post forgn_treat forgn_treat_post i.state i.year

reghdfe clntst53 expansion expansion##forgnborn [pweight = poolwt], absorb(state year) vce(cluster state)
same as
svy: reg clntst53 post treat post_treat forgnborn forgn_treat_post i.state i.year // (this might be wrong)

svy: reg clntst53 i.forgnborn##i.expansion i.state i.year
same as 
reghdfe clntst53 expansion forgn_expansion forgnborn  [pweight = poolwt], absorb(state year) vce(cluster state)


svy: reg clntst53 post treat forgnborn  post_treat forgn_post forgn_treat forgn_treat_post i.state i.year
*/

** furhter investigation with hispanic population 

cap g f_hisp = 0
replace f_hisp = 1 if race_hp==1 & forgnborn == 1

cap g f_other = 0
replace f_other = 1 if racethx!=1 & forgnborn ==1 

cap g fhisp_post_treat = f_hisp*expansion
cap g fother_post_treat = f_other*expansion


* reghdfe clntst53 expansion f_hisp f_other fhisp_exp fother_exp  [pweight = poolwt], absorb(state year) vce(cluster state)

*svy: reg clntst53  i.f_other##(i.expansion i.state i.year) 

*reghdfe clntst53 expansion fhisp_exp f_hisp#post  [pweight = poolwt], absorb(state year) vce(cluster state)

*svy: reg cscreen i.forgnborn##(i.post##i.treat) i.forgnborn##($lesvar c.ipc_index eng yrsinus) 

svy: reg cscreen i.f_hisp##i.post i.f_other##i.post i.treat##i.post i.f_hisp##i.treat##i.post i.f_other##i.treat##i.post $lesvar ipc_index eng yrsinus

svy: reg cscreen post treat forgnborn   post_treat forgn_post forgn_treat fhisp_post_treat fother_post_treat $lesvar ipc_index eng yrsinus

outreg2 using $output/table2.xls,  ctitle (All population interacted-2) drop($lesvar) nocons addstat(Population size , e(N_pop)) dec(3)  fmt(gc) append


* on foreign-born hispanic
*svy: reg clntst53 post_treat i.state i.year  if f_other ==1

