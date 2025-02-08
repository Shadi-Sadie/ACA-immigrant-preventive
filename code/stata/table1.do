

keep if year < 2014 & age >= 24 & age <= 64


* for cat 
foreach var in insurc female marryx educ povcat empst31 rthlth yrsinus eng {
	replace `var' =. if `var'==-15
}

levelsof forgnborn, local(levels)
local forgnbornlabel : value label forgnborn
local counter = 0
local filemethod= "replace"
local heading= ""

local catvars insurc female marryx educ povcat empst31 rthlth yrsinus eng
foreach level of local levels {
	
	if `counter' > 0{
			local filemethod = "append"
			local heading = "h1 (nil) h2(nil)"
	}

	local vlabel : label `forgnbornlabel' `level'
	tabout `catvars' adopted if forgnborn == `level' using "$output/table1_cat.csv", ///
	c(col ) format(2) clab(Col_% ) svy stats(chi2) npos(lab) `filemethod' ///
		`heading' h3("Nativity: `vlabel'")
	
	local counter = `counter' + 1

}
 

*for cont
levelsof forgnborn, local(levels)
local forgnbornlabel : value label forgnborn
eststo clear

foreach level of local levels {
	local vlabel : label `forgnbornlabel' `level'
	display "`vlabel'"
svy, subpop(if forgnborn==`level'): mean age, over(adopted)
test age#1.adopted = age#0.adopted
eststo, addscalars(p_diff r(p), replace)
}
esttab using $output/tab1.csv, replace cells(b(fmt(2)) se(fmt(2))) scalars(p_diff) nonumbers mtitles ("`forgnbornlabel'") title( "MEAN Age over expansion and non expansion by nativity")



levelsof forgnborn, local(levels)
local forgnbornlabel : value label forgnborn
eststo clear

foreach level of local levels {
	local vlabel : label `forgnbornlabel' `level'
	display "`vlabel'"
svy, subpop(if forgnborn==`level'): mean ipc, over(adopted)
test ipc#1.adopted = ipc#0.adopted
eststo, addscalars(p_diff r(p), replace)
}
esttab using $output/tab1.csv, append cells(b(fmt(2)) se(fmt(2))) scalars(p_diff) nonumbers mtitles ("`forgnbornlabel'") title( "MEAN IPC over expansion and non expansion by nativity")


levelsof forgnborn, local(levels)
local forgnbornlabel : value label forgnborn
eststo clear

foreach level of local levels {
	local vlabel : label `forgnbornlabel' `level'
	display "`vlabel'"
svy, subpop(if forgnborn==`level'): mean unempr, over(adopted)
test unempr#1.adopted = unempr#0.adopted
eststo, addscalars(p_diff r(p), replace)
}
esttab using $output/tab1.csv, append cells(b(fmt(2)) se(fmt(2))) scalars(p_diff) nonumbers mtitles ("`forgnbornlabel'") title( "MEAN state unemplyment rate over expansion and non expansion by nativity")
