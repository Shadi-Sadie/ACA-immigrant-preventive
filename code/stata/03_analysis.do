
cap log close
log using "$output/codebook.txt", replace text
codebook
log close

use "$data/raw/MEPS_PUBLIC.dta"

global fullvars SEX         EMPST31     EDUC        MCDEV       PRIV        CANCERDX    CHDDX       ADRISK42    PHYEXE53    IPC           RACETHX     POVCAT      BORNUSA     MCREV    CABREAST 

   ** CAOTHER     ARTHDX      ADOVER42    poolwt      UnempR PSTATS31    VARPSU      HISPANX     POVLEV    YRSINUS     PRVEV       CACERVIX    HIBPDX      CLNTST53    HAVEUS42    Adopted PSTATS42    SPOUID31 MARRYX      ENG         UNINS       CACOLON     DIABDX      SGMTST53    RTHLTH42    ExpansionY  DUID        PSTATS53    AGE         FAMS1231    REGION      INSURC      INSCOV      CAPROSTA    ASTHDX      MAMOGR53    ADSMOK42    treat */
fd

 
table ExpansionY BORNUSA

******************************
** Create Summary Table for **
******************************

*** preparing dataset

** summerize before 2014 
** Aged 25-64 

keep if year < 2014 & age >= 24 & age <= 64


save "$temp/beforeaca.dta", replace



*** create table 
use "$temp/beforeaca.dta", replace

levelsof BORNUSA, local(levels)
local BORNUSAlabel : value label BORNUSA
local counter = 0
local filemethod= "replace"
local heading= ""


foreach level of local levels {
	
	if `counter' > 0{
			local filemethod = "append"
			local heading = "h1 (nil) h2(nil)"
	}

	local vlabel : label `BORNUSAlabel' `level'
	tabout SEX treat if BORNUSA == `level' using "$output/table1.xls", ///
	c(col ) format(2) clab(Col_% ) svy stats(chi2)  `filemethod' ///
		`heading' h3("BORNUSA: `vlabe'")
	
	local counter = `counter' + 1

}

*****************************

