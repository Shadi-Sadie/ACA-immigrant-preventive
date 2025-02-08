*-------------------------------------------------------
* Project: Medicaid Expansion Impact on health and access
* Author: Shadi Seyedi 
* Date: Feb 7, 2025
* Purpose: Master file to manage entire project workflow 
* Stata version 18
*-------------------------------------------------------

clear all 
set more off 
set type double, permanently
cls

**********************************
* Setting path and directories
**********************************


* path to all necessary packages

adopath + "p:\ado"
 
* set global paths

global dofile  "D:\Users\shadi.seyedi\ACA-preventive\codes"
global data  "D:\Users\shadi.seyedi\ACA-preventive\data"
global datatemp  "D:\Users\shadi.seyedi\ACA-preventive\datatemp"
global output  "D:\Users\shadi.seyedi\ACA-preventive\output"
global log  "D:\Users\shadi.seyedi\ACA-preventive\log"



**********************************
* Define global variable names 
**********************************
global allvar  race_hp race_wht race_blk race_asn race_oth marryx empst31  edu_ba edu_hs edu_maphd edu_none  unins  yrsinus eng c.ipc_index c.unemployment_rate 
global fullvar age i.racethx marryx empst31  i.educ  unins rthlth  yrsinus eng ipc_index unemployment_rate
global usvar age yrsinus eng 
global frvar i.marryx empst31 i.educ unins ipc_index
global lesvar  age i.racethx marryx empst31  i.educ 
global usvar age i.racethx marryx empst31  i.educ  unins rthlth unemployment_rate 

**********************************
* Runing data import, merging &
*  cleaning 
**********************************

* Meriging data set

do $dofile/01_merge.do

do $dofile/02_clean.do


**********************************
* Runing the main analysis
**********************************

* Table 1: Balance table

* Table 2: Colon cancer  table conditional: all onsubset with interaction foreign-born and foreignborn_hisp: on  eligible for medicaid low income <139  aged 50-65 

* Table 3: Breast cancer : unconditional conditional with foreign-born and foreignborn_hisp eligible for medicaid low income <139  aged 50-65 

**********************************
* Runing the sensitivity analysis
**********************************

* Table 4: Colon cancer for people aged 65-75 


* Table 5: Breast cancer for people aged 65-75


**********************************
* Further analysis
**********************************

* graphs or event studies 

* Table A1: Colon cancer  table unconditional: all onsubset with interaction foreign-born and foreignborn_hisp: on  eligible for medicaid low income <139  aged 50-65 

