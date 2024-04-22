##########################################################
## this is the first attmept for data cleaning for paper 2
## Intitated at March 5th 2024
## The link to MEPS data documentation is as follow:
## 2019: https://meps.ahrq.gov/data_stats/download_data/pufs/h216/h216doc.shtml#Disability257
##########################################################

# Load libraries

library(MEPS)     
library(survey)
library(tidyverse)
library(haven)
library(labelled)
library(broom)


# Set survey option for lonely PSUs

options(survey.lonely.psu="adjust")


# Load datasets and Rename year-specific variables prior to combining  --------------------------- 


for (year in 2011:2019) {
    
    assign(paste0("fyc", year),
    read_MEPS(year = year, type = "FYC"))  ## loading dataset
    
    
    dataset_name <- paste0("fyc", year)
    new_dataset_name <- paste0("fyc", year)
    
    assign(new_dataset_name,
           get(dataset_name) %>%
               rename(
                   PERWT  = paste0("PERWT", substr(year, 3, 4), "F"), # substr(), the function extracts a substring from the year, starting from the third 
                                                                        #character position and ending at the fourth character position
                                                                           
                   AGE = paste0("AGE", substr(year, 3, 4), "X"),
                   MARRYX = paste0("MARRY", substr(year, 3, 4), "X"),
                   POVCAT = paste0("POVCAT", substr(year, 3, 4)),
                   POVLEV = paste0("POVLEV", substr(year, 3, 4) ),
                   REGION = paste0("REGION", substr(year, 3, 4)),
                   INSURC = paste0("INSURC", substr(year, 3, 4)), #Insurance
                   MCDEV = paste0("MCDEV", substr(year, 3, 4)), 
                   MCREV = paste0("MCREV", substr(year, 3, 4)), 
                   PRVEV = paste0("PRVEV", substr(year, 3, 4)) , 
                   UNINS = paste0("UNINS", substr(year, 3, 4)) ,
                   INSCOV = paste0("INSCOV", substr(year, 3, 4)) ,
                   PRIV = paste0("PRIV", substr(year, 3, 4))  
               ))
}


fyc2020<-read_MEPS(year = 2020, type = "FYC")

# Keeping only needed variables --------------------------------------------------


fyc19 <- fyc2019 %>% select(
                DUPERSID,DUID,PANEL, PID,PSTATS31 , PSTATS42 , PSTATS53 , PERWT, VARSTR, VARPSU,SPOUID31, # IDS and weights
                AGE, SEX, RACETHX,MARRYX, FAMS1231, EDUCYR,  EMPST31, POVCAT,POVLEV, DOBYY ,REGION,HIDEG, # Demographic
                BORNUSA, YRSINUS, OTHLGSPK,  HWELLSPK, WHTLGSPK, ADLANG42, # Immigration
                INSURC,MCDEV, MCREV, PRVEV, UNINS,INSCOV, PRIV ,#Insurance
                CABREAST, CACERVIX, CACOLON, CAPROSTA, CANCERDX, CAOTHER # cancer diagnoses
     )


fyc18  <- fyc2018 %>% select(
    DUPERSID,DUID,PANEL, PID,PSTATS31 , PSTATS42 , PSTATS53 , PERWT, VARSTR, VARPSU,SPOUID31, # IDS and weights
    AGE, SEX, RACETHX,MARRYX, FAMS1231, EDUCYR,  EMPST31, POVCAT,POVLEV, DOBYY , REGION,HIDEG, # Demographic
    BORNUSA, YRSINUS, OTHLGSPK,  HWELLSPK, WHTLGSPK, ADLANG42, # Immigration
    INSURC,MCDEV, MCREV, PRVEV, UNINS,INSCOV, PRIV ,#Insurance
    CABREAST, CACERVIX, CACOLON, CAPROSTA, CANCERDX, CAOTHER, # cancer diagnoses
    ADUTRM42, ADPAP42,ADPAPG42, #Cervical Cancer
    ADCOLN42, ADCLNS42, ADSGMD42, ADBLDS42, # Colorectal Cancer
    ADBRST42,ADMMGR42 # Breast Cancer
    ADPROS42, PSA53 # Prostate Cancer
)

# There is no OTHLGSPK in 
fyc17  <- fyc2017 %>% select(
    DUPERSID,DUID,PANEL, PID,PSTATS31 , PSTATS42 , PSTATS53 , PERWT, VARSTR, VARPSU,SPOUID31, # IDS and weights
    AGE, SEX, RACETHX,MARRYX, FAMS1231, EDUCYR,  EMPST31, POVCAT,POVLEV, DOBYY ,REGION,HIDEG, # Demographic
    BORNUSA, YRSINUS,  OTHLANG,LANGSPK, HWELLSPE, ADLANG42, # Immigration
    INSURC,MCDEV, MCREV, PRVEV, UNINS,INSCOV, PRIV ,#Insurance
    CABREAST, CACERVIX, CACOLON, CAPROSTA, CANCERDX, CAOTHER # cancer diagnoses
)

fyc16  <- fyc2016 %>% select(
    DUPERSID,DUID,PANEL, PID,PSTATS31 , PSTATS42 , PSTATS53 , PERWT, VARSTR, VARPSU,SPOUID31, # IDS and weights
    AGE, SEX, RACETHX,MARRYX, FAMS1231, EDUCYR,  EMPST31, POVCAT,POVLEV, DOBYY ,REGION,HIDEG, # Demographic
    BORNUSA, YRSINUS, OTHLANG,LANGSPK, HWELLSPE, ADLANG42, # Immigration
    INSURC,MCDEV, MCREV, PRVEV, UNINS,INSCOV, PRIV ,#Insurance
    CABREAST, CACERVIX, CACOLON, CAPROSTA, CANCERDX, CAOTHER, # Cancer diagnoses
    HYSTER53, PAPSMR53, #Cervical Cancer
    BSTST53, BSTSRE53, CLNTST53, CLNTRE53,SGMTST53,SGMTRE53 , # Colorectal Cancer
    MAMOGR53, # Breast Cancer
    PSA53 # Prostate Cancer:
)


fyc15  <- fyc2015 %>% select(
    DUPERSID,DUID,PANEL, PID,PSTATS31 , PSTATS42 , PSTATS53 , PERWT, VARSTR, VARPSU,SPOUID31, # IDS and weights
    AGE, SEX, RACETHX,MARRYX, FAMS1231, EDUCYR,  EMPST31, POVCAT,POVLEV, DOBYY ,REGION,HIDEG, # Demographic
    BORNUSA, YRSINUS, OTHLANG,LANGSPK, HWELLSPE, ADLANG42, # Immigration
    INSURC,MCDEV, MCREV, PRVEV, UNINS,INSCOV, PRIV ,#Insurance
    CABREAST, CACERVIX, CACOLON, CAPROSTA, CANCERDX, CAOTHER, # cancer diagnoses
    HYSTER53, PAPSMR53, #Cervical Cancer
    BSTST53, BSTSRE53, CLNTST53, CLNTRE53,SGMTST53,SGMTRE53 , # Colorectal Cancer
    MAMOGR53, # Breast Cancer
    PSA53 # Prostate Cancer:
)

# There was no  HIDEG in this year data there is info about in documanation a subsittue variable used called
# EDRECODE read further in : 
# https://meps.ahrq.gov/data_stats/download_data/pufs/h155/h155doc.shtml#2582FamilyOrigins
fyc14  <- fyc2014 %>% select(
    DUPERSID,DUID,PANEL, PID,PSTATS31 , PSTATS42 , PSTATS53 , PERWT, VARSTR, VARPSU,SPOUID31, # IDS and weights
    AGE, SEX, RACETHX,MARRYX, FAMS1231, EDUCYR,  EMPST31, POVCAT,POVLEV, DOBYY ,REGION, EDUYRDG , # Demographic
    BORNUSA, YRSINUS, OTHLANG,LANGSPK, HWELLSPE, ADLANG42, # Immigration
    INSURC,MCDEV, MCREV, PRVEV, UNINS,INSCOV, PRIV ,#Insurance
    CABREAST, CACERVIX, CACOLON, CAPROSTA, CANCERDX, CAOTHER, # cancer diagnoses
    HYSTER53, PAPSMR53, #Cervical Cancer
    BSTST53, BSTSRE53, CLNTST53, CLNTRE53,SGMTST53,SGMTRE53 , # Colorectal Cancer
    MAMOGR53, # Breast Cancer
    PSA53 # Prostate Cancer:
)

# Neither EDUCYR nor HIDEG exist in this dataset instead there are: EDUYRDG, EDRECODE
fyc13  <- fyc2013 %>% select(
    DUPERSID,DUID,PANEL, PID,PSTATS31 , PSTATS42 , PSTATS53 , PERWT, VARSTR, VARPSU,SPOUID31, # IDS and weights
    AGE, SEX, RACETHX,MARRYX, FAMS1231, EDUYRDG,  EMPST31, POVCAT,POVLEV, DOBYY ,REGION, EDRECODE, # Demographic
    BORNUSA, YRSINUS, OTHLANG,LANGSPK, HWELLSPE, ADLANG42, # Immigration
    INSURC,MCDEV, MCREV, PRVEV, UNINS,INSCOV, PRIV ,#Insurance
    CABREAST, CACERVIX, CACOLON, CAPROSTA, CANCERDX, CAOTHER, # cancer diagnoses
    HYSTER53, PAPSMR53, #Cervical Cancer
    BSTST53, BSTSRE53, CLNTST53, CLNTRE53,SGMTST53,SGMTRE53 , # Colorectal Cancer
    MAMOGR53, # Breast Cancer
    PSA53 # Prostate Cancer:
)

## Name change in US born and years lived in US, I dodn't rename it yet because there might be differences
fyc12  <- fyc2012 %>% select(
    DUPERSID,DUID,PANEL, PID,PSTATS31 , PSTATS42 , PSTATS53 , PERWT, VARSTR, VARPSU,SPOUID31, # IDS and weights
    AGE, SEX, RACETHX,MARRYX, FAMS1231, EDUCYR,  EMPST31, POVCAT,POVLEV, DOBYY ,REGION,HIDEG, # Demographic
    USBORN42, USLIVE42, ENGCMF42,ENGSPK42, LANGHM42, ADLANG42, # Immigration
    INSURC,MCDEV, MCREV, PRVEV, UNINS,INSCOV, PRIV ,#Insurance
    CABREAST, CACERVIX, CACOLON, CAPROSTA, CANCERDX, CAOTHER, # cancer diagnoses
    HYSTER53, PAPSMR53, #Cervical Cancer
    BSTST53, BSTSRE53, CLNTST53, CLNTRE53,SGMTST53,SGMTRE53 , # Colorectal Cancer
    MAMOGR53, # Breast Cancer
    PSA53 # Prostate Cancer:
)

# In addition to the US-Born and everything there is a difference with RACE variable as well 
fyc11  <- fyc2011 %>% select(
    DUPERSID,DUID,PANEL, PID,PSTATS31 , PSTATS42 , PSTATS53 , PERWT, VARSTR, VARPSU,SPOUID31, # IDS and weights
    AGE, SEX, RACEX,HISPANX, MARRYX, FAMS1231, EDUCYR,  EMPST31, POVCAT,POVLEV, DOBYY ,REGION,HIDEG, # Demographic
    USBORN42, USLIVE42, ENGCMF42,ENGSPK42, LANGHM42, ADLANG42, # Immigration
    INSURC,MCDEV, MCREV, PRVEV, UNINS,INSCOV, PRIV ,#Insurance
    CABREAST, CACERVIX, CACOLON, CAPROSTA, CANCERDX, CAOTHER, # cancer diagnoses
    HYSTER53, PAPSMR53, #Cervical Cancer
    BSTST53, BSTSRE53, CLNTST53, CLNTRE53,SGMTST53,SGMTRE53 , # Colorectal Cancer
    MAMOGR53, # Breast Cancer
    PSA53 # Prostate Cancer:
)


