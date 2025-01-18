

library(did)
data(mpdta)


mw.attgt <- att_gt(yname = "lemp",
                   gname = "first.treat",
                   idname = "countyreal",
                   tname = "year",
                   xformla = ~1,
                   est_method = 'reg',
                   data = mpdta
)

summary(mw.attgt)
 
mp<-
    aggte(mw.attgt, type = "dynamic")
aggte(mw.attgt, type = "simple")
aggte(mw.attgt, type = "group")
aggte(mw.attgt, type = "calendar")


summary(mw.dyn)


library(fixest)

Data$ttot<-ifelse(Data$expansion == "Adopted", Data$YEAR - Data$ExpansionY, 0)

mpdta$ttot<-ifelse(mpdta$treat == 1, mpdta$year - mpdta$first.treat, 0)


Event1 = feols(lemp ~ i(ttot, treat, ref = -4)  | 
                   year+countyreal,
               vcov = "hetero" ,                   ## FEs
               data = mpdta)


mpdta$TR<-ifelse(mpdta$year>=mpdta$first.treat & mpdta$first.treat!=0 , 1,0)

feols(lemp ~ TR | year+countyreal,  vcov = "hetero", data= mpdta)

