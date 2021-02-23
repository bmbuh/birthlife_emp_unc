#Coded by: Brian Buh
#Started on: 17.02.2021
#Last Updated: 22.02.2021

# install.packages("zoo")
# install.packages("survminer")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/survminer", build_vignettes = FALSE)

library(data.table)
library(padr)
library(tidyverse)
library(haven)
library(lubridate)
library(arsenal)
library(zoo)
library(survival)
library(survminer)


#the first task is filling in missing interview dates from missing internal waves
#This is probably best accomplished by simply picking the month halfway between the existing waves
#This process will be less extensive after eliminating the rows before the first observed waves and after the last observed wave

com_panel <- file.choose()
com_panel <- readRDS(com_panel)

enddate <- com_panel %>% 
  unite(intdate, c(intdatm_dv, intdaty_dv), sep = "-") %>% #combines the mnth & yr var to make int dates
  mutate(intdate = parse_date_time(intdate, "my")) %>% 
  dplyr::select(pidp, wave, intdate)

foo <- function(x) coredata(na.approx(zoo(x), na.rm = FALSE))
enddate2 <- data.table(enddate)
enddate2[, intdate := foo(intdate), by = "pidp"]
# DT[, enddate := foo(enddate), by = "pidp"]

enddate3 <- enddate2 %>% 
  mutate(wave = wave - 1) %>% 
  filter(wave != 0) %>% 
  rename("enddate" = "intdate")

enddate4 <- 
  left_join(enddate, enddate3, by = c("pidp", "wave")) %>% 
  rename("startdate" = "intdate")

com_panel2 <- com_panel %>% 
  mutate(imp = ifelse(is.na(dvage), 1, 0)) %>% 
  left_join(., enddate4, by = c("pidp", "wave")) %>% 
  filter(fwtest >= 0, lwtest >= 0)%>% #removes rows before the first int or after the last
  # unite(intdate, c(intdatm_dv, intdaty_dv), sep = "-") %>% #combines the mnth & yr var to make int dates
  # mutate(intdate = parse_date_time(intdate, "my")) %>% 
  unite(dob, c(birthm, birthy), sep = "-") %>% #creates a dob
  mutate(dob = parse_date_time(dob, "my")) %>% 
  # mutate(age = (intdate - dob)/(365*24*60*60)) %>% #Done so I can use age at each wave
  group_by(pidp) %>% 
  fill(fb_check) %>%
  fill(dvage) %>% 
  fill(gor_dv) %>% 
  fill(racel_dv) %>% 
  fill(generation) %>% 
  ungroup() %>% 
  filter(fb_check == 0 | fb_check == 2) %>% #this variable takes the observed "anychild" and subtracts the binary "kdob oberseved" 1 = had child but no kdob or not had child but observed kdob
  filter(dvage <= 49) %>% 
  dplyr::select(pidp, wave, imp, kdob, sex, dvage, dob, racel_dv, gor_dv, ppid, jbsec, generation,
                edu_cat, se_ee, finnow.imp, finfut.imp, startdate, enddate)


#In order to fill in start dates from missing waves
#Calculated as the median point between the two waves
foo <- function(x) coredata(na.approx(zoo(x), na.rm = FALSE))
com_panel3 <- data.table(com_panel2)
com_panel3[, startdate := foo(startdate), by = "pidp"]
# DT[, enddate := foo(enddate), by = "pidp"]


#Some median dates are halfway between the month
#The "floor_date" function in lubridate didn't work, so this is a function to round to start of month
firstOfMonth <- function(dates) {
  as.Date(strftime(dates, format="%Y-%m-01"))}


com_panel4 <- com_panel3 %>% 
  mutate(startdate = firstOfMonth(startdate)) %>% 
  mutate(enddate = firstOfMonth(enddate)) %>% 
  mutate(gap = as.duration(startdate %--% enddate) / dmonths(1)) %>% 
  mutate(gap = round(gap, digits = 0)) %>% 
  group_by(pidp) %>% 
  mutate(rollgap = cumsum(gap)) %>% 
  mutate(condate = as.Date(kdob %m-% months(9))) %>% 
  mutate(fbgap = as.duration(startdate %--% condate) / dmonths(1))

com_panel5 <- com_panel4 %>% 
  mutate(condate = ifelse(enddate <= condate, NA, condate)) %>% 
  mutate(condate = as.Date(condate)) %>% 
  mutate(condate = ifelse(is.na(condate), 1900-01-01, condate)) %>% 
  mutate(condate = as.Date(condate)) %>% 
  mutate(enddate2 = ifelse(condate <= enddate, condate, enddate)) %>% 
  mutate(enddate2 = ifelse(enddate2 == 1898, enddate, enddate2)) %>% 
  mutate(enddate2 = as.Date(enddate2)) %>% 
  mutate(cut = ifelse(startdate > enddate2, 1, 0)) %>% 
  filter(cut != 1) %>% 
  mutate(gap2 = as.duration(startdate %--% enddate2) / dmonths(1)) %>% 
  mutate(gap2 = round(gap2, digits = 0)) %>% 
  group_by(pidp) %>% 
  mutate(rollgap2 = cumsum(gap2)) %>% 
  mutate(time1 = rollgap2 - gap2) %>% 
  rename("time2" = "rollgap2") %>% 
  mutate(event = ifelse(condate == enddate2, 1, 0)) %>% 
  dplyr::select(-gap2, -cut, -enddate, -gap, -rollgap, -fbgap, -condate) %>% 
  rename("enddate" = "enddate2") %>% 
  mutate(dob = as.Date(dob)) %>% 
  mutate(agemn = as.duration(dob %--% startdate) / dmonths(1)) %>% 
  mutate(agemn = round(agemn, digits = 0)) %>% 
  mutate(agesq = agemn*agemn)
  
com_panel4 %>% 
  ungroup() %>% 
  count(gor_dv)

str(com_panel5)
###########################################################################
# Cox Prop Haz Model ------------------------------------------------------
###########################################################################

#There is an issue where some end dates are the same as the start dates because the baby was born the same month as the interview
#This is solved by adding one month to the end date
com_panel6 <- com_panel5 %>% 
  mutate(test = time2 - time1) %>% 
  mutate(test2 = ifelse(test <= 0, 1, 0)) %>% 
  ungroup() %>% 
  mutate(time2 = test2 + time2) %>% 
  dplyr::select(-test, -test2)

#Save and load the combined individual data file as an RDS
saveRDS(com_panel6, file = "surv.rds")
surv <- file.choose()
surv <- readRDS(surv)

surv %>% 
  count(racel_dv)

msurv <- surv %>% #data set for men
  filter(sex == 1)

fsurv <- surv %>% #data set for women
  filter(sex == 2)

#Kaplan-Meier non-parametric analysis
kmsurv_sex <- survfit(Surv(time1, time2, event) ~ sex, data = com_panel6, cluster = pidp)
summary(kmsurv_sex)
plot(kmsurv_sex, xlab = "Time", ylab = "First Birth Probability by Sex")
ggsurvplot(fit, data = kmsurv_sex)
kmsurv_edu <- survfit(Surv(time1, time2, event) ~ edu_cat, data = com_panel6, cluster = pidp)
summary(kmsurv_edu)
plot(kmsurv_edu, xlab = "Time", ylab = "First Birth Probability by Education")

mcoxph <- coxph(formula = Surv(time1, time2, event) ~ sex + se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat, data = msurv, cluster = pidp, method = "breslow")
summary(mcoxph)
fcoxph <- coxph(formula = Surv(time1, time2, event) ~ sex + se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat, data = fsurv, cluster = pidp, method = "breslow")
summary(fcoxph)

survfit(Surv(time1, time2, event) ~ 1, data = com_panel6, cluster = pidp)


###########################################################################
# Couple data set ---------------------------------------------------------
###########################################################################

partnerid <- surv %>% 
  dplyr::select(ppid, wave) %>% 
  fill(ppid) %>% 
  rename("pidp" = "ppid") %>% 
  left_join(., com_panel, by = c("pidp", "wave"))%>% 
  filter(!is.na(sex)) %>% 
  dplyr::select(pidp, wave, sex, birthm, birthy, racel_dv,
                jbsec, generation, edu_cat, se_ee, finnow.imp, finfut.imp) %>%
  unite(dob, c(birthm, birthy), sep = "-") %>% #creates a dob
  mutate(dob = parse_date_time(dob, "my")) %>% 
  fill(racel_dv, generation) %>% 
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames)


  oldnames = c("pidp", "wave", "sex", "dob", "racel_dv", "jbsec", "generation", 
               "edu_cat", "se_ee", "finnow.imp", "finfut.imp")
  newnames = c("ppid", "wave", "sexp", "dobp", "racel_dvp", "jbsecp", "generationp", 
               "edu_catp", "se_eep", "finnow.impp", "finfut.impp")
  
couplesurv <- 
  left_join(surv, partnerid, by = c("ppid", "wave")) %>% 
  filter(!is.na(sexp)) %>% 
  mutate(dobp = as.Date(dobp)) %>% 
  mutate(agemnp = as.duration(dobp %--% startdate) / dmonths(1)) %>% 
  mutate(agemnp = round(agemnp, digits = 0)) 

fcouplesurv <- couplesurv %>% 
  filter(sex == 2)

couplecoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + se_eep + agemn + agemnp, data = couplesurv, cluster = pidp, method = "breslow")
summary(couplecoxph)

##### Leftovers

time1 <- com_panel5 %>% 
  dplyr::select(pidp, wave, rollgap2) %>% 
  mutate(rollgap3 = )
  
test %>% 
  ungroup() %>% 
  count(cut)
  
  mutate(rollsum = rollapply(gap, FUN = sum, fill = NA))
  


str(com_panel3)


intdate <- com_panel2 %>% 
  dplyr::select(pidp, wave, intdate) %>% 
  mutate(int = "int") %>% 
  unite(int, int, wave, sep = "") %>% 
  pivot_wider(names_from = int, values_from = intdate) 

intdate2 <- intdate %>% 
 mutate(gap1 = interval(int1, int2) %/% months(1))


com_panel2 %>% 
  count(gor_dv)

idlong <- com_panel2 %>% 
  dplyr::select(pidp, wave, intdate) %>% 
  group_by(pidp) %>% 
  summarise("stdate" = min(as.Date(intdate)),
            "eddate" = max(as.character(intdate)))
                
idlong2 <- idlong %>%
  mutate(intdate = as.POSIXct(intdate)) %>% 
  thicken()
  
  group_by(pidp) %>% 
  fill_by_function(fun = median)
  
  
  
  do(pad(.))



%>% 
  fill_by_prevalent()
