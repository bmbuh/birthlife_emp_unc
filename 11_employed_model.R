#Coded by: Brian Buh
#Started on: 26.05.2021
#Last Updated: 27.05.2021

surv5 <- file.choose()
surv5 <- readRDS(surv5)

library(data.table)
library(tidyverse)
library(haven)
library(effects)

surv5 %>% count(jbsec)

survemp <-  surv5 %>% 
  mutate(fb = ifelse(is.na(kdob), 0, 1)) %>% 
  filter(wave == 2 | wave == 4 | wave == 6 | wave == 8 | wave == 10) %>% 
  mutate(jbsec = as.character(jbsec)) %>% 
  mutate(jbsec = ifelse(jbsec == "3 non-employed", NA, jbsec)) %>% 
  filter(!is.na(jbsec)) %>% 
  mutate(jbsec.dummy = ifelse(jbsec == "1 likely", 1, 0)) %>% 
  mutate(jbsec.dummy = as.character(jbsec.dummy)) %>% 
  select(-jbsec.num, -hhorig, -neg, -negstu) %>% 
  arrange(pidp, desc(wave)) %>% 
  group_by(pidp) %>% 
  mutate(eventcheck = row_number()) %>% 
  mutate(event2 = ifelse(eventcheck == 1 & fb == 1, 1, 0)) %>% 
  ungroup() %>% 
  arrange(pidp, wave) %>% 
  select(-eventcheck)
  
  

survemp %>% count(event2)  
str(survemp)
testglm <- glm(formula = event ~ t2 + agemn + agesq + se_ee + jbsec.dummy + parttime + edu_cat,
               family = binomial(link = "cloglog"),
               data = survemp)
summary(testglm)
summ(testglm, exp = TRUE, scale = TRUE) #exp = TRUE means that we want exponentiated estimates



###########################################################################
# Transform data set to monthly observations ------------------------------
###########################################################################

#This code is takes from script 3 to be used here if needed
pji1 <- transform(pji_sample, from = as.Date(start_date), to = as.Date(end_date), lagfb = as.Date(lagged_kdob))

pji2 <- pji1 %>%
  dplyr::select(pidp, unemp, to, from, lagfb, age45f, age50m, sex, hhorig, dob, kdob)

dt <- data.table(pji2)
pji3 <- dt[, list(pidp, unemp, lagfb, age45f, age50m, sex, hhorig, dob, kdob, date = seq(from, to, by = "month")), by = 1:nrow(dt)] %>% 
  mutate(age_start = (dob %--% date)/dyears(1)) %>% 
  filter(age_start >= 16) 
