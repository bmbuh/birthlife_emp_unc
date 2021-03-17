#Coded by: Brian Buh
#Started on: 12.03.2021
#Last Updated: 16.03.2021

# install.packages("lme4")
# install.packages("survey")
# install.packages("jtools")
# install.packages("ggstance")

library(data.table)
library(padr)
library(tidyverse)
library(haven)
library(lubridate)
library(arsenal)
library(zoo)
library(survival)
library(survminer)
library(survPen)
library(flexsurv)
library(coxme)
library(stargazer)
library(texreg)
library(forestplot)
library(sjPlot)

library(lme4)
library(survey)
library(jtools)
library(ggstance)

###########################################################################
# Discrete Time Model -----------------------------------------------------
###########################################################################

surv <- file.choose()
surv <- readRDS(surv)

survcut <- surv %>% 
  dplyr::select(pidp, time1, event)

#The model is not converging. Look into what can be done about this!
dth <- glmer(event ~ time1 + (1 + time1|pidp),  data = survcut, family = "binomial")
summary(dth)

des <- svydesign(ids = ~pidp, data = surv)
dth2 <- svyglm(event ~ time1, design = des, family = "binomial")
summary(dth2)

coxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee , data = surv, cluster = pidp, method = "exact")


testglm <- glm(formula = event ~time2,
               family = binomial(link = "cloglog"),
               data = surv)

summary(testglm)
#The strong relationship between time 2 and event in this models
#signifies that the baseline hazard is the same for all individuals ( :-) )

summ(testglm, exp = TRUE) #takes a minute to process

testglm2 <- glm(formula = event ~time2 + se_ee + finnow.num + finfut.num,
               family = binomial(link = "cloglog"),
               data = substat)
summary(testglm2)
summ(testglm2, exp = TRUE) #takes a minute to process
summ(testglm2, exp = TRUE, scale = TRUE)
plot_summs(surv, exp = T, scale = T)

testmultglm <- glmer(formula = event ~time2 + se_ee + finnow.num + finfut.num + 
                       agemn + agesq + combo + (1|pidp),
                     family = binomial(cloglog),
                     data = substat,
                     control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl = list(maxfun = 2e5)))

summary(testmultglm)

# Test finished education

edu_his_cut <- edu_his %>% 
  dplyr::select(pidp, start_date, end_date) %>% 
  group_by(pidp) %>% 
  mutate(spellnum = row_number()) %>% 
  arrange(pidp, desc(spellnum)) %>% 
  mutate(resnum = row_number()) %>% 
  filter(resnum == 1) %>% 
  dplyr::select(pidp, end_date)

surv_edu <- surv %>% 
  left_join(., edu_his_cut, by = "pidp") %>% 
  mutate(gap = as.duration(startdate %--% end_date) / dmonths(1))
  
  
  
  
  
  

