#Coded by: Brian Buh
#Started on: 12.03.2021
#Last Updated: 

# install.packages("lme4")
# install.packages("survey")

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
