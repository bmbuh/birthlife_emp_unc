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

testmultglm <- glmer(formula = event ~ t2 + se_ee + finnow.num + finfut.num + (1|pidp),
                     family = binomial(cloglog),
                     data = surv2,
                     control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl = list(maxfun = 2e5)))

summary(testmultglm)

#Test with newly transformed times

coxph <- coxph(formula = Surv(t1, t2, event) ~ se_ee + agemn + agesq + finnow.num + finfut.num + edu_cat, data = surv2, cluster = pidp, method = "breslow")
summary(coxph)
testph <- cox.zph(coxph)
summary(testph)

kmtest <- survfit(Surv(t1, t2, event) ~ strata (sex), data = surv2, cluster = pidp)
summary(kmtest)
plot(kmtest)

ggsurvplot(kmtest, size = 1,   # change line size
           #ylim = c(0.69,1),
           # palette = c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = TRUE,          # Add confidence interval
           # pval = TRUE,              # Add p-value
           risk.table = TRUE,        # Add risk table
           # risk.table.col = "strata",# Risk table color by groups
           legend.labs =
           c("Women", "Men"),    # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw()      # Change ggplot2 theme
           ) 
#+ labs(caption = "Survival probability cut at 0.7")
  
  
  
  

