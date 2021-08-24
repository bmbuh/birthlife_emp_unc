#Coded by: Brian Buh
#Started on: 24.08.2021
#Last Updated: 

### This script was created as an overflow script for the testing of models using a PJI from the first 2 years 
#post-education. The final decision to use three years made the code not part of the final project.

library(data.table)
library(tidyverse)
library(haven)
library(effects)
library(huxtable) #for the export_summs function
library(sandwich) #for the export_summs function, robust SE
library(officer) #for the export_summs function, export to word
library(flextable) #for the export_summs function, export to word
library(modelsummary)
library(jtools)
library(broom.mixed)
library(arsenal)
library(survival)
# library(plyr)

surv5 <- file.choose()
surv5 <- readRDS(surv5)

#Making a DF for the PJI2

#PJI First 2 years post-education
pji_2yr <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/pji_busetta_mendola/panel_pji_2yr_run.dta")

pji_2yr_2 <- pji_2yr %>% 
  rename("pidp" = "id", "pji2" = "se_ee") %>% 
  select(pidp, pji2) %>% 
  filter(!is.na(pji2))

#DF for 2 years after the end of education
surv7 <- surv5 %>% 
  filter(t1 >= 24) %>% 
  left_join(. , pji_2yr_2, by = "pidp")

#Men
surv7m <- surv7 %>% filter(sex == 1) 
surv7m %>% count(event)
#Women
surv7f <- surv7 %>% filter(sex == 2)
surv7f %>% count(event)


###########################################################################
# Full Sample - 2 Years ---------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Full Sample Model Run ---------------------------------------------------
# -------------------------------------------------------------------------

####Model for men
baseline_mglm2 <- glm(formula = event ~ t2,
                      family = binomial(link = "cloglog"),
                      data = surv7m)
summ(baseline_mglm2, exp = TRUE, scale = TRUE)
mglm2 <- glm(formula = event ~ t2 + agemn + agesq + pji2 + finnow3cat + finfut.imp + employed + edu,
             family = binomial(link = "cloglog"),
             data = surv7m)
summary(mglm2)
summ(mglm2, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

####Model for women
baseline_fglm2 <- glm(formula = event ~ t2,
                      family = binomial(link = "cloglog"),
                      data = surv7f)
summ(baseline_fglm2, exp = TRUE)
fglm2 <- glm(formula = event ~ t2 + agemn + agesq + pji2 + finnow3cat + finfut.imp + employed + edu,
             family = binomial(link = "cloglog"),
             data = surv7f)
summary(fglm2)
summ(fglm2, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

# -------------------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# -------------------------------------------------------------------------

export_summs(mglm2,fglm2,
             model.names = c("Men", "Women"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2",
                       "PJI" = "pji2",
                       "Finding it difficult" = "finnow3catfinddifficult",
                       "Getting by" = "finnow3catgetby",
                       "Employed" = "employed",
                       "Worse off" = "finfut.impWorse off",
                       "Better off" = "finfut.impBetter off",
                       "Education Low" = "edulow",
                       "Education Medium" = "edumedium",
                       "Age in Months" = "agemn",
                       "Age Squared" = "agesq"),
             exp = TRUE,
             to.file = "docx",
             file.name = "full_model_paper1_2yrs_21-07-21.docx")




###########################################################################
# Employed Sample - 2 Years -----------------------------------------------
###########################################################################


# -------------------------------------------------------------------------
# Making a dataset fit for new models -------------------------------------
# -------------------------------------------------------------------------

#DF for 2 years after the end of education
survemp2 <- survemp %>% 
  filter(t1 >= 24) %>% 
  left_join(. , pji_2yr_2, by = "pidp") 

#Men
survemp2m <- survemp2 %>% filter(sex == 1) 
#Women
survemp2f <- survemp2 %>% filter(sex == 2)


# -------------------------------------------------------------------------
# Employed Sample Model Run -----------------------------------------------
# -------------------------------------------------------------------------

####Model for men
empmglm <- glm(formula = event2 ~ t2_3 + agemn + agesq + pji2 + jbsec.dummy + permcon + isco + edu,
               family = binomial(link = "cloglog"),
               data = survemp2m)
summary(empmglm)
summ(empmglm, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

####Model for women
empfglm <- glm(formula = event2 ~ t2_3 + agemn + agesq + pji2 +  jbsec.dummy + permcon + isco +  edu,
               family = binomial(link = "cloglog"),
               data = survemp2f)
summary(empfglm)
summ(empfglm, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

survemp2f %>% count(jbsec.dummy)

# -------------------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# -------------------------------------------------------------------------
export_summs(empmglm,empfglm,
             model.names = c("Men", "Women"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2_3",
                       "PJI - 2yrs" = "pji2",
                       "Job Security" = "jbsec.dummy1",
                       "Permanent Contract" = "permcon1",
                       "Education Low" = "edulow",
                       "Education Medium" = "edumedium",
                       "Age in Months" = "agemn",
                       "Age Squared" = "agesq"),
             exp = TRUE,
             to.file = "docx",
             file.name = "employed_model_output_2yrs_21-07-21.docx")
