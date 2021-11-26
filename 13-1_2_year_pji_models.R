#Coded by: Brian Buh
#Started on: 24.08.2021
#Last Updated: 10.11.2021

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

#Number of events at various cut offs years
surv5 %>% count(event)

surv5_1 <- surv5 %>% 
  filter(t1 >= 12)
surv5_1 %>% count(event)

surv5_1_5 <- surv5 %>% 
  filter(t1 >= 18)
surv5_1_5 %>% count(event)

surv5_2 <- surv5 %>% 
  filter(t1 >= 24)
surv5_2 %>% count(event)

surv5_2_5 <- surv5 %>% 
  filter(t1 >= 30)
surv5_2_5 %>% count(event)

surv5_3_5 <- surv5 %>% 
  filter(t1 >= 42)
surv5_3_5 %>% count(event)

surv5_4 <- surv5 %>% 
  filter(t1 >= 48)
surv5_4 %>% count(event)

surv5_4_5 <- surv5 %>% 
  filter(t1 >= 54)
surv5_4_5 %>% count(event)

surv5_5 <- surv5 %>% 
  filter(t1 >= 60)
surv5_5 %>% count(event)


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
# Full Sample - 3 Years ---------------------------------------------------
###########################################################################

#Making a DF for the PJI2

#PJI First 2 years post-education
pji_3yr <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/pji_busetta_mendola/panel_pji_3yr_run.dta")

pji_3yr_2 <- pji_3yr %>% 
  rename("pidp" = "id", "pji3" = "se_ee") %>% 
  select(pidp, pji3) %>% 
  filter(!is.na(pji3))

#DF for 2 years after the end of education
# surv6 <- surv5 %>% 
#   filter(t1 >= 36) %>% 
#   left_join(. , pji_3yr_2, by = "pidp")

#Men
surv6m <- surv6 %>% filter(sex == 1) %>% mutate(employed = as.numeric(employed))
surv6m %>% count(event)
#Women
surv6f <- surv6 %>% filter(sex == 2) %>% mutate(employed = as.numeric(employed))
surv6f %>% count(event)

# -------------------------------------------------------------------------
# Full Sample Model Run ---------------------------------------------------
# -------------------------------------------------------------------------

####Model for men
baseline_mglm3 <- glm(formula = event ~ t2,
                      family = binomial(link = "cloglog"),
                      data = surv6m)
summ(baseline_mglm3, exp = TRUE, scale = TRUE)
mglm3 <- glm(formula = event ~ t2 + agemn + agesq + pji3 + finnow3cat + finfut.imp + employed + edu,
             family = binomial(link = "cloglog"),
             data = surv6m)
summary(mglm3)
summ(mglm3, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

####Model for women
baseline_fglm3 <- glm(formula = event ~ t2,
                      family = binomial(link = "cloglog"),
                      data = surv6f)
summ(baseline_fglm3, exp = TRUE)
fglm3 <- glm(formula = event ~ t2 + agemn + agesq + pji3 + finnow3cat + finfut.imp + employed + edu,
             family = binomial(link = "cloglog"),
             data = surv6f)
summary(fglm3)
summ(fglm3, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

###########################################################################
# Full Sample - 4 Years ---------------------------------------------------
###########################################################################

#Making a DF for the PJI2

#PJI First 2 years post-education
pji_4yr <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/pji_busetta_mendola/panel_pji_4yr_run.dta")

pji_4yr_2 <- pji_4yr %>% 
  rename("pidp" = "id", "pji4" = "se_ee") %>% 
  select(pidp, pji4) %>% 
  filter(!is.na(pji4))

#DF for 2 years after the end of education
surv8 <- surv5 %>% 
  filter(t1 >= 48) %>% 
  left_join(. , pji_4yr_2, by = "pidp")

#Men
surv8m <- surv8 %>% filter(sex == 1) 
surv8m %>% count(event)
#Women
surv8f <- surv8 %>% filter(sex == 2)
surv8f %>% count(event)

# -------------------------------------------------------------------------
# Full Sample Model Run ---------------------------------------------------
# -------------------------------------------------------------------------

####Model for men
baseline_mglm4 <- glm(formula = event ~ t2,
                      family = binomial(link = "cloglog"),
                      data = surv8m)
summ(baseline_mglm4, exp = TRUE, scale = TRUE)
mglm4 <- glm(formula = event ~ t2 + agemn + agesq + pji4 + finnow3cat + finfut.imp + employed + edu,
             family = binomial(link = "cloglog"),
             data = surv8m)
summary(mglm4)
summ(mglm4, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

####Model for women
baseline_fglm4 <- glm(formula = event ~ t2,
                      family = binomial(link = "cloglog"),
                      data = surv8f)
summ(baseline_fglm4, exp = TRUE)
fglm4 <- glm(formula = event ~ t2 + agemn + agesq + pji4 + finnow3cat + finfut.imp + employed + edu,
             family = binomial(link = "cloglog"),
             data = surv8f)
summary(fglm4)
summ(fglm4, exp = TRUE) #exp = TRUE means that we want exponentiated estimates



###########################################################################
# Table for all groups ----------------------------------------------------
###########################################################################



# -------------------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# -------------------------------------------------------------------------
export_summs(fglm2, mglm2, fglm3,mglm3, fglm4, mglm4,
             model.names = c("Women 2 Years", "Men 2 Years", "Women 3 Years", "Men 3 Years", "Women 4 Years", "Men 4 Years"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2",
                       "PJI - 2yrs" = "pji2",
                       "PJI - 3yrs" = "pji3",
                       "PJI - 4yrs" = "pji4",
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
             to.file = "html",
             file.name = "pji_testing_2-3-4yrs_26.11.2021.html")

export_summs(fglm2, mglm2, fglm3,mglm3, fglm4, mglm4,
             model.names = c("Women 2 Years", "Men 2 Years", "Women 3 Years", "Men 3 Years", "Women 4 Years", "Men 4 Years"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2",
                       "PJI - 2yrs" = "pji2",
                       "PJI - 3yrs" = "pji3",
                       "PJI - 4yrs" = "pji4",
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
             file.name = "pji_testing_2-3-4yrs_26.11.2021.docx")
