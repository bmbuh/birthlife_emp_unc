#Coded by: Brian Buh
#Started on: 21.07.2021
#Last Updated: 

# install.packages("plyr")

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
# library(plyr)

###########################################################################
# Load DF -----------------------------------------------------------------
###########################################################################

surv5 <- file.choose()
surv5 <- readRDS(surv5)

survemp<- file.choose()
survemp<- readRDS(survemp)

#PJI First 3 years post-education
pji_3yr <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/pji_busetta_mendola/panel_pji_3yr_run.dta")
#PJI First 2 years post-education
pji_2yr <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/pji_busetta_mendola/panel_pji_2yr_run.dta")


###########################################################################
# sample Description ------------------------------------------------------
###########################################################################

statsurv <- surv6 %>% 
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(fb = ifelse(is.na(kdob), 0, 1)) %>% 
  mutate(fb = as.factor(fb)) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  mutate(edu = fct_relevel(edu, c("high", "medium", "low"))) %>% 
  ungroup()

mycontrols <- tableby.control(test = FALSE)
fullstats <-arsenal::tableby(fb ~ t2 + sex + pji3 + finnow3cat + finfut.imp + employed + edu, data = statsurv, control = mycontrols)
labels(fullstats) <-  c(t2 = "Time since end of education (months)", sex = "Sex", pji3 = "PJI", employed = "Employed",
                        finnow3cat = "Present Finacial", finfut.imp = "Future Finacial", edu = "Educational Attainment")
summary(fullstats)
write2word(fullstats , "fullstats_surv6_21-07-2021.docx") 

###########################################################################
# Full Sample - 3 Years ---------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Making a dataset fit for new models -------------------------------------
# -------------------------------------------------------------------------

pji_3yr_2 <- pji_3yr %>% 
  rename("pidp" = "id", "pji3" = "se_ee") %>% 
  select(pidp, pji3) %>% 
  filter(!is.na(pji3))

pji_2yr_2 <- pji_2yr %>% 
  rename("pidp" = "id", "pji2" = "se_ee") %>% 
  select(pidp, pji2) %>% 
  filter(!is.na(pji2))

#DF for 3 years after the end of education
surv6 <- surv5 %>% 
  filter(t1 >= 36) %>% 
  left_join(. , pji_3yr_2, by = "pidp") 

#Men
surv6m <- surv6 %>% filter(sex == 1) 
surv6m %>% count(event)
#Women
surv6f <- surv6 %>% filter(sex == 2)
surv6f %>% count(event)

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

# -------------------------------------------------------------------------
# Full Sample Model Run ---------------------------------------------------
# -------------------------------------------------------------------------
 
####Model for men
baseline_mglm <- glm(formula = event ~ t2,
                     family = binomial(link = "cloglog"),
                     data = surv6m)
summ(baseline_mglm, exp = TRUE, scale = TRUE)
mglm <- glm(formula = event ~ t2 + agemn + agesq + pji3 + finnow3cat + finfut.imp + employed + edu,
            family = binomial(link = "cloglog"),
            data = surv6m)
summary(mglm)
summ(mglm, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

####Model for women
baseline_fglm <- glm(formula = event ~ t2,
                     family = binomial(link = "cloglog"),
                     data = surv6f)
summ(baseline_fglm, exp = TRUE)
fglm <- glm(formula = event ~ t2 + agemn + agesq + pji3 + finnow3cat + finfut.imp + employed + edu,
            family = binomial(link = "cloglog"),
            data = surv6f)
summary(fglm)
summ(fglm, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

# -------------------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# -------------------------------------------------------------------------

export_summs(mglm,fglm,
             model.names = c("Men", "Women"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2",
                       "PJI" = "pji3",
                       "Employed" = "employed",
                       "Finding it difficult" = "finnow3catfinddifficult",
                       "Getting by" = "finnow3catgetby",
                       "Worse off" = "finfut.impWorse off",
                       "Better off" = "finfut.impBetter off",
                       "Education Low" = "edulow",
                       "Education Medium" = "edumedium",
                       "Age in Months" = "agemn",
                       "Age Squared" = "agesq"),
             exp = TRUE,
             to.file = "docx",
             file.name = "full_model_paper1_21-07-21.docx")

plot_summs(mglm, fglm, 
           model.names = c("Men", "Women"),
           coefs = c("Time since Education" = "t2",
                     "PJI" = "pji3",
                     "Finding it difficult" = "finnow3catfinddifficult",
                     "Getting by" = "finnow3catgetby",
                     "Employed" = "employed",
                     "Worse off" = "finfut.impWorse off",
                     "Better off" = "finfut.impBetter off",
                     "Education Low" = "edulow",
                     "Education Medium" = "edumedium",
                     "Age in Months" = "agemn",
                     "Age Squared" = "agesq"),
           exp = TRUE) +
  ggsave("full_model_paper1_21-07-21.png")


###########################################################################
# Employed Sample - 3 Years -----------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Making a dataset fit for new models -------------------------------------
# -------------------------------------------------------------------------

#DF for 3 years after the end of education
survemp3 <- survemp %>% 
  filter(t1 >= 36) %>% 
  left_join(. , pji_3yr_2, by = "pidp") 

#Men
survemp3m <- survemp3 %>% filter(sex == 1) 
#Women
survemp3f <- survemp3 %>% filter(sex == 2)

#DF for 2 years after the end of education
survemp2 <- survemp %>% 
  filter(t1 >= 24) %>% 
  left_join(. , pji_2yr_2, by = "pidp")

# -------------------------------------------------------------------------
# Employed Sample Model Run -----------------------------------------------
# -------------------------------------------------------------------------

####Model for men
empmglm <- glm(formula = event2 ~ t2_3 + agemn + agesq + pji3 + jbsec.dummy + permcon + isco + edu,
               family = binomial(link = "cloglog"),
               data = survemp3m)
summary(empmglm)
summ(empmglm, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

####Model for women
empfglm <- glm(formula = event2 ~ t2_3 + agemn + agesq + pji3 +  jbsec.dummy + permcon + isco +  edu,
               family = binomial(link = "cloglog"),
               data = survemp3f)
summary(empfglm)
summ(empfglm, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

survemp3f %>% count(jbsec.dummy)

# -------------------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# -------------------------------------------------------------------------
export_summs(empmglm,empfglm,
             model.names = c("Men", "Women"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2_3",
                       "PJI - 3yrs" = "pji3",
                       "Permanent Contract" = "permcon1",
                       "Job Security" = "jbsec.dummy1",
                       "Education Low" = "edulow",
                       "Education Medium" = "edumedium",
                       "Age in Months" = "agemn",
                       "Age Squared" = "agesq"),
             exp = TRUE,
             to.file = "docx",
             file.name = "employed_model_output_21-07-21.docx")


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
