#Coded by: Brian Buh
#Started on: 11.10.2021
#Last Updated: 


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


#Use the surv6 RDS created on script 13
#High education
surv6high <- surv6 %>% filter(edu == "high") 
surv6highm <- surv6high %>% filter(sex == 1)
surv6highf <- surv6high %>% filter(sex == 2)
#Medium Education
surv6medium <- surv6 %>% filter(edu == "medium")
surv6mediumm <- surv6medium %>% filter(sex == 1)
surv6mediumf <- surv6medium %>% filter(sex == 2)
#Low Education
surv6low <- surv6 %>% filter(edu == "low")
surv6lowm <- surv6low %>% filter(sex == 1)
surv6lowf <- surv6low %>% filter(sex == 2)

#Use the survemp RDS created on script 13
#High education
survemphigh <- survemp3 %>% filter(edu == "high") 
survemphighm <- survemphigh %>% filter(sex == 1)
survemphighf <- survemphigh %>% filter(sex == 2)
#Medium Education
survempmedium <- survemp3 %>% filter(edu == "medium")
survempmediumm <- survempmedium %>% filter(sex == 1)
survempmediumf <- survempmedium %>% filter(sex == 2)
#Low Education
survemplow <- survemp3 %>% filter(edu == "low")
survemplowm <- survemplow %>% filter(sex == 1)
survemplowf <- survemplow %>% filter(sex == 2)





# -------------------------------------------------------------------------
# High Education Testing --------------------------------------------------
# -------------------------------------------------------------------------

####Model for highly educated men
highmglm1 <- glm(formula = event ~ t2 + pji3 + employed +finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = surv6highm)
summary(highmglm1)
summ(highmglm1, exp = TRUE)

highmglm2 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
             family = binomial(link = "cloglog"),
             data = surv6highm)
summary(highglm2)
summ(highglm2, exp = TRUE)

highmglm3 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo,
             family = binomial(link = "cloglog"),
             data = surv6highm)
summary(highmglm3)
summ(highmglm3, exp = TRUE)

highmglm4 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv,
             family = binomial(link = "cloglog"),
             data = surv6highm)
summary(highmglm4)
summ(highmglm4, exp = TRUE)

####Model for highly educated women
highfiglm1 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = surv6highf)
summary(highfiglm1)
summ(highfiglm1, exp = TRUE)

highfiglm2 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
             family = binomial(link = "cloglog"),
             data = surv6highf)
summary(highfiglm2)
summ(highfiglm2, exp = TRUE)

highfiglm3 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo,
             family = binomial(link = "cloglog"),
             data = surv6highf)
summary(highfiglm3)
summ(highfiglm3, exp = TRUE)

highfiglm4 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv,
             family = binomial(link = "cloglog"),
             data = surv6highf)
summary(highfiglm4)
summ(highfiglm4, exp = TRUE)

# -------------------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# -------------------------------------------------------------------------

export_summs(highmglm1, highmglm2, highmglm3, highmglm4, highfiglm1, highfiglm2, highfiglm3, highfiglm4,
             model.names = c("Men 1", "Men 2", "Men 3", "Men 4", "Women 1", "Women 2", "Women 3", "Women 4"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t2",
             #           "PJI" = "pji3",
             #           "Employed" = "employed",
             #           "Finding it difficult" = "finnow3catfinddifficult",
             #           "Getting by" = "finnow3catgetby",
             #           "Worse off" = "finfut.impWorse off",
             #           "Better off" = "finfut.impBetter off",
             #           "CCI" = "cci",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "< = 1975" = "cohort2<=1975",
             #           ">= 1990" = "cohort2>=1990",
             #           "Education Low" = "edulow",
             #           "Education Medium" = "edumedium",
             #           "Immigrant" = "immigrant1",
             #           "Time 2" = "t2_3",
             #           "Likely lose job next 12 months" = "jbsec.dummy1",
             #           "Permanent Contract" = "permcon",
             #           "Part-Time" = "parttime",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "Household Income, benefits and deductions adjusted" = "fihhmnnet4_dv"),
             exp = TRUE,
             to.file = "html",
             file.name = "high_full1_11-10-21.html")

# -------------------------------------------------------------------------
# medium Education Testing --------------------------------------------------
# -------------------------------------------------------------------------

####Model for mediumly educated men
mediummglm1 <- glm(formula = event ~ t2 + pji3 + employed +finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = surv6mediumm)
summary(mediummglm1)
summ(mediummglm1, exp = TRUE)

mediummglm2 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                 family = binomial(link = "cloglog"),
                 data = surv6mediumm)
summary(mediummglm2)
summ(mediummglm2, exp = TRUE)

mediummglm3 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo,
                 family = binomial(link = "cloglog"),
                 data = surv6mediumm)
summary(mediummglm3)
summ(mediummglm3, exp = TRUE)

mediummglm4 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv,
                 family = binomial(link = "cloglog"),
                 data = surv6mediumm)
summary(mediummglm4)
summ(mediummglm4, exp = TRUE)

####Model for mediumly educated women
mediumfglm1 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = surv6mediumf)
summary(mediumfglm1)
summ(mediumfglm1, exp = TRUE)

mediumfglm2 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                 family = binomial(link = "cloglog"),
                 data = surv6mediumf)
summary(mediumfglm2)
summ(mediumfglm2, exp = TRUE)

mediumfglm3 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo,
                 family = binomial(link = "cloglog"),
                 data = surv6mediumf)
summary(mediumfglm3)
summ(mediumfglm3, exp = TRUE)

mediumfglm4 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv,
                 family = binomial(link = "cloglog"),
                 data = surv6mediumf)
summary(mediumfglm4)
summ(mediumfglm4, exp = TRUE)

# -------------------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# -------------------------------------------------------------------------

export_summs(mediummglm1, mediummglm2, mediummglm3, mediummglm4, mediumfglm1, mediumfglm2, mediumfglm3, mediumfglm4,
             model.names = c("Men 1", "Men 2", "Men 3", "Men 4", "Women 1", "Women 2", "Women 3", "Women 4"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t2",
             #           "PJI" = "pji3",
             #           "Employed" = "employed",
             #           "Finding it difficult" = "finnow3catfinddifficult",
             #           "Getting by" = "finnow3catgetby",
             #           "Worse off" = "finfut.impWorse off",
             #           "Better off" = "finfut.impBetter off",
             #           "CCI" = "cci",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "< = 1975" = "cohort2<=1975",
             #           ">= 1990" = "cohort2>=1990",
             #           "Education Low" = "edulow",
             #           "Education Medium" = "edumedium",
             #           "Immigrant" = "immigrant1",
             #           "Time 2" = "t2_3",
             #           "Likely lose job next 12 months" = "jbsec.dummy1",
             #           "Permanent Contract" = "permcon",
             #           "Part-Time" = "parttime",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "Household Income, benefits and deductions adjusted" = "fihhmnnet4_dv"),
             exp = TRUE,
             to.file = "html",
             file.name = "medium_full1_11-10-21.html")

# -------------------------------------------------------------------------
# low Education Testing --------------------------------------------------
# -------------------------------------------------------------------------

####Model for lowly educated men
lowmglm1 <- glm(formula = event ~ t2 + pji3 + employed +finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = surv6lowm)
summary(lowmglm1)
summ(lowmglm1, exp = TRUE)

lowmglm2 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                 family = binomial(link = "cloglog"),
                 data = surv6lowm)
summary(lowmglm2)
summ(lowmglm2, exp = TRUE)

lowmglm3 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo,
                 family = binomial(link = "cloglog"),
                 data = surv6lowm)
summary(lowmglm3)
summ(lowmglm3, exp = TRUE)

lowmglm4 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv,
                 family = binomial(link = "cloglog"),
                 data = surv6lowm)
summary(lowmglm4)
summ(lowmglm4, exp = TRUE)

####Model for lowly educated women
lowfglm1 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = surv6lowf)
summary(lowfglm1)
summ(lowfglm1, exp = TRUE)

lowfglm2 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                 family = binomial(link = "cloglog"),
                 data = surv6lowf)
summary(lowfglm2)
summ(lowfglm2, exp = TRUE)

lowfglm3 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo,
                 family = binomial(link = "cloglog"),
                 data = surv6lowf)
summary(lowfglm3)
summ(lowfglm3, exp = TRUE)

lowfglm4 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv,
                 family = binomial(link = "cloglog"),
                 data = surv6lowf)
summary(lowfglm4)
summ(lowfglm4, exp = TRUE)

# -------------------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# -------------------------------------------------------------------------

export_summs(lowmglm1, lowmglm2, lowmglm3, lowmglm4, lowfglm1, lowfglm2, lowfglm3, lowfglm4,
             model.names = c("Men 1", "Men 2", "Men 3", "Men 4", "Women 1", "Women 2", "Women 3", "Women 4"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t2",
             #           "PJI" = "pji3",
             #           "Employed" = "employed",
             #           "Finding it difficult" = "finnow3catfinddifficult",
             #           "Getting by" = "finnow3catgetby",
             #           "Worse off" = "finfut.impWorse off",
             #           "Better off" = "finfut.impBetter off",
             #           "CCI" = "cci",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "< = 1975" = "cohort2<=1975",
             #           ">= 1990" = "cohort2>=1990",
             #           "Education Low" = "edulow",
             #           "Education Medium" = "edumedium",
             #           "Immigrant" = "immigrant1",
             #           "Time 2" = "t2_3",
             #           "Likely lose job next 12 months" = "jbsec.dummy1",
             #           "Permanent Contract" = "permcon",
             #           "Part-Time" = "parttime",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "Household Income, benefits and deductions adjusted" = "fihhmnnet4_dv"),
             exp = TRUE,
             to.file = "html",
             file.name = "low_full1_11-10-21.html")

###########################################################################
# Interactions by education -----------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# High Education Interaction Testing --------------------------------------------------
# -------------------------------------------------------------------------

####Model for highly educated men
highmiglm1 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed * finfut.imp + cci + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = surv6highm)
summary(highmiglm1)
summ(highmiglm1, exp = TRUE)

highmiglm2 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed * finfut.imp + cci + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                 family = binomial(link = "cloglog"),
                 data = surv6highm)
summary(highmiglm2)
summ(highmiglm2, exp = TRUE)

highmiglm3 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed * finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo,
                 family = binomial(link = "cloglog"),
                 data = surv6highm)
summary(highmiglm3)
summ(highmiglm3, exp = TRUE)

highmiglm4 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed * finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv,
                 family = binomial(link = "cloglog"),
                 data = surv6highm)
summary(highmiglm4)
summ(highmiglm4, exp = TRUE)


####Employed Model for men

highempmglm2 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                   family = binomial(link = "cloglog"),
                   data = survemphighm)
summary(highempmglm2)
summ(highempmglm2, exp = TRUE)
highempmglm3 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv ,
                   family = binomial(link = "cloglog"),
                   data = survemphighm)
summary(highempmglm3)
summ(highempmglm3, exp = TRUE)

####Model for highly educated women
highfiglm1 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed * finfut.imp + cci + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = surv6highf)
summary(highfglm1)
summ(highfglm1, exp = TRUE)

highfiglm2 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed * finfut.imp + cci + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                 family = binomial(link = "cloglog"),
                 data = surv6highf)
summary(highfglm2)
summ(highfglm2, exp = TRUE)

highfiglm3 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed * finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo,
                 family = binomial(link = "cloglog"),
                 data = surv6highf)
summary(highfglm3)
summ(highfglm3, exp = TRUE)

highfiglm4 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed * finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv,
                 family = binomial(link = "cloglog"),
                 data = surv6highf)
summary(highfglm4)
summ(highfglm4, exp = TRUE)

#### Employed Model for women

highempfglm2 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                   family = binomial(link = "cloglog"),
                   data = survemphighf)
summary(highempfglm2)
summ(highempfglm2, exp = TRUE)

highempfglm3 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv ,
                   family = binomial(link = "cloglog"),
                   data = survemphighf)
summary(highempfglm3)
summ(highempfglm3, exp = TRUE)

# -------------------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# -------------------------------------------------------------------------

export_summs(highmiglm1, highmiglm2, highmiglm3, highmiglm4, highfiglm1, highfiglm2, highfiglm3, highfiglm4,
             model.names = c("Men 1", "Men 2", "Men 3", "Men 4", "Women 1", "Women 2", "Women 3", "Women 4"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t2",
             #           "PJI" = "pji3",
             #           "Employed" = "employed",
             #           "Finding it difficult" = "finnow3catfinddifficult",
             #           "Getting by" = "finnow3catgetby",
             #           "Worse off" = "finfut.impWorse off",
             #           "Better off" = "finfut.impBetter off",
             #           "CCI" = "cci",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "< = 1975" = "cohort2<=1975",
             #           ">= 1990" = "cohort2>=1990",
             #           "Education Low" = "edulow",
             #           "Education Medium" = "edumedium",
             #           "Immigrant" = "immigrant1",
             #           "Time 2" = "t2_3",
             #           "Likely lose job next 12 months" = "jbsec.dummy1",
             #           "Permanent Contract" = "permcon",
             #           "Part-Time" = "parttime",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "Household Income, benefits and deductions adjusted" = "fihhmnnet4_dv"),
             exp = TRUE,
             to.file = "html",
             file.name = "high_full1_interactions_11-10-21.html")

export_summs(highmiglm2, highempmglm2, highmiglm4, highempmglm3, highfiglm2, highempfglm2, highfiglm4,highempfglm3,
             model.names = c("Full Men 1", "Employed Men 1", "Full Men 2", "Employed Men 2", "Full Women 1", "Employed Women 1", "Women 2", "Employed Women 2"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t2",
             #           "PJI" = "pji3",
             #           "Employed" = "employed",
             #           "Finding it difficult" = "finnow3catfinddifficult",
             #           "Getting by" = "finnow3catgetby",
             #           "Worse off" = "finfut.impWorse off",
             #           "Better off" = "finfut.impBetter off",
             #           "CCI" = "cci",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "< = 1975" = "cohort2<=1975",
             #           ">= 1990" = "cohort2>=1990",
             #           "Education Low" = "edulow",
             #           "Education Medium" = "edumedium",
             #           "Immigrant" = "immigrant1",
             #           "Time 2" = "t2_3",
             #           "Likely lose job next 12 months" = "jbsec.dummy1",
             #           "Permanent Contract" = "permcon",
             #           "Part-Time" = "parttime",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "Household Income, benefits and deductions adjusted" = "fihhmnnet4_dv"),
             exp = TRUE,
             to.file = "html",
             file.name = "high_full2_interactions_11-10-21.html")


# -------------------------------------------------------------------------
# medium Education Testing --------------------------------------------------
# -------------------------------------------------------------------------

####Model for mediumly educated men
mediummiglm1 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = surv6mediumm)
summary(mediummiglm1)
summ(mediummiglm1, exp = TRUE)

mediummiglm2 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                   family = binomial(link = "cloglog"),
                   data = surv6mediumm)
summary(mediummiglm2)
summ(mediummiglm2, exp = TRUE)

mediummiglm3 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo,
                   family = binomial(link = "cloglog"),
                   data = surv6mediumm)
summary(mediummiglm3)
summ(mediummiglm3, exp = TRUE)

mediummiglm4 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv,
                   family = binomial(link = "cloglog"),
                   data = surv6mediumm)
summary(mediummiglm4)
summ(mediummiglm4, exp = TRUE)

####Employed Model for men

mediumempmglm2 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                      family = binomial(link = "cloglog"),
                      data = survempmediumm)
summary(mediumempmglm2)
summ(mediumempmglm2, exp = TRUE)
mediumempmglm3 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv ,
                      family = binomial(link = "cloglog"),
                      data = survempmediumm)
summary(mediumempmglm3)
summ(mediumempmglm3, exp = TRUE)

####Model for mediumly educated women
mediumfiglm1 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = surv6mediumf)
summary(mediumfiglm1)
summ(mediumfiglm1, exp = TRUE)

mediumfiglm2 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                   family = binomial(link = "cloglog"),
                   data = surv6mediumf)
summary(mediumfiglm2)
summ(mediumfiglm2, exp = TRUE)

mediumfiglm3 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo,
                   family = binomial(link = "cloglog"),
                   data = surv6mediumf)
summary(mediumfiglm3)
summ(mediumfiglm3, exp = TRUE)

mediumfiglm4 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv,
                   family = binomial(link = "cloglog"),
                   data = surv6mediumf)
summary(mediumfiglm4)
summ(mediumfiglm4, exp = TRUE)

####Model for women

mediumempfglm2 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                   family = binomial(link = "cloglog"),
                   data = survempmediumf)
summary(mediumempfglm2)
summ(mediumempfglm2, exp = TRUE)
mediumempfglm3 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv ,
                   family = binomial(link = "cloglog"),
                   data = survempmediumf)
summary(mediumempfglm3)
summ(mediumempfglm3, exp = TRUE)

# -------------------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# -------------------------------------------------------------------------

export_summs(mediummiglm1, mediummiglm2, mediummiglm3, mediummiglm4, mediumfiglm1, mediumfiglm2, mediumfiglm3, mediumfiglm4,
             model.names = c("Men 1", "Men 2", "Men 3", "Men 4", "Women 1", "Women 2", "Women 3", "Women 4"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t2",
             #           "PJI" = "pji3",
             #           "Employed" = "employed",
             #           "Finding it difficult" = "finnow3catfinddifficult",
             #           "Getting by" = "finnow3catgetby",
             #           "Worse off" = "finfut.impWorse off",
             #           "Better off" = "finfut.impBetter off",
             #           "CCI" = "cci",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "< = 1975" = "cohort2<=1975",
             #           ">= 1990" = "cohort2>=1990",
             #           "Education Low" = "edulow",
             #           "Education Medium" = "edumedium",
             #           "Immigrant" = "immigrant1",
             #           "Time 2" = "t2_3",
             #           "Likely lose job next 12 months" = "jbsec.dummy1",
             #           "Permanent Contract" = "permcon",
             #           "Part-Time" = "parttime",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "Household Income, benefits and deductions adjusted" = "fihhmnnet4_dv"),
             exp = TRUE,
             to.file = "html",
             file.name = "medium_full1_interactions_11-10-21.html")

export_summs(mediummiglm2, mediumempmglm2, mediummiglm4, mediumempmglm3, mediumfiglm2, mediumempfglm2, mediumfiglm4,mediumempfglm3,
             model.names = c("Full Men 1", "Employed Men 1", "Full Men 2", "Employed Men 2", "Full Women 1", "Employed Women 1", "Women 2", "Employed Women 2"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t2",
             #           "PJI" = "pji3",
             #           "Employed" = "employed",
             #           "Finding it difficult" = "finnow3catfinddifficult",
             #           "Getting by" = "finnow3catgetby",
             #           "Worse off" = "finfut.impWorse off",
             #           "Better off" = "finfut.impBetter off",
             #           "CCI" = "cci",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "< = 1975" = "cohort2<=1975",
             #           ">= 1990" = "cohort2>=1990",
             #           "Education Low" = "edulow",
             #           "Education Medium" = "edumedium",
             #           "Immigrant" = "immigrant1",
             #           "Time 2" = "t2_3",
             #           "Likely lose job next 12 months" = "jbsec.dummy1",
             #           "Permanent Contract" = "permcon",
             #           "Part-Time" = "parttime",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "Household Income, benefits and deductions adjusted" = "fihhmnnet4_dv"),
             exp = TRUE,
             to.file = "html",
             file.name = "medium_full2_interactions_11-10-21.html")


# -------------------------------------------------------------------------
# low Education Testing --------------------------------------------------
# -------------------------------------------------------------------------

####Model for lowly educated men
lowmiglm1 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant,
                    family = binomial(link = "cloglog"),
                    data = surv6lowm)
summary(lowmiglm1)
summ(lowmiglm1, exp = TRUE)

lowmiglm2 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                    family = binomial(link = "cloglog"),
                    data = surv6lowm)
summary(lowmiglm2)
summ(lowmiglm2, exp = TRUE)

lowmiglm3 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo,
                    family = binomial(link = "cloglog"),
                    data = surv6lowm)
summary(lowmiglm3)
summ(lowmiglm3, exp = TRUE)

lowmiglm4 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv,
                    family = binomial(link = "cloglog"),
                    data = surv6lowm)
summary(lowmiglm4)
summ(lowmiglm4, exp = TRUE)

####Model for men

lowempmglm2 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                family = binomial(link = "cloglog"),
                data = survemplowm)
summary(lowempmglm2)
summ(lowempmglm2, exp = TRUE)
lowempmglm3 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv ,
                family = binomial(link = "cloglog"),
                data = survemplowm)
summary(lowempmglm3)
summ(lowempmglm3, exp = TRUE)

####Model for lowly educated women
lowfiglm1 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant,
                    family = binomial(link = "cloglog"),
                    data = surv6lowf)
summary(lowfiglm1)
summ(lowfiglm1, exp = TRUE)

lowfiglm2 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                    family = binomial(link = "cloglog"),
                    data = surv6lowf)
summary(lowfiglm2)
summ(lowfiglm2, exp = TRUE)

lowfiglm3 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo,
                    family = binomial(link = "cloglog"),
                    data = surv6lowf)
summary(lowfiglm3)
summ(lowfiglm3, exp = TRUE)

lowfiglm4 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed *finfut.imp + cci + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv,
                    family = binomial(link = "cloglog"),
                    data = surv6lowf)
summary(lowfiglm4)
summ(lowfiglm4, exp = TRUE)

####Model for women

lowempfglm2 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant + fihhmnnet4_dv,
                   family = binomial(link = "cloglog"),
                   data = survemplowf)
summary(lowempfglm2)
summ(lowempfglm2, exp = TRUE)
lowempfglm3 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant + combo + fihhmnnet4_dv ,
                   family = binomial(link = "cloglog"),
                   data = survemplowf)
summary(lowempfglm3)
summ(lowempfglm3, exp = TRUE)

# -------------------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# -------------------------------------------------------------------------

export_summs(lowmiglm1, lowmiglm2, lowmiglm3, lowmiglm4, lowfiglm1, lowfiglm2, lowfiglm3, lowfiglm4,
             model.names = c("Men 1", "Men 2", "Men 3", "Men 4", "Women 1", "Women 2", "Women 3", "Women 4"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t2",
             #           "PJI" = "pji3",
             #           "Employed" = "employed",
             #           "Finding it difficult" = "finnow3catfinddifficult",
             #           "Getting by" = "finnow3catgetby",
             #           "Worse off" = "finfut.impWorse off",
             #           "Better off" = "finfut.impBetter off",
             #           "CCI" = "cci",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "< = 1975" = "cohort2<=1975",
             #           ">= 1990" = "cohort2>=1990",
             #           "Education Low" = "edulow",
             #           "Education Medium" = "edumedium",
             #           "Immigrant" = "immigrant1",
             #           "Time 2" = "t2_3",
             #           "Likely lose job next 12 months" = "jbsec.dummy1",
             #           "Permanent Contract" = "permcon",
             #           "Part-Time" = "parttime",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "Household Income, benefits and deductions adjusted" = "fihhmnnet4_dv"),
             exp = TRUE,
             to.file = "html",
             file.name = "low_full1_interactions_11-10-21.html")

export_summs(lowmiglm2, lowempmglm2, lowmiglm4, lowempmglm3, lowfiglm2, lowempfglm2, lowfiglm4,lowempfglm3,
             model.names = c("Full Men 1", "Employed Men 1", "Full Men 2", "Employed Men 2", "Full Women 1", "Employed Women 1", "Women 2", "Employed Women 2"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t2",
             #           "PJI" = "pji3",
             #           "Employed" = "employed",
             #           "Finding it difficult" = "finnow3catfinddifficult",
             #           "Getting by" = "finnow3catgetby",
             #           "Worse off" = "finfut.impWorse off",
             #           "Better off" = "finfut.impBetter off",
             #           "CCI" = "cci",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "< = 1975" = "cohort2<=1975",
             #           ">= 1990" = "cohort2>=1990",
             #           "Education Low" = "edulow",
             #           "Education Medium" = "edumedium",
             #           "Immigrant" = "immigrant1",
             #           "Time 2" = "t2_3",
             #           "Likely lose job next 12 months" = "jbsec.dummy1",
             #           "Permanent Contract" = "permcon",
             #           "Part-Time" = "parttime",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "Household Income, benefits and deductions adjusted" = "fihhmnnet4_dv"),
             exp = TRUE,
             to.file = "html",
             file.name = "low_full2_interactions_11-10-21.html")
