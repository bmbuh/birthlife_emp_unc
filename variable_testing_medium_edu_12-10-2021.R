#Coded by: Brian Buh
#Started on: 11.10.2021
#Last Updated: 02.11.2021


#Use the surv6 RDS created on script 13
#Medium Education
surv6medium <- surv6 %>% filter(edu == "medium")
surv6mediumm <- surv6medium %>% filter(sex == 1)
surv6mediumf <- surv6medium %>% filter(sex == 2)

summary(surv6medium$agebirth)

# -------------------------------------------------------------------
# Testing for the age of first birth and months from EE to conception
# -------------------------------------------------------------------

# create data sets to find the average age and months since end of education for those that experience first child birth
fbsurv6medium <- surv6medium %>% filter(event == 1)
fbsurv6mediumm <- fbsurv6medium %>% filter(sex == 1)
fbsurv6mediumf <- fbsurv6medium %>% filter(sex == 2)
summary(fbsurv6medium$agebirth)
summary(fbsurv6mediumm$agebirth)
summary(fbsurv6mediumf$agebirth)
summary(fbsurv6mediumm$t2)
summary(fbsurv6mediumf$t2)

#Use the survemp RDS created on script 13
#Medium Education
survempmedium <- survemp3 %>% filter(edu == "medium")
survempmediumm <- survempmedium %>% filter(sex == 1)
survempmediumf <- survempmedium %>% filter(sex == 2)

survempmediumm %>% count(event)
survempmediumf %>% count(event)

# create data sets to find the average age and months since end of education for those that experience first child birth
fbsurvempmedium <- survempmedium %>% filter(event == 1)
fbsurvempmediumm <- fbsurvempmedium %>% filter(sex == 1)
fbsurvempmediumf <- fbsurvempmedium %>% filter(sex == 2)
summary(fbsurvempmedium$agebirth)
summary(fbsurvempmediumm$agebirth)
summary(fbsurvempmediumf$agebirth)
summary(fbsurvempmediumm$t2_3)
summary(fbsurvempmediumf$t2_3)

# ------------------------------------------------------------------------
# At least 1 wave descriptives --------------------------------------------
# ------------------------------------------------------------------------

#Number of unique respondents
statsurv6medium <- surv6medium %>%
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ungroup() 

statsurv6medium %>% count(sex)
#Women = 1507, Men = 1438

#Ever report difficult financial situation
statsurv6medium1 <- surv6medium %>% 
  filter(finnow3cat == "finddifficult") %>% 
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ungroup() 

statsurv6medium1 %>% count(sex)
#Women = 247, Men = 284
#Women = 16.4%, Men = 19.7%

#Ever reported "Worse off"
statsurv6medium2 <- surv6medium %>% 
  filter(finfut.imp == "Worse off") %>% 
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ungroup() 

statsurv6medium2 %>% count(sex)
#Women = 386, Men = 391
#Women = 25.9%, Men = 27.2%

#Ever reported not employed
statsurv6medium3 <- surv6medium %>% 
  filter(employed == 0) %>% 
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ungroup() 

statsurv6medium3 %>% count(sex)
# Women = 749, Men = 789
# Women = 49.7%, Men = 54.9%

#Ever likely lose job
statsurvempmedium <- survempmedium %>% 
  filter(jbsec.dummy == 1) %>%
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ungroup() 

statsurvempmedium %>% count(sex)
#Total Women = 734, Men = 779
# Women = 102, Men = 94
# Women = 13.9%, Men = 12.1%

###########################################################################
# Testing the medium educated variables -------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Model for mediumly educated men -------------------------------------------
# -------------------------------------------------------------------------

#Model 1: PJI3 + employed
mediumm1 <- glm(formula = event ~ t2 + pji3 + employed + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6mediumm)
summary(mediumm1)
summ(mediumm1, exp = TRUE)
#Result: PJI 0.29***

#Model 2: finnow  + employed
mediumm2 <- glm(formula = event ~ t2 + finnow3cat + employed + cci + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6mediumm)
summary(mediumm2)
summ(mediumm2, exp = TRUE)
#Result: employed 3.16***

#Model 3: finfut + employed + cci 
mediumm3 <- glm(formula = event ~ t2 + finfut.imp + employed +  cci + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6mediumm)
summary(mediumm3)
summ(mediumm3, exp = TRUE)
#Result: employed 2.97***

#Model 4: finnow3cat*employed + finfut.imp*employed + cci 
mediumm4 <- glm(formula = event ~ t2 + finnow3cat*employed + finfut.imp*employed + cci  + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6mediumm)
summary(mediumm4)
summ(mediumm4, exp = TRUE)
#Result: no sign.

#Model 5: pji3 + finnow3cat*employed + finfut.imp*employed + cci 
mediumm5 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6mediumm)
summary(mediumm5)
summ(mediumm5, exp = TRUE)
#Result: no sign.

#Model 6: pji3 + finnow3cat*employed + finfut.imp*employed + cci + hh income
mediumm6 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + fihhmnnet4_dv + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6mediumm)
summary(mediumm6)
summ(mediumm6, exp = TRUE)
#Result: Nothing is significant

#Model 7: pji3 + finnow3cat*employed + finfut.imp*employed + cci + partner
mediumm7 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + combo + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6mediumm)
summary(mediumm7)
summ(mediumm7, exp = TRUE)
#Result: Only partnership stuff is significant
#Result: large bonus for unemployed partners

#Take away: Models 5 & 7 are the best fits

#PJI is significant before adding subject measures then nothing is. Non-employed partners add a bonus to fertility


# -------------------------------------------------------------------------
# Model for medium educated women -----------------------------------------
# -------------------------------------------------------------------------

#Model 1: PJI3 + employed
mediumf1 <- glm(formula = event ~ t2 + pji3 + employed + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6mediumf)
summary(mediumf1)
summ(mediumf1, exp = TRUE)
#Result: PJI 0.47*

#Model 2: finnow  + employed
mediumf2 <- glm(formula = event ~ t2 + finnow3cat + employed + cci + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6mediumf)
summary(mediumf2)
summ(mediumf2, exp = TRUE)
#Result: no sign.

#Model 3: finfut + employed + cci (note without employed finfut-worse off is significant)
mediumf3 <- glm(formula = event ~ t2 + finfut.imp + employed +  cci + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6mediumf)
summary(mediumf3)
summ(mediumf3, exp = TRUE)
#Result: no sign.

#Model 4: finnow3cat*employed + finfut.imp*employed + cci 
mediumf4 <- glm(formula = event ~ t2 + finnow3cat*employed + finfut.imp*employed + cci  + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6mediumf)
summary(mediumf4)
summ(mediumf4, exp = TRUE)
#Result: no sign.

#Model 5: pji3 + finnow3cat*employed + finfut.imp*employed + cci 
mediumf5 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6mediumf)
summary(mediumf5)
summ(mediumf5, exp = TRUE)
#Result: no sign.

#Model 6: pji3 + finnow3cat*employed + finfut.imp*employed + cci + hh income
mediumf6 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + fihhmnnet4_dv + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6mediumf)
summary(mediumf6)
summ(mediumf6, exp = TRUE)
#Result: no sign.

#Model 7: pji3 + finnow3cat*employed + finfut.imp*employed + cci + partner
mediumf7 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + combo + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6mediumf)
summary(mediumf7)
summ(mediumf7, exp = TRUE)
#Result: pji3 0.43*
#Result 2: all partner significant a bonus for the non-employed(?)

#Take away: Models 5 & 7 are the best fits

# pji is important when it is just the obj. measure and in the partner model. No subjective measures are significant

###########################################################################
# Employed sample ---------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Model for medium educated men ---------------------------------------------
# -------------------------------------------------------------------------

#Model 1: pji
mediumempm1 <- glm(formula = event2 ~ t2_3 + pji3 + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = survempmediumm)
summary(mediumempm1)
summ(mediumempm1, exp = TRUE)
# Result: no sign.

#Model 2: jbsec + permcon + parttime
mediumempm2 <- glm(formula = event2 ~ t2_3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = survempmediumm)
summary(mediumempm2)
summ(mediumempm2, exp = TRUE)
# Result: no sign.

#Model 3: pji + jbsec + permcon + parttime
mediumempm3 <- glm(formula = event2 ~ t2_3 + pji3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = survempmediumm)
summary(mediumempm3)
summ(mediumempm3, exp = TRUE)
# Result: no sign.

#Model 4: pji + jbsec + permcon + parttime + finnow3cat + finfut.imp
mediumempm4 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci + pji3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = survempmediumm)
summary(mediumempm4)
summ(mediumempm4, exp = TRUE)
# Result: finddifficult 1.90*
# Result 2: better off 1.47*


#Model 5: pji + jbsec + permcon + parttime + finnow3cat + finfut.imp + hh income
mediumempm5 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci + pji3 + jbsec.dummy + permcon + parttime + fihhmnnet4_dv + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = survempmediumm)
summary(mediumempm5)
summ(mediumempm5, exp = TRUE)
# Result: better off 1.58*

#Model 6: pji + jbsec + permcon + finnow3cat*parttime + finfut.imp*parttime  + hh income
mediumempm6 <- glm(formula = event2 ~ t2_3 + finnow3cat*parttime  + finfut.imp*parttime + cci + pji3 + jbsec.dummy +  permcon  + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = survempmediumm)
summary(mediumempm6)
summ(mediumempm6, exp = TRUE)
# Result: better off 1.46*

#Model 7: pji + jbsec + parttime + permcon + finnow3cat + finfut.imp + partner
mediumempm7 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci + pji3 + jbsec.dummy + permcon + parttime + combo + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = survempmediumm)
summary(mediumempm7)
summ(mediumempm7, exp = TRUE)
# Result: better off 1.51*
#Result 2: huge bonus for non-employed partner

#It appears that employed medium men are only affected when they perceive the future as better off (positive significant)

# -------------------------------------------------------------------------
# Model for medium educated women -------------------------------------------
# -------------------------------------------------------------------------

#Model 1: pji
mediumempf1 <- glm(formula = event2 ~ t2_3 + pji3 + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = survempmediumf)
summary(mediumempf1)
summ(mediumempf1, exp = TRUE)
# Result: pji3 0.01***

#Model 2: jbsec + permcon + parttime
mediumempf2 <- glm(formula = event2 ~ t2_3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = survempmediumf)
summary(mediumempf2)
summ(mediumempf2, exp = TRUE)
# Result: no sign.

#Model 3: pji + jbsec + permcon + parttime
mediumempf3 <- glm(formula = event2 ~ t2_3 + pji3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = survempmediumf)
summary(mediumempf3)
summ(mediumempf3, exp = TRUE)
# Result: pji3 0.01***

#Model 4: pji + jbsec + permcon + parttime + finnow3cat + finfut.imp
mediumempf4 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci + pji3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = survempmediumf)
summary(mediumempf4)
summ(mediumempf4, exp = TRUE)
# Result:  pji3 0.01***
# Result 2: find difficult 0.36*
# Result 3: worse off 2.07**
# Result: parttime 1.59*


#Model 5: pji + jbsec + permcon + parttime + finnow3cat + finfut.imp + hh income
mediumempf5 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci + pji3 + jbsec.dummy + permcon + parttime + fihhmnnet4_dv + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = survempmediumf)
summary(mediumempf5)
summ(mediumempf5, exp = TRUE)
# Result:  pji3 0.02***
# Result 2: find difficult 0.36*
# Result 3: worse off 1.95*
# Result: income removes effect of income

#Model 6: pji + jbsec + permcon + finnow3cat*parttime + finfut.imp*parttime  + hh income
mediumempf6 <- glm(formula = event2 ~ t2_3 + finnow3cat*parttime  + finfut.imp*parttime + cci + pji3 + jbsec.dummy +  permcon  + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = survempmediumf)
summary(mediumempf6)
summ(mediumempf6, exp = TRUE)
# Result: the interaction changes nothing

#Model 7: pji + jbsec + parttime + permcon + finnow3cat + finfut.imp + partner
mediumempf7 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci + pji3 + jbsec.dummy + permcon + parttime + combo + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = survempmediumf)
summary(mediumempf7)
summ(mediumempf7, exp = TRUE)
# Result:  pji3 0.01***
# Result 2: worse off 2.02**
# Result: no strong partner employment effects

#Take away: Models 4 and 7 work best
#It seems that employed medium women are very strong effected by PJI and Positive significant worse off

###########################################################################
# Outputs -----------------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# medium Men ----------------------------------------------------------------
# -------------------------------------------------------------------------

#html
export_summs(mediumm1, mediumm5, mediumm7, mediumempm3, mediumempm4,mediumempm7, 
             # model.names = c("Men 1", "Employed Men 1", "Men 2", "Employed Men 2", "Men 3", "Men 4", "Employed Men 3"),
             model.names = c("Men 1", "Men 2", "Men 3", "Employed Men 1", "Employed Men 2", "Employed Men 3"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2",
                       "Time 2" = "t2_3",
                       "PJI" = "pji3",
                       "Employed" = "employed",
                       "Likely lose job next 12 months" = "jbsec.dummy1",
                       "Permanent Contract" = "permcon",
                       "Part-Time" = "parttime",
                       "Finding it difficult" = "finnow3catfinddifficult",
                       "Getting by" = "finnow3catgetby",
                       "Worse off" = "finfut.impWorse off",
                       "Better off" = "finfut.impBetter off",
                       "Cohab - Employed" = "combocohab-employed",
                       "Cohab - Non-employed" = "combocohab-non-employed",
                       "Cohab - Unknown" = "combocohab-unknown",
                       "Married - Employed" = "combomarried-employed",
                       "Married - Non-employed" = "combomarried-non-employed",
                       "Married - Unknown" = "combomarried-unknown",
                       "finddifficult:employed" = "finnow3catfinddifficult:employed",
                       "getby:employed" =  "finnow3catgetby:employed",
                       "worseoff:employed" = "employed:finfut.impWorse off",
                       "betteroff:employed" = "employed:finfut.impBetter off",
                       "CCI" = "cci",
                       "Age in Months" = "agemn",
                       "Age Squared" = "agesq",
                       "< = 1975" = "cohort2<=1975",
                       ">= 1990" = "cohort2>=1990",
                       "Immigrant" = "immigrant1"),
             exp = TRUE,
             to.file = "html",
             file.name = "medium_full_men_13-10-21.html")


# -------------------------------------------------------------------------
# medium Women --------------------------------------------------------------
# -------------------------------------------------------------------------

#html
export_summs(mediumf1, mediumf5, mediumf7, mediumempf3, mediumempf4,mediumempf7, 
             # model.names = c("Men 1", "Employed Men 1", "Men 2", "Employed Men 2", "Men 3", "Men 4", "Employed Men 3"),
             model.names = c("Women 1", "Women 2", "Women 3", "Employed Women 1", "Employed Women 2", "Employed Women 3"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2",
                       "Time 2" = "t2_3",
                       "PJI" = "pji3",
                       "Employed" = "employed",
                       "Likely lose job next 12 months" = "jbsec.dummy1",
                       "Permanent Contract" = "permcon",
                       "Part-Time" = "parttime",
                       "Finding it difficult" = "finnow3catfinddifficult",
                       "Getting by" = "finnow3catgetby",
                       "Worse off" = "finfut.impWorse off",
                       "Better off" = "finfut.impBetter off",
                       "Cohab - Employed" = "combocohab-employed",
                       "Cohab - Non-employed" = "combocohab-non-employed",
                       "Cohab - Unknown" = "combocohab-unknown",
                       "Married - Employed" = "combomarried-employed",
                       "Married - Non-employed" = "combomarried-non-employed",
                       "Married - Unknown" = "combomarried-unknown",
                       "finddifficult:employed" = "finnow3catfinddifficult:employed",
                       "getby:employed" =  "finnow3catgetby:employed",
                       "worseoff:employed" = "employed:finfut.impWorse off",
                       "betteroff:employed" = "employed:finfut.impBetter off",
                       "CCI" = "cci",
                       "Age in Months" = "agemn",
                       "Age Squared" = "agesq",
                       "< = 1975" = "cohort2<=1975",
                       ">= 1990" = "cohort2>=1990",
                       "Immigrant" = "immigrant1"),
             exp = TRUE,
             to.file = "html",
             file.name = "medium_full_women_13-10-21.html")

# -------------------------------------------------------------------------
# Combined Output ---------------------------------------------------------
# -------------------------------------------------------------------------

#html
export_summs(mediumf5, mediumf7, mediumempf4, mediumempf7, mediumm5, mediumm7,  mediumempm4, mediumempm7, 
             model.names = c("Women 1", "Women 2", "Employed Women 1", "Employed Women 2", "Men 1", "Men 2", "Employed Men 1", "Employed Men 2"),
             # model.names = c("Men 1", "Men 2", "Men 3", "Employed Men 1", "Employed Men 2", "Employed Men 3"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2",
                       "Time 2" = "t2_3",
                       "PJI" = "pji3",
                       "Employed" = "employed",
                       "Likely lose job next 12 months" = "jbsec.dummy1",
                       "Permanent Contract" = "permcon",
                       "Part-Time" = "parttime",
                       "Finding it difficult" = "finnow3catfinddifficult",
                       "Getting by" = "finnow3catgetby",
                       "Worse off" = "finfut.impWorse off",
                       "Better off" = "finfut.impBetter off",
                       "Cohab - Employed" = "combocohab-employed",
                       "Cohab - Non-employed" = "combocohab-non-employed",
                       "Cohab - Unknown" = "combocohab-unknown",
                       "Married - Employed" = "combomarried-employed",
                       "Married - Non-employed" = "combomarried-non-employed",
                       "Married - Unknown" = "combomarried-unknown",
                       "finddifficult:employed" = "finnow3catfinddifficult:employed",
                       "getby:employed" =  "finnow3catgetby:employed",
                       "worseoff:employed" = "employed:finfut.impWorse off",
                       "betteroff:employed" = "employed:finfut.impBetter off",
                       "CCI" = "cci",
                       "Age in Months" = "agemn",
                       "Age Squared" = "agesq",
                       "< = 1975" = "cohort2<=1975",
                       ">= 1990" = "cohort2>=1990",
                       "Immigrant" = "immigrant1"),
             exp = TRUE,
             to.file = "html",
             file.name = "medium_full-emp_18-10-21.html")

#word
export_summs(mediumf5, mediumf7, mediumempf4, mediumempf7, mediumm5, mediumm7,  mediumempm4, mediumempm7, 
             model.names = c("Women 1", "Women 2", "Employed Women 1", "Employed Women 2", "Men 1", "Men 2", "Employed Men 1", "Employed Men 2"),
             # model.names = c("Men 1", "Men 2", "Men 3", "Employed Men 1", "Employed Men 2", "Employed Men 3"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2",
                       "Time 2" = "t2_3",
                       "PJI" = "pji3",
                       "Employed" = "employed",
                       "Permanent Contract" = "permcon",
                       "Part-Time" = "parttime",
                       "Likely lose job next 12 months" = "jbsec.dummy1",
                       "Finding it difficult" = "finnow3catfinddifficult",
                       "Getting by" = "finnow3catgetby",
                       "Worse off" = "finfut.impWorse off",
                       "Better off" = "finfut.impBetter off",
                       "Cohab - Employed" = "combocohab-employed",
                       "Cohab - Non-employed" = "combocohab-non-employed",
                       "Cohab - Unknown" = "combocohab-unknown",
                       "Married - Employed" = "combomarried-employed",
                       "Married - Non-employed" = "combomarried-non-employed",
                       "Married - Unknown" = "combomarried-unknown",
                       "finddifficult:employed" = "finnow3catfinddifficult:employed",
                       "getby:employed" =  "finnow3catgetby:employed",
                       "worseoff:employed" = "employed:finfut.impWorse off",
                       "betteroff:employed" = "employed:finfut.impBetter off",
                       "CCI" = "cci",
                       "Age in Months" = "agemn",
                       "Age Squared" = "agesq",
                       "< = 1975" = "cohort2<=1975",
                       ">= 1990" = "cohort2>=1990",
                       "Immigrant" = "immigrant1"),
             exp = TRUE,
             to.file = "word",
             file.name = "medium_full-emp_18-10-21.docx")
