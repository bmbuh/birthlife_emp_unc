#Coded by: Brian Buh
#Started on: 11.10.2021
#Last Updated: 02.11.2021

#full sample mean age & months since educations
fbsurv6 <- surv6 %>% filter(event == 1)
fbsurv6m <- fbsurv6 %>% filter(sex == 1)
fbsurv6f <- fbsurv6 %>% filter(sex == 2)
summary(fbsurv6$agebirth)
summary(fbsurv6m$agebirth)
summary(fbsurv6f$agebirth)
summary(fbsurv6m$t2)
summary(fbsurv6f$t2)

# --------------------------------------------------------------------------------------
#Testing to see if removing births in the first 3 year affects transition to parenthood
# --------------------------------------------------------------------------------------

fbsurv5 <- surv5 %>%  mutate(agebirth = dvage + 0.75) %>% filter(event == 1)
fbsurv5m <- fbsurv5 %>% filter(sex == 1)
fbsurv5f <- fbsurv5 %>% filter(sex == 2)
summary(fbsurv5$agebirth)
summary(fbsurv5m$agebirth)
summary(fbsurv5f$agebirth)
summary(fbsurv5m$t2)
summary(fbsurv5f$t2)

#High
surv5high <- fbsurv5 %>% filter(edu == "high")
fbsurv5high <- surv5high %>% filter(event == 1)
fbsurv5highm <- fbsurv5high %>% filter(sex == 1)
fbsurv5highf <- fbsurv5high %>% filter(sex == 2)
summary(fbsurv5highm$agebirth)
summary(fbsurv5highf$agebirth)
summary(fbsurv5highm$t2)
summary(fbsurv5highf$t2)

surv5medium <- fbsurv5 %>% filter(edu == "medium")
fbsurv5medium <- surv5medium %>% filter(event == 1)
fbsurv5mediumm <- fbsurv5medium %>% filter(sex == 1)
fbsurv5mediumf <- fbsurv5medium %>% filter(sex == 2)
summary(fbsurv5mediumm$agebirth)
summary(fbsurv5mediumf$agebirth)
summary(fbsurv5mediumm$t2)
summary(fbsurv5mediumf$t2)

surv5low <- fbsurv5 %>% filter(edu == "low")
fbsurv5low <- surv5low %>% filter(event == 1)
fbsurv5lowm <- fbsurv5low %>% filter(sex == 1)
fbsurv5lowf <- fbsurv5low %>% filter(sex == 2)
summary(fbsurv5lowm$agebirth)
summary(fbsurv5lowf$agebirth)
summary(fbsurv5lowm$t2)
summary(fbsurv5lowf$t2)

#The general pattern of the months from EE to conception is the same when including the first 3 years.

# -------------------------------------------------------------------
# Testing for the age of first birth and months from EE to conception
# -------------------------------------------------------------------

#Use the surv6 RDS created on script 13
#High education
surv6high <- surv6 %>% filter(edu == "high")
surv6highm <- surv6high %>% filter(sex == 1)
surv6highf <- surv6high %>% filter(sex == 2)

summary(surv6high$agebirth)

# create data sets to find the average age and months since end of education for those that experience first child birth
fbsurv6high <- surv6high %>% filter(event == 1)
fbsurv6highm <- fbsurv6high %>% filter(sex == 1)
fbsurv6highf <- fbsurv6high %>% filter(sex == 2)
summary(fbsurv6high$agebirth)
summary(fbsurv6highm$agebirth)
summary(fbsurv6highf$agebirth)
summary(fbsurv6highm$t2)
summary(fbsurv6highf$t2)

#Use the survemp RDS created on script 13
#High education
survemphigh <- survemp3 %>% filter(edu == "high")
survemphighm <- survemphigh %>% filter(sex == 1)
survemphighf <- survemphigh %>% filter(sex == 2)

survemphighm %>% count(event)
survemphighf %>% count(event)

# create data sets to find the average age and months since end of education for those that experience first child birth
fbsurvemphigh <- survemphigh %>% filter(event == 1)
fbsurvemphighm <- fbsurvemphigh %>% filter(sex == 1)
fbsurvemphighf <- fbsurvemphigh %>% filter(sex == 2)
summary(fbsurvemphigh$agebirth)
summary(fbsurvemphighm$agebirth)
summary(fbsurvemphighf$agebirth)
summary(fbsurvemphighm$t2_3)
summary(fbsurvemphighf$t2_3)


###########################################################################
# Testing the high educated variables -------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Model for high educated men ---------------------------------------------
# -------------------------------------------------------------------------

#Model 1: PJI3 + employed
highm1 <- glm(formula = event ~ t2 + pji3 + employed + agemn + agesq + cohort2 + immigrant,
                 family = binomial(link = "cloglog"),
                 data = surv6highm)
summary(highm1)
summ(highm1, exp = TRUE)
#Result: PJI 0.58***
#Result 2: employed 1.82 0.00***

#Model 2: finnow  + employed
highm2 <- glm(formula = event ~ t2 + finnow3cat + employed + cci + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6highm)
summary(highm2)
summ(highm2, exp = TRUE)
#Result: employed 2.04***

#Model 3: finfut + employed + cci (note without employed finfut-worse off is significant)
highm3 <- glm(formula = event ~ t2 + finfut.imp + employed +  cci + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6highm)
summary(highm3)
summ(highm3, exp = TRUE)
#Result: employed 1.94***
#Result 2: worse off 1.30**

#Model 4: finnow3cat*employed + finfut.imp*employed + cci (note without employed finfut-worse off is significant)
highm4 <- glm(formula = event ~ t2 + finnow3cat*employed + finfut.imp*employed + cci  + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6highm)
summary(highm4)
summ(highm4, exp = TRUE)
#Result: employed 2.49*

#Model 5: pji3 + finnow3cat*employed + finfut.imp*employed + cci (note without employed finfut-worse off is significant)
highm5 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6highm)
summary(highm5)
summ(highm5, exp = TRUE)
#Result: pji3 0.62*
#Results: employed 2.28*

#Model 6: pji3 + finnow3cat*employed + finfut.imp*employed + cci + hh income
highm6 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + fihhmnnet4_dv + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6highm)
summary(highm6)
summ(highm6, exp = TRUE)
#Result: Nothing is significant

#Model 7: pji3 + finnow3cat*employed + finfut.imp*employed + cci + partner
highm7 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + combo + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6highm)
summary(highm7)
summ(highm7, exp = TRUE)
#Result: Only partnership stuff is significant

#Take away: Models 5 & 7 are the best fits

#PJI and employment are both very important. After partnership nothing else matter but there is no strong partner's employment bonus


# -------------------------------------------------------------------------
# Model for high educated women -----------------------------------------
# -------------------------------------------------------------------------

#Model 1: PJI3 + employed
highf1 <- glm(formula = event ~ t2 + pji3 + employed + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6highf)
summary(highf1)
summ(highf1, exp = TRUE)
#Result: PJI 0.59**
#Result 2: employed 1.50**

#Model 2: finnow  + employed
highf2 <- glm(formula = event ~ t2 + finnow3cat + employed + cci + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6highf)
summary(highf2)
summ(highf2, exp = TRUE)
#Result: finnow - getby 0.68***
#Result 2: employed 1.51**

#Model 3: finfut + employed + cci (note without employed finfut-worse off is significant)
highf3 <- glm(formula = event ~ t2 + finfut.imp + employed +  cci + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6highf)
summary(highf3)
summ(highf3, exp = TRUE)
#Result: employed 1.60***
#Result 2: better off 0.61**

#Model 4: finnow3cat*employed + finfut.imp*employed + cci 
highf4 <- glm(formula = event ~ t2 + finnow3cat*employed + finfut.imp*employed + cci  + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6highf)
summary(highf4)
summ(highf4, exp = TRUE)
#Result: employed 2.06**
#Result 2: no significance for subj. measures

#Model 5: pji3 + finnow3cat*employed + finfut.imp*employed + cci 
highf5 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6highf)
summary(highf5)
summ(highf5, exp = TRUE)
#Result: pji3 0.64*
#Result 2: employed 1.87*

#Model 6: pji3 + finnow3cat*employed + finfut.imp*employed + cci + hh income
highf6 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + fihhmnnet4_dv + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6highf)
summary(highf6)
summ(highf6, exp = TRUE)
#Result: employed 2.00**

#Model 7: pji3 + finnow3cat*employed + finfut.imp*employed + cci + partner
highf7 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + combo + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6highf)
summary(highf7)
summ(highf7, exp = TRUE)
#Result: employed 2.36***
#Result 2: all partner significant

#Take away: Models 5 & 7 are the best fits

#Similar to men only pji and employed matter but do so significantly. After partnership employed STILL matter and there seems to be a bonus for an employed partner

###########################################################################
# Employed sample ---------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Model for high educated men ---------------------------------------------
# -------------------------------------------------------------------------

#Model 1: pji
highempm1 <- glm(formula = event2 ~ t2_3 + pji3 + agemn + agesq + cohort2 + immigrant,
                family = binomial(link = "cloglog"),
                data = survemphighm)
summary(highempm1)
summ(highempm1, exp = TRUE)
# Result: pji3 0.45**

#Model 2: jbsec + permcon + parttime
highempm2 <- glm(formula = event2 ~ t2_3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = survemphighm)
summary(highempm2)
summ(highempm2, exp = TRUE)
# Result: parttime 0.64**

#Model 3: pji + jbsec + permcon + parttime
highempm3 <- glm(formula = event2 ~ t2_3 + pji3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = survemphighm)
summary(highempm3)
summ(highempm3, exp = TRUE)
# Result: parttime 0.64**
# Result 2: pji3 0.46*

#Model 4: pji + jbsec + permcon + parttime + finnow3cat + finfut.imp
highempm4 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci + pji3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = survemphighm)
summary(highempm4)
summ(highempm4, exp = TRUE)
# Result: finddifficult 1.54*
# Result 2: worse off 1.45**
# Result 3: pji3 0.45*
# Result 4: parttime 0.61**

#Model 5: pji + jbsec + permcon + parttime + finnow3cat + finfut.imp + cci + hh income
highempm5 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci + pji3 + jbsec.dummy + permcon + parttime + fihhmnnet4_dv + agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = survemphighm)
summary(highempm5)
summ(highempm5, exp = TRUE)
# Result: pji3 0.43**
# Result 2: parttime 0.63*

#Model 6: pji + jbsec + parttime + finnow3cat*parttime + finfut.imp*parttime
highempm6 <- glm(formula = event2 ~ t2_3 + finnow3cat*parttime  + finfut.imp*parttime + cci + pji3 + jbsec.dummy +  permcon  + agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = survemphighm)
summary(highempm6)
summ(highempm6, exp = TRUE)
# Result: the interaction changes nothing

#Model 7: pji + jbsec + parttime + finnow3cat*parttime + finfut.imp*parttime  + hh income
highempm7 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci + pji3 + jbsec.dummy + permcon + parttime + combo + agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = survemphighm)
summary(highempm7)
summ(highempm7, exp = TRUE)
# Result: Only partnership significant

# It appears that pji and parttime have a significant negative effect. Oddly enough finding it difficult and worse off do as well. After partnership all else disappears but it there isn't a strong partner's employment effect

# -------------------------------------------------------------------------
# Model for high educated women -------------------------------------------
# -------------------------------------------------------------------------

#Model 1: pji + employed
highempf1 <- glm(formula = event2 ~ t2_3 + pji3 +  agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = survemphighf)
summary(highempf1)
summ(highempf1, exp = TRUE)
# Result: no sign.

#Model 2: jbsec + permcon + parttime
highempf2 <- glm(formula = event2 ~ t2_3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = survemphighf)
summary(highempf2)
summ(highempf2, exp = TRUE)
# Result: no sign.

#Model 3: pji + jbsec + permcon + parttime
highempf3 <- glm(formula = event2 ~ t2_3 + pji3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = survemphighf)
summary(highempf3)
summ(highempf3, exp = TRUE)
# Result: no sign.

#Model 4: pji + jbsec + permcon + parttime + finnow3cat + finfut.imp
highempf4 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci + pji3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = survemphighf)
summary(highempf4)
summ(highempf4, exp = TRUE)
# Result: no sign.

#Model 5: pji + jbsec + permcon + parttime + finnow3cat + finfut.imp + cci + hh income
highempf5 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci + pji3 + jbsec.dummy + permcon + parttime + fihhmnnet4_dv + agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = survemphighf)
summary(highempf5)
summ(highempf5, exp = TRUE)
# Result: get by 0.70**
# Result 2: better off 0.80*

#Model 6: pji + jbsec + parttime + finnow3cat*parttime + finfut.imp*parttime  + hh income
highempf6 <- glm(formula = event2 ~ t2_3 + finnow3cat*parttime  + finfut.imp*parttime + cci + pji3 + jbsec.dummy +  permcon  + agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = survemphighf)
summary(highempf6)
summ(highempf6, exp = TRUE)
# Result: the interaction makes the subj. non-sign.

#Model 7: pji + jbsec + parttime + finnow3cat*parttime + finfut.imp*parttime  + hh income
highempf7 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci + pji3 + jbsec.dummy + permcon + parttime + combo + agemn + agesq + cohort2 + immigrant,
             family = binomial(link = "cloglog"),
             data = survemphighf)
summary(highempf7)
summ(highempf7, exp = TRUE)
# Result: better off 0.81*
# Result: all partnership sign. - married-employed big bonus

#There is almost no effect from the objective measures. 
# Better off is associated with negative fertility even after partnership. Seems to be a sizable bonus for married-employed


###########################################################################
# Outputs -----------------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# High Men ----------------------------------------------------------------
# -------------------------------------------------------------------------

#html
export_summs(highm1, highm5, highm7, highempm3, highempm4,highempm7, 
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
             file.name = "high_full_men_13-10-21.html")


# -------------------------------------------------------------------------
# High Women --------------------------------------------------------------
# -------------------------------------------------------------------------

#html
export_summs(highf1, highf5, highf7, highempf3, highempf4,highempf7, 
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
             file.name = "high_full_women_13-10-21.html")


# -------------------------------------------------------------------------
# Combined Output ---------------------------------------------------------
# -------------------------------------------------------------------------

#html
export_summs(highf5, highf7, highempf4, highempf7, highm5, highm7,  highempm4, highempm7, 
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
             file.name = "high_full-emp_18-10-21.html")

#word
export_summs(highf5, highf7, highempf4, highempf7, highm5, highm7,  highempm4, highempm7, 
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
             file.name = "high_full-emp_18-10-21.docx")

