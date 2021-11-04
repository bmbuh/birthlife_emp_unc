#Coded by: Brian Buh
#Started on: 11.10.2021
#Last Updated: 02.11.2021


#Use the surv6 RDS created on script 13
#Low Education
surv6low <- surv6 %>% filter(edu == "low")
surv6lowm <- surv6low %>% filter(sex == 1)
surv6lowf <- surv6low %>% filter(sex == 2)

# -------------------------------------------------------------------
# Testing for the age of first birth and months from EE to conception
# -------------------------------------------------------------------

# create data sets to find the average age and months since end of education for those that experience first child birth
fbsurv6low <- surv6low %>% filter(event == 1)
fbsurv6lowm <- fbsurv6low %>% filter(sex == 1)
fbsurv6lowf <- fbsurv6low %>% filter(sex == 2)
summary(fbsurv6low$agebirth)
summary(fbsurv6lowm$agebirth)
summary(fbsurv6lowf$agebirth)
summary(fbsurv6lowm$t2)
summary(fbsurv6lowf$t2)

#Use the survemp RDS created on script 13
#Low Education
survemplow <- survemp3 %>% filter(edu == "low")
survemplowm <- survemplow %>% filter(sex == 1)
survemplowf <- survemplow %>% filter(sex == 2)

survemplowf %>% count(jbsec.dummy)

survemplowm %>% count(event)
survemplowf %>% count(event)

# create data sets to find the average age and months since end of education for those that experience first child birth
fbsurvemplow <- survemplow %>% filter(event == 1)
fbsurvemplowm <- fbsurvemplow %>% filter(sex == 1)
fbsurvemplowf <- fbsurvemplow %>% filter(sex == 2)
summary(fbsurvemplow$agebirth)
summary(fbsurvemplowm$agebirth)
summary(fbsurvemplowf$agebirth)
summary(fbsurvemplowm$t2_3)
summary(fbsurvemplowf$t2_3)


###########################################################################
# Testing the low educated variables -------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Model for lowly educated men -------------------------------------------
# -------------------------------------------------------------------------

#Model 1: PJI3 +employed
lowm1 <- glm(formula = event ~ t2 + pji3 + employed + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6lowm)
summary(lowm1)
summ(lowm1, exp = TRUE)
#Result: PJI 0.30***

#Model 2: finnow  + employed
lowm2 <- glm(formula = event ~ t2 + finnow3cat + employed + cci + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6lowm)
summary(lowm2)
summ(lowm2, exp = TRUE)
#Result: find difficult 1.44**
#Result 2: employed 1.68***

#Model 3: finfut + employed + cci 
lowm3 <- glm(formula = event ~ t2 + finfut.imp + employed +  cci + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6lowm)
summary(lowm3)
summ(lowm3, exp = TRUE)
#Result: better off 1.19*
#Result 2: employed 1.56***

#Model 4: finnow3cat*employed + finfut.imp*employed + cci 
lowm4 <- glm(formula = event ~ t2 + finnow3cat*employed + finfut.imp*employed + cci  + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6lowm)
summary(lowm4)
summ(lowm4, exp = TRUE)
#Result: employed 3.45***
# Result 2: find difficult 2.31**
# Result 3: better off 2.32***

#Model 5: pji3 + finnow3cat*employed + finfut.imp*employed + cci (note without employed finfut-worse off is significant)
lowm5 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6lowm)
summary(lowm5)
summ(lowm5, exp = TRUE)
#Result: pji3 0.35***
# Result: employed 2.21**
# Result 2: find difficult 2.18**
# Result 3: better off 2.19***

#Model 6: pji3 + finnow3cat*employed + finfut.imp*employed + cci + hh income
lowm6 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + fihhmnnet4_dv + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6lowm)
summary(lowm6)
summ(lowm6, exp = TRUE)
#Result: same result as without income

#Model 7: pji3 + finnow3cat*employed + finfut.imp*employed + cci + partner
lowm7 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + combo + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6lowm)
summary(lowm7)
summ(lowm7, exp = TRUE)
#Result: pji 0.67*
# Result 2: find difficult 2.11**
# Result: There is a small partner non-employed bonus

#Take away: Models 5 & 7 are the best fits

#It is clear that pji, employed, find difficult and better off have clear relationships to timing of first birth. Even with partnership pji is important. There is a small partner non-employed bonus.


# -------------------------------------------------------------------------
# Model for lowly educated women -----------------------------------------
# -------------------------------------------------------------------------

#Model 1: PJI3 + employed
lowf1 <- glm(formula = event ~ t2 + pji3 + employed + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6lowf)
summary(lowf1)
summ(lowf1, exp = TRUE)
#Result: no sign.

#Model 2: finnow  + employed
lowf2 <- glm(formula = event ~ t2 + finnow3cat + employed + cci + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6lowf)
summary(lowf2)
summ(lowf2, exp = TRUE)
#Result 2: employed 1.51**

#Model 3: finfut + employed + cci (note without employed finfut-worse off is significant)
lowf3 <- glm(formula = event ~ t2 + finfut.imp + employed +  cci + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6lowf)
summary(lowf3)
summ(lowf3, exp = TRUE)
#Result: employed 1.48**

#Model 4: finnow3cat*employed + finfut.imp*employed + cci 
lowf4 <- glm(formula = event ~ t2 + finnow3cat*employed + finfut.imp*employed + cci  + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6lowf)
summary(lowf4)
summ(lowf4, exp = TRUE)
#Result: employed 2.62***
#Result 2: better off 2.49***

#Model 5: pji3 + finnow3cat*employed + finfut.imp*employed + cci 
lowf5 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6lowf)
summary(lowf5)
summ(lowf5, exp = TRUE)
#Result: employed 2.33**
#Result 2: better off 2.40***

#Model 6: pji3 + finnow3cat*employed + finfut.imp*employed + cci + hh income
lowf6 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + fihhmnnet4_dv + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6lowf)
summary(lowf6)
summ(lowf6, exp = TRUE)
#Result: same result as without income

#Model 7: pji3 + finnow3cat*employed + finfut.imp*employed + cci + partner
lowf7 <- glm(formula = event ~ t2 + pji3 + finnow3cat*employed + finfut.imp*employed + cci  + combo + agemn + agesq + cohort2 + immigrant,
              family = binomial(link = "cloglog"),
              data = surv6lowf)
summary(lowf7)
summ(lowf7, exp = TRUE)
#Result: employed 2.01*
#Result 2: better off 2.35**
#Result 2: all partner significant but not huge

#Take away: Models 5 & 7 are the best fits

# It is clear that current employment status and perception of better off matter through all model runs

###########################################################################
# Employed sample ---------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Model for low educated men ---------------------------------------------
# -------------------------------------------------------------------------

#Model 1: pji
lowempm1 <- glm(formula = event2 ~ t2_3 + pji3 + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = survemplowm)
summary(lowempm1)
summ(lowempm1, exp = TRUE)
# Result: pji3 0.48*

#Model 2: jbsec + permcon + parttime
lowempm2 <- glm(formula = event2 ~ t2_3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = survemplowm)
summary(lowempm2)
summ(lowempm2, exp = TRUE)
# Result: parttime 0.60**

#Model 3: pji + jbsec + permcon + parttime
lowempm3 <- glm(formula = event2 ~ t2_3 + pji3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = survemplowm)
summary(lowempm3)
summ(lowempm3, exp = TRUE)
# Result: parttime 0.64*

#Model 4: pji + jbsec + permcon + parttime + finnow3cat + finfut.imp
lowempm4 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci +  pji3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = survemplowm)
summary(lowempm4)
summ(lowempm4, exp = TRUE)
# Result: finddifficult 1.51*
# Result 2: parttime 0.62*


#Model 5: pji + jbsec + permcon + parttime + finnow3cat + finfut.imp + hh income
lowempm5 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci +  pji3 + jbsec.dummy + permcon + parttime + fihhmnnet4_dv + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = survemplowm)
summary(lowempm5)
summ(lowempm5, exp = TRUE)
# Result: permcon 7.36**
# Results 2: parttime 0.51**
# Results 3: income removes sign. of subj. measure

#Model 6: pji + jbsec + permcon + finnow3cat*parttime + finfut.imp*parttime  + hh income
lowempm6 <- glm(formula = event2 ~ t2_3 + finnow3cat*parttime  + finfut.imp*parttime + cci +  pji3 + jbsec.dummy +  permcon  + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = survemplowm)
summary(lowempm6)
summ(lowempm6, exp = TRUE)
# Result: better off 1.46*

#Model 7: pji + jbsec + parttime + permcon + finnow3cat + finfut.imp + partner
lowempm7 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci +  pji3 + jbsec.dummy + permcon + parttime + combo + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = survemplowm)
summary(lowempm7)
summ(lowempm7, exp = TRUE)
#Result : huge bonus for non-employed partner

#Parttime consistently negatively affects fertility. finddifficult again appears. partnership rules everything plus a large non-employed partner bonus


# -------------------------------------------------------------------------
# Model for low educated women -------------------------------------------
# -------------------------------------------------------------------------

#Model 1: pji
lowempf1 <- glm(formula = event2 ~ t2_3 + pji3 + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = survemplowf)
summary(lowempf1)
summ(lowempf1, exp = TRUE)
# Result: no sign.

#Model 2: jbsec + permcon + parttime
lowempf2 <- glm(formula = event2 ~ t2_3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = survemplowf)
summary(lowempf2)
summ(lowempf2, exp = TRUE)
# Result: jbsec 2.73***

#Model 3: pji + jbsec + permcon + parttime
lowempf3 <- glm(formula = event2 ~ t2_3 + pji3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = survemplowf)
summary(lowempf3)
summ(lowempf3, exp = TRUE)
# Result: jbsec 2.29***

#Model 4: pji + jbsec + permcon + parttime + finnow3cat + finfut.imp
lowempf4 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci +  pji3 + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = survemplowf)
summary(lowempf4)
summ(lowempf4, exp = TRUE)
# Result: jbsec 2.66***


#Model 5: pji + jbsec + permcon + parttime + finnow3cat + finfut.imp + hh income
lowempf5 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci +  pji3 + jbsec.dummy + permcon + parttime + fihhmnnet4_dv + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = survemplowf)
summary(lowempf5)
summ(lowempf5, exp = TRUE)
# Result: jbsec 2.95***


#Model 6: pji + jbsec + permcon + finnow3cat*parttime + finfut.imp*parttime  + hh income
lowempf6 <- glm(formula = event2 ~ t2_3 + finnow3cat*parttime  + finfut.imp*parttime + cci +  pji3 + jbsec.dummy +  permcon  + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = survemplowf)
summary(lowempf6)
summ(lowempf6, exp = TRUE)
# Result: the interaction changes nothing

#Model 7: pji + jbsec + parttime + permcon + finnow3cat + finfut.imp + partner
lowempf7 <- glm(formula = event2 ~ t2_3 + finnow3cat + finfut.imp + cci + pji3 + jbsec.dummy + permcon + parttime + combo + agemn + agesq + cohort2 + immigrant,
                   family = binomial(link = "cloglog"),
                   data = survemplowf)
summary(lowempf7)
summ(lowempf7, exp = TRUE)
# Result: jbsec 2.62***
# Result: partnership important but no big partner employment bonus


#Take away: Models 4 and 7 work best
#It seems that employed low women are very significantly positively affected by perceiving they will lose their job in the next 12 months

###########################################################################
# Outputs -----------------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# low Men ----------------------------------------------------------------
# -------------------------------------------------------------------------

#html
export_summs(lowm1, lowm5, lowm7, lowempm3, lowempm4,lowempm7, 
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
             file.name = "low_full_men_13-10-21.html")


# -------------------------------------------------------------------------
# low Women --------------------------------------------------------------
# -------------------------------------------------------------------------

#html
export_summs(lowf1, lowf5, lowf7, lowempf3, lowempf4,lowempf7, 
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
             file.name = "low_full_women_13-10-21.html")

# -------------------------------------------------------------------------
# Combined Output ---------------------------------------------------------
# -------------------------------------------------------------------------

#html
export_summs(lowf5, lowf7, lowempf4, lowempf7, lowm5, lowm7,  lowempm4, lowempm7, 
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
             file.name = "low_full-emp_18-10-21.html")

#word
export_summs(lowf5, lowf7, lowempf4, lowempf7, lowm5, lowm7,  lowempm4, lowempm7, 
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
             file.name = "low_full-emp_18-10-21.docx")
