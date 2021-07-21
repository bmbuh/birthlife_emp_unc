#Coded by: Brian Buh
#Started on: 26.05.2021
#Last Updated: 08.06.2021

# install.packages("huxtable")
# install.packages("sandwich")
# install.packages("modelsummary")
# install.packages("officer")
# install.packages("flextable")

surv5 <- file.choose()
surv5 <- readRDS(surv5)

# saveRDS(surv5, "surv5.rds")

#From script 7; allows for adding start/end dates to the data frames
intdates <- file.choose()
intdates <- readRDS(intdates)

library(data.table)
library(tidyverse)
library(haven)
library(effects)
library(huxtable) #for the export_summs function
library(sandwich) #for the export_summs function, robust SE
library(officer) #for the export_summs function, export to word
library(flextable) #for the export_summs function, export to word
library(modelsummary)


surv5 %>% count(jbsec)

#First attempt at transforming the dataset to just working individuals
#
endates <-  surv5 %>% 
  mutate(t2_2 = ifelse(event == 1, t2, NA)) %>% 
  group_by(pidp) %>% 
  dplyr::select(pidp, wave, t2_2) %>% 
  fill(t2_2, .direction = "up") %>% 
  ungroup()

survemp <-  surv5 %>% 
  mutate(fb = ifelse(is.na(kdob), 0, 1)) %>% 
  filter(wave == 2 | wave == 4 | wave == 6 | wave == 8 | wave == 10) %>% #This removes all non-response waves
  mutate(jbsec = as.character(jbsec)) %>% 
  mutate(jbsec = ifelse(jbsec == "3 non-employed", NA, jbsec)) %>% #This transforms the variable back to NA for the respondents with no job
  filter(!is.na(jbsec)) %>% 
  mutate(jbsec.dummy = ifelse(jbsec == "1 likely", 1, 0)) %>% #This dummy variable is for people who are likely or very likely to lose their job in the next 12 months
  mutate(jbsec.dummy = as.character(jbsec.dummy)) %>% 
  select(-jbsec.num, -neg, -negstu) %>% 
  arrange(pidp, desc(wave)) %>% 
  filter(employed == 1) %>%  #There were still same observations for unemployed observations after making all other cuts
  group_by(pidp) %>% 
  mutate(eventcheck = row_number()) %>% #There is an issue the the event variable had some events occur in odd waves and was lost in the filtering
  mutate(event2 = ifelse(eventcheck == 1 & fb == 1, 1, 0)) %>% #"event2" is updated to make sure all births are accounted for in even waves
  fill(permcon, .direction = "downup") %>%  
  ungroup() %>% 
  arrange(pidp, wave) %>% 
  select(-eventcheck, -gor_dv) %>% 
  left_join(., endates, by = c("pidp", "wave")) %>% 
  mutate(t2_2 = ifelse(event2 == 0, NA, t2_2)) %>% 
  mutate(timecheck = t2 - t2_2) %>% 
  dplyr::select(-finfut.num, -finnow.num) %>% 
  mutate(t2_3 = ifelse(timecheck >= 0, t2, t2_2)) %>% 
  mutate(t2_3 = ifelse(is.na(t2_3), t2, t2_3)) %>% 
  rename("isco88" = "jbisco88_cc") %>% 
  mutate(isco88 = as.integer(isco88)) %>% 
  mutate(isco = case_when(
    isco88 >= 100 & isco88 <=199 ~ "1",
    isco88 >= 200 & isco88 <=299 ~ "2",
    isco88 >= 300 & isco88 <=399 ~ "3",
    isco88 >= 400 & isco88 <=499 ~ "4",
    isco88 >= 500 & isco88 <=599 ~ "5",
    isco88 >= 600 & isco88 <=699 ~ "6",
    isco88 >= 700 & isco88 <=799 ~ "7",
    isco88 >= 800 & isco88 <=899 ~ "8",
    isco88 >= 900 & isco88 <=999 ~ "9",
    isco88 == 10 ~ "10",
    isco88 < 0 ~ "NA")) %>% 
  mutate(isco = ifelse(isco == "NA", NA, isco))

# %>% #This final number ensures that the timing of the conception is correct
  # left_join(., imp_permcon, by = c("pidp", "wave")) #This dataframe is created a bit further down but retrospectively added here to ensure there is not issues
survemp %>% count(isco)

# Save survemp DF ---------------------------------------------------------

saveRDS(survemp, "survemp.rds")
survemp<- file.choose()
survemp<- readRDS(survemp)

survemp %>% count(isco)
survemp %>% count(permcon)
survemp %>% count(permcon.imp)
survemp2 %>% count(permcon)

#The objective job measure variables have ~8% missing values.
#I do a simple imputation here to see if it is worth it to do a more advanced one.
# survemp2 <- survemp %>% 
#   fill(permcon, .direction = "up") %>% 
#   fill(parttime, .direction = "up") %>% 
#   fill(jbsat, .direction = "up") %>% 
#   fill(jbpl, .direction = "up") %>% 
#   mutate(jbpl = as.character(jbpl)) %>% 
#   fill(priv, .direction = "up")
#It appears that only permcon has a significant effect.  

xsex <- xsex %>% mutate(pidp = as.factor(pidp))
fb_check <- fb_check %>% mutate(pidp = as.factor(pidp))

#Impute missing values into permcon
wide_permcon <- survemp %>% 
  dplyr::select(pidp, wave, permcon) %>% 
  # mutate(finnow = as.factor(finnow)) %>% 
  mutate(wn = "w") %>% 
  #mutate(finnow = ifelse(finnow <= -1, NA, finnow)) %>% 
  unite(wavenum, wn, wave, sep = "", remove = TRUE) %>% 
  pivot_wider(names_from = wavenum, values_from = permcon) %>% 
  mutate_if(is.numeric, as.factor) %>% 
  left_join(., xsex, by = "pidp") %>% #From script 6
  left_join(., fb_check, by = "pidp") %>% 
  mutate(fb = ifelse(is.na(fb), 0, fb))

#First Imputation of permcon
imp <-  mice(wide_permcon, m = 5, method = "logreg")
densityplot(imp)
fit <- with(imp, glm(sex ~ w2 + w4 + w6 + w8 + w10, family = binomial(link = "logit")))
summary(pool(fit))

#My fitness tests are low. It seems to me that the data is not truly missing at random.
#Later tests seem to confirm this. The distributions are similar, but when using the MICE adding permcon
#based on priors seems to take away the effect that simple line deletion has. Something is going on here.
imp_wide_permcon = complete(imp, 4)

imp_permcon<- imp_wide_permcon %>% 
  #dplyr::select(-sex, -fb) %>% 
  pivot_longer(cols = c("w2", "w4", "w6", "w8", "w10"), names_to = "wavename", values_to = "permcon.imp") %>%
  group_by(pidp) %>% 
  mutate(wave = row_number()) %>% 
  mutate(wave = ifelse(wave == 5, 10, ifelse(wave == 4, 8, ifelse(wave == 3, 6, ifelse(wave == 2, 4, 2))))) %>% 
  ungroup() %>% 
  dplyr::select(-wavename) 


############################################################################
# Discrete time hazard model - employed -----------------------------------
###########################################################################

survemp %>% count(event2)  
str(survemp)
testglm <- glm(formula = event2 ~ t2_3 + agemn + agesq + se_ee + jbsec.dummy*permcon.imp + edu_cat + isco,
               family = binomial(link = "cloglog"),
               data = survemp)
summary(testglm)
summ(testglm, exp = TRUE, scale = TRUE) #exp = TRUE means that we want exponentiated estimates

#Interaction Test 1 - jbsec parttime
#
interaction.plot(x.factor = survemp2$parttime,
                 trace.factor = survemp2$jbsec.dummy,
                 response = survemp2$hazard)

boxplot(hazard ~ parttime * jbsec.dummy, data=survemp2)


aov15 <- aov(hazard ~ parttime * jbsec.dummy, data=survemp2)
summary(aov15)
model.tables(aov15, type = "means")
lm15 <- lm(hazard ~ parttime * jbsec.dummy, data=survemp2)
summary(lm15)

plot(allEffects(aov15), multiline=TRUE, ci.style="bars")

#Interaction Test 2 - jbsec permcon
#
interaction.plot(x.factor = survemp2$permcon,
                 trace.factor = survemp2$jbsec.dummy,
                 response = survemp2$hazard)

boxplot(hazard ~ permcon * jbsec.dummy, data=survemp2)


aov16 <- aov(hazard ~ permcon * jbsec.dummy, data=survemp2)
summary(aov16)
model.tables(aov16, type = "means")
lm16 <- lm(hazard ~ permcon * jbsec.dummy, data=survemp2)

summary(lm16)

plot(allEffects(aov16), multiline=TRUE, ci.style="bars")

interaction.plot(x.factor = survmemp$permcon,
                 trace.factor = survmemp$jbsec.dummy,
                 response = survmemp$hazard)

#Interaction for women
interaction.plot(x.factor = survfemp$permcon,
                 trace.factor = survfemp$jbsec.dummy,
                 response = survfemp$hazard)


###Interaction Test 3 - jbsec priv
#
interaction.plot(x.factor = survemp2$priv,
                 trace.factor = survemp2$jbsec.dummy,
                 response = survemp2$hazard)

boxplot(hazard ~ priv * jbsec.dummy, data=survemp2)


aov17 <- aov(hazard ~ priv * jbsec.dummy, data=survemp2)
summary(aov17)
model.tables(aov17, type = "means")
lm17 <- lm(hazard ~ priv * jbsec.dummy, data=survemp2)
summary(lm17)

plot(allEffects(aov17), multiline=TRUE, ci.style="bars")

#Interaction Test 4 - jbsec jbpl
#
interaction.plot(x.factor = survemp2$jbsec.dummy,
                 trace.factor = survemp2$jbpl,
                 response = survemp2$hazard)

boxplot(hazard ~ jbpl * jbsec.dummy, data=survemp2)


aov18 <- aov(hazard ~ jbpl * jbsec.dummy, data=survemp2)
summary(aov18)
model.tables(aov18, type = "means")
lm18 <- lm(hazard ~ jbpl * jbsec.dummy, data=survemp2)
summary(lm18)

plot(allEffects(aov18), multiline=TRUE, ci.style="bars")

#############################################################################
# Gender specific models ----------------------------------------------------
#############################################################################

#Men Dataframe
survmemp <- survemp %>% filter(sex == 1)
#Women Dataframe
survfemp <- survemp %>% filter(sex == 2)

empmglm <- glm(formula = event2 ~ t2_3 + se_ee + jbsec.dummy + permcon + edu + agemn + agesq,
               family = binomial(link = "cloglog"),
               data = survmemp)
summary(empmglm)
summ(empmglm, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

survfemp %>% count(jbsec.dummy)
empfglm <- glm(formula = event2 ~ t2_3 + se_ee +  jbsec.dummy + permcon +  edu + agemn + agesq,
               family = binomial(link = "cloglog"),
               data = survfemp)
summary(empfglm)
summ(empfglm, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

###########################################################################
# Model output ------------------------------------------------------------
###########################################################################

#Descriptive stats
statsurvemp <- survemp %>% 
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  #mutate(fb = ifelse(is.na(kdob), 0, 1)) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women"))

mycontrols <- tableby.control(test = FALSE)
empstats <-arsenal::tableby(fb ~ t2_3 + sex + se_ee + jbsec.dummy + permcon + edu_cat, data = statsurvemp, control = mycontrols)
labels(empstats) <-  c(t2_3 = "Time since end of education (months)", sex = "Sex", se_ee = "PJI", permcon = "Permanent contract",
                        jbsec.dummy = "Job Security", edu_cat = "Educational Attainment")
summary(empstats)
write2word(empstats , "empstats .docx")




export_summs(empmglm, empfglm,
             model.names = c("Men","Women"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2_3",
                        "PJI" = "se_ee",
                        "Job security" = "jbsec.dummy1",
                       "Permanent contract" = "permcon1",
                       "Education - Low" = "edu_catlow",
                       "Education - Medium" = "edu_catmedium",
                       "Age in Months" = "agemn",
                       "Age Squared" = "agesq"),
                       # "jbsec:permcon" = "jbsec.dummy1:permcon1"),
             exp = TRUE,
             to.file = "docx",
             file.name = "emp_model_paper1_8-6-21.docx")


plot_summs(empmglm, empfglm,
           model.names = c("Men", "Women"),
           coefs = c("Time since Education" = "t2_3",
                     "PJI" = "se_ee",
                     "Job security" = "jbsec.dummy1",
                     "Permanent contract" = "permcon1",
                     "Education - Low" = "edu_catlow",
                     "Education - Medium" = "edu_catmedium",
                     "Age in Months" = "agemn",
                     "Age Squared" = "agesq",
                     "jbsec:permcon" = "jbsec.dummy1:permcon1"),
           exp = TRUE) +
  ggtitle("Employed Sample") +
  ggsave("emp_model_paper1_8-6-21.png")

###modelsummary regression output approach
# row <- data.frame("Coefficients" = "Reference category: High",
#                   "Model 1" = "",
#                   "Model 2" = "")
# attr(row, "position") <- 11
# modelsummary(list(empmglm, empfglm), 
#              output = "table.html",
#              exp = TRUE, 
#              stars = TRUE,
#              coef_rename = c("t2_3" = "Time since Education",
#                       "se_ee" =  "PJI",
#                       "jbsec.dummy1" =  "Job security",
#                       "permcon1"  = "Permanent contract",
#                       "edu_catlow" = "Education - Low",
#                       "edu_catmedium" = "Education - Medium",
#                       "agemn" = "Age in Months",
#                       "agesq" = "Age Squared",
#                       "jbsec.dummy1:permcon1" = "jbsec*permcon"),
#              add_rows = row)


##########################################################################
# For entire sample (fix for later open science) --------------------------
###########################################################################
#Men Dataframe
survm <- surv5 %>% filter(edu_cat != "other", sex == 1) %>% ungroup()
#Women Dataframe
survf <- surv5 %>% filter(edu_cat != "other", sex == 2) %>% ungroup()



#Descriptive stats
statsurv <- surv5 %>% 
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(fb = ifelse(is.na(kdob), 0, 1)) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ungroup()

mycontrols <- tableby.control(test = FALSE)
fullstats <-arsenal::tableby(fb ~ t2 + sex + se_ee + finnow3cat + finfut.imp + employed + edu_cat, data = statsurv, control = mycontrols)
labels(fullstats) <-  c(t2 = "Time since end of education (months)", sex = "Sex", se_ee = "PJI", employed = "Employed",
                       finnow3cat = "Present Finacial", finfut.imp = "Future Finacial", edu_cat = "Educational Attainment")
summary(fullstats)
write2word(fullstats , "fullstats .docx") 

statsurv %>% count(immigrant)

#Male Model
mglm <- glm(formula = event ~ t2 + se_ee + finnow3cat+employed + finfut.imp + edu_cat + agemn + agesq,
            family = binomial(link = "cloglog"),
            data = survm)
summary(mglm)
summ(mglm, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

#Female Model
fglm <- glm(formula = event ~ t2 + se_ee + finnow3cat*employed + finfut.imp + edu_cat + agemn + agesq,
            family = binomial(link = "cloglog"),
            data = survf)
summary(fglm)
summ(fglm, exp = TRUE)

export_summs(mglm,fglm,
             model.names = c("Men", "Women"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2",
                       "PJI" = "se_ee",
                       "Finding it difficult" = "finnow3catfinddifficult",
                       "Getting by" = "finnow3catgetby",
                       "Employed" = "employed",
                       "Worse off" = "finfut.impWorse off",
                       "Better off" = "finfut.impBetter off",
                       "Education Low" = "edu_catlow",
                       "Education Medium" = "edu_catmedium",
                       "Age in Months" = "agemn",
                       "Age Squared" = "agesq",
                       "finddifficult:employed" = "finnow3catfinddifficult:employed",
                       "gettingby:employed" = "finnow3catgetby:employed"),
             exp = TRUE,
             to.file = "docx",
             file.name = "full_model_paper1_8-6-21.docx")


plot_summs(mglm, fglm, 
           model.names = c("Men", "Women"),
           coefs = c("Time since Education" = "t2",
                     "PJI" = "se_ee",
                     "Finding it difficult" = "finnow3catfinddifficult",
                     "Getting by" = "finnow3catgetby",
                     "Employed" = "employed",
                     "Worse off" = "finfut.impWorse off",
                     "Better off" = "finfut.impBetter off",
                     "Education Low" = "edu_catlow",
                     "Education Medium" = "edu_catmedium",
                     "Age in Months" = "agemn",
                     "Age Squared" = "agesq",
                     "finddifficult:employed" = "finnow3catfinddifficult:employed",
                     "gettingby:employed" = "finnow3catgetby:employed"),
           exp = TRUE) +
  ggsave("full_model_paper1_8-6-21.png")


##########################################################################
# Transform data set to monthly observations ------------------------------
###########################################################################

#This code is takes from script 3 to be used here if needed
# pji1 <- transform(pji_sample, from = as.Date(start_date), to = as.Date(end_date), lagfb = as.Date(lagged_kdob))
# 
# pji2 <- pji1 %>%
#   dplyr::select(pidp, unemp, to, from, lagfb, age45f, age50m, sex, hhorig, dob, kdob)
# 
# dt <- data.table(pji2)
# pji3 <- dt[, list(pidp, unemp, lagfb, age45f, age50m, sex, hhorig, dob, kdob, date = seq(from, to, by = "month")), by = 1:nrow(dt)] %>% 
#   mutate(age_start = (dob %--% date)/dyears(1)) %>% 
#   filter(age_start >= 16) 
