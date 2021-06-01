#Coded by: Brian Buh
#Started on: 26.05.2021
#Last Updated: 27.05.2021

surv5 <- file.choose()
surv5 <- readRDS(surv5)

#From script 7; allows for adding start/end dates to the data frames
intdates <- file.choose()
intdates <- readRDS(intdates)

library(data.table)
library(tidyverse)
library(haven)
library(effects)

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
  select(-jbsec.num, -hhorig, -neg, -negstu) %>% 
  arrange(pidp, desc(wave)) %>% 
  filter(employed == 1) %>%  #There were still same observations for unemployed observations after making all other cuts
  group_by(pidp) %>% 
  mutate(eventcheck = row_number()) %>% #There is an issue the the event variable had some events occur in odd waves and was lost in the filtering
  mutate(event2 = ifelse(eventcheck == 1 & fb == 1, 1, 0)) %>% #"event2" is updated to make sure all births are accounted for in even waves
  ungroup() %>% 
  arrange(pidp, wave) %>% 
  select(-eventcheck, -gor_dv) %>% 
  left_join(., endates, by = c("pidp", "wave")) %>% 
  mutate(t2_2 = ifelse(event2 == 0, NA, t2_2)) %>% 
  mutate(timecheck = t2 - t2_2) %>% 
  dplyr::select(-finfut.num, -finnow.num) %>% 
  mutate(t2_3 = ifelse(timecheck >= 0, t2, t2_2)) %>% 
  mutate(t2_3 = ifelse(is.na(t2_3), t2, t2_3))  #This final number ensures that the timing of the conception is correct

survemp %>% count(parttime)

#The objective job measure variables have ~8% missing values.
#I do a simple imputation here to see if it is worth it to do a more advanced one.
survemp2 <- survemp %>% 
  fill(permcon, .direction = "up") %>% 
  fill(parttime, .direction = "up") %>% 
  fill(jbsat, .direction = "up") %>% 
  fill(jbpl, .direction = "up") %>% 
  mutate(jbpl = as.character(jbpl)) %>% 
  fill(priv, .direction = "up")
#It appears that only permcon has a significant effect.  

survemp2 %>% count(jbpl)

# Discrete time hazard model

survemp %>% count(event2)  
str(survemp)
testglm <- glm(formula = event2 ~ t2_3 + agemn + agesq + se_ee + jbsec.dummy*permcon + edu_cat,
               family = binomial(link = "cloglog"),
               data = survemp2)
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

#Interaction Test 3 - jbsec priv
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
survmemp <- survemp2 %>% filter(edu_cat != "other", sex == 1)
#Women Dataframe
survfemp <- survemp2 %>% filter(edu_cat != "other", sex == 2)

empmglm <- glm(formula = event2 ~ t2_3 + agemn + agesq + se_ee + jbsec.dummy + jbpl + edu_cat,
               family = binomial(link = "cloglog"),
               data = survmemp)
summary(empmglm)
summ(empmglm, exp = TRUE, scale = TRUE) #exp = TRUE means that we want exponentiated estimates


empfglm <- glm(formula = event2 ~ t2_3 + agemn + agesq + se_ee +  jbsec.dummy + jbpl +  edu_cat,
               family = binomial(link = "cloglog"),
               data = survfemp)
summary(empfglm)
summ(empfglm, exp = TRUE, scale = TRUE) #exp = TRUE means that we want exponentiated estimates






##########################################################################
# Transform data set to monthly observations ------------------------------
###########################################################################

#This code is takes from script 3 to be used here if needed
pji1 <- transform(pji_sample, from = as.Date(start_date), to = as.Date(end_date), lagfb = as.Date(lagged_kdob))

pji2 <- pji1 %>%
  dplyr::select(pidp, unemp, to, from, lagfb, age45f, age50m, sex, hhorig, dob, kdob)

dt <- data.table(pji2)
pji3 <- dt[, list(pidp, unemp, lagfb, age45f, age50m, sex, hhorig, dob, kdob, date = seq(from, to, by = "month")), by = 1:nrow(dt)] %>% 
  mutate(age_start = (dob %--% date)/dyears(1)) %>% 
  filter(age_start >= 16) 
