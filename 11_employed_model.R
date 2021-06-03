#Coded by: Brian Buh
#Started on: 26.05.2021
#Last Updated: 03.06.2021

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
  mutate(t2_3 = ifelse(is.na(t2_3), t2, t2_3))  %>% #This final number ensures that the timing of the conception is correct
  left_join(., imp_permcon, by = c("pidp", "wave")) #This dataframe is created a bit further down but retrospectively added here to ensure there is not issues
  
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
testglm <- glm(formula = event2 ~ t2_3 + agemn + agesq + se_ee + jbsec.dummy*permcon.imp + edu_cat,
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
survmemp <- survemp %>% filter(edu_cat != "other", sex == 1)
#Women Dataframe
survfemp <- survemp %>% filter(edu_cat != "other", sex == 2)

empmglm <- glm(formula = event2 ~ t2_3 + agemn + agesq + se_ee + jbsec.dummy*permcon + edu_cat,
               family = binomial(link = "cloglog"),
               data = survmemp)
summary(empmglm)
summ(empmglm, exp = TRUE, scale = TRUE) #exp = TRUE means that we want exponentiated estimates


empfglm <- glm(formula = event2 ~ t2_3 + agemn + agesq + se_ee +  jbsec.dummy*permcon +  edu_cat,
               family = binomial(link = "cloglog"),
               data = survfemp)
summary(empfglm)
summ(empfglm, exp = TRUE, scale = TRUE) #exp = TRUE means that we want exponentiated estimates

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
desstats <-arsenal::tableby(fb ~ t2 + sex + se_ee + jbsec.dummy + permcon + edu_cat, data = statsurvemp, control = mycontrols)
labels(desstats ) <-  c(sex = "Sex", se_ee = "PJI", finnow.imp = "Present Financial Outlook", finfut.imp = "Future Financial Outlook",
                        jbsec = "Job Security", edu_cat = "Educational Attainment", combo = "Partnership, Partner's Job Status")
summary(desstats )
#write2word(desstats , "desstats .doc")


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
