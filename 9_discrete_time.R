#Coded by: Brian Buh
#Started on: 12.03.2021
#Last Updated: 16.04.2021

# install.packages("lme4")
# install.packages("survey")
# install.packages("jtools")
# install.packages("ggstance")
# install.packages("joots") #for plotting visualisation of parameter effects
# install.packages("broom.mixed")\
# install.packages("effects")

library(data.table)
library(padr)
library(tidyverse)
library(haven)
library(lubridate)
library(arsenal)
library(zoo)
library(survival)
library(survminer)
library(survPen)
library(flexsurv)
library(coxme)
library(stargazer)
library(texreg)
library(forestplot)
library(sjPlot)

library(lme4)
library(survey)
library(jtools)
library(ggstance)
# library(joots) #not available for this version of R
library(broom.mixed)
library(effects)

###########################################################################
# Discrete Time Model First Test ------------------------------------------
###########################################################################

# surv <- file.choose()
# surv <- readRDS(surv)
# 
# survcut <- surv %>% 
#   dplyr::select(pidp, time1, event)

#The model is not converging. Look into what can be done about this!
# dth <- glmer(event ~ time1 + (1 + time1|pidp),  data = survcut, family = "binomial")
# summary(dth)
# 
# des <- svydesign(ids = ~pidp, data = surv)
# dth2 <- svyglm(event ~ time1, design = des, family = "binomial")
# summary(dth2)
# 
# coxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee , data = surv, cluster = pidp, method = "exact")


############################################################################
# Loading data Surv2 (with subj/obj) ---------------------------------------
############################################################################

surv2 <- file.choose()
surv2 <- readRDS(surv2)

#The edu_cat hasn't be updated to have the issues fixed with isced97
surv2 %>% count(edu_cat)
surv2 %>% count(isced97)
#surv3 fixes this issue by creating new variable "edu"

#"surv3" is for modifications made on this script
#For modification of variables
surv3 <- surv2 %>% 
  # filter(edu_cat != "other") %>% #I made the decision to remove other category as it is mainly people who were not raised in the UK
  mutate(worse = ifelse(finfut.num == -1, 1, 0)) %>%  #A binary variable for people who think their finances will get worse
  mutate(comf = ifelse(finnow.num > 2, 1, 0)) %>%  #Creates a binary for positive versus negative current financial stability
  mutate(employed = ifelse(is.na(employed), 0, employed)) %>% 
  mutate(edu = case_when(
    isced97 == 2 ~ "low",
    isced97 == 3 | isced97 == 4  ~ "medium",
    isced97 == 5 | isced97 == 6  ~ "high",
    is.na(isced97) ~ "other")) %>% 
  filter(edu != "other")
  
surv3 %>% count(finnow.num)
surv3 %>% count(finnow.imp)
surv3 %>% count(comf)
surv3 %>% count(edu)
surv3 %>% count(is.na(jbisco88_cc))



#Create separate data sets for men and women
#Removes "other" educational level
survm <- surv3 %>% filter(sex == 1)
survf <- surv3 %>% filter(sex == 2)

# surv2m <- surv2 %>% filter(sex==1) %>%  mutate(edu_cat = fct_relevel(edu_cat, c("other", "high", "medium", "low")))
# surv2f <- surv2 %>% filter(sex==2) %>%  mutate(edu_cat = fct_relevel(edu_cat, c("other", "high", "medium", "low")))

###TO DO
#Descriptive statistics of the sample
mycontrols <- tableby.control(test = FALSE)
surv2stats <-arsenal::tableby(sex ~ se_ee + finnow.imp + finfut.imp + jbsec + edu_cat + combo, data = surv3, control = mycontrols)
labels(surv2stats) <-  c(se_ee = "PJI", finnow.imp = "Present Financial Outlook", finfut.imp = "Future Financial Outlook",
                        jbsec = "Job Security", edu_cat = "Educational Attainment", combo = "Partnership, Partner's Job Status")
summary(surv2stats)
write2word(surv2stats, "surv2stats.doc")


###########################################################################
# Dataframes for looking at ethnic/immigrant variance ---------------------
###########################################################################

# substat <- surv %>% 
#   mutate(finnow.imp = fct_relevel(finnow.imp, c("5 Finding it very difficult", "4 Finding it quite difficult",
#                                                 "3 Just getting by", "2 Doing alright", "1 Living comfortably"))) %>%
#   mutate(finnow.num = as.numeric(finnow.imp)) %>% 
#   #I change the scale of finfut to be centered at 0
#   mutate(finfut.imp = fct_relevel(finfut.imp, c( "Worse off", "About the same", "Better off"))) %>% 
#   mutate(finfut.num = as.numeric(finfut.imp)) %>% 
#   mutate(finfut.num = recode(finfut.num,
#                              "2" = "0",
#                              "1" = "-1",
#                              "3" = "1")) %>% 
#   mutate(finfut.num = as.integer(finfut.num)) %>% 
#   fill(jbsec, .direction = "downup") %>% #Note this si done for quick testing on the past slide. Consider its use!!!
#   mutate(jbsec = fct_relevel(jbsec, c("3 non-employed", "1 likely", "2 unlikely"))) %>%
#   mutate(jbsec2 = as.numeric(jbsec))
# 
# str(substat)
# 
# surv3 <- surv2 %>% 
#   mutate(gor_dv = as.character(gor_dv)) %>% 
#   #Create categories for ethnicity based on Kulu&Hannemann2016
#   mutate(ethnic = ifelse(racel_dv == 1, 1, #english, scottish, welsh, ni
#                          ifelse(racel_dv == 2 | racel_dv == 3 | racel_dv == 4, 2, #other white
#                                 ifelse(racel_dv == 9, 3, #indian
#                                        ifelse(racel_dv == 10, 4, #pakistani
#                                               ifelse(racel_dv == 11, 5, #bangladeshi
#                                                      ifelse(racel_dv == 14, 7, #carribean
#                                                             ifelse(racel_dv == 12 | racel_dv == 13, 6, #other asian
#                                                                    ifelse(racel_dv == 15, 8, 9))))))))) %>% #african or other
#   mutate(ethnic = as.character(ethnic)) %>%  
#   mutate(ethnic = recode(ethnic,
#                          "1" = "UK",
#                          "2" = "Other White",
#                          "3" = "Indian",
#                          "4" = "Pakistani",
#                          "5" = "Bangladeshi",
#                          "6" = "Other Asian",
#                          "7" = "Caribbean",
#                          "8" = "African",
#                          "9" = "Mixed/Other")) %>%
#   mutate(ethnic = fct_relevel(ethnic, c( "UK",
#                                          "Other White",
#                                          "Indian",
#                                          "Pakistani",
#                                          "Bangladeshi",
#                                          "Other Asian",
#                                          "Caribbean",
#                                          "African",
#                                          "Mixed/Other"))) %>% 
#   mutate(gen = ifelse(generation == 1, 1, 2)) %>% #Creates a binary of immigrants versus born UK
#   mutate(gen = recode(gen,
#                       "1" = "First Generation",
#                       "2" = "UK Born")) %>% 
#   mutate(gen = as.character(gen)) %>% 
#   unite(genethnic, ethnic, gen,  sep = "-", remove = FALSE) %>% 
#   unite(sexethnic, sex, ethnic, sep = "-", remove = FALSE) %>% 
#   unite(ethnicsex, ethnic, sex, sep = "-", remove = FALSE) %>% 
#   mutate(gor_dv = recode(gor_dv,
#                          "1" = "North East",
#                          "2" = "North West",
#                          "3" = "Yorkshire and Humberside",
#                          "4" = "East Midlands",
#                          "5" = "West Midlands",
#                          "6" = "East of England",
#                          "7" = "London",
#                          "8" = "South East",
#                          "9" = "South West",
#                          "10" = "Wales",
#                          "11" = "Scotland",
#                          "12" = "Northern Ireland",
#                          "-9" = "missing"))
# 
# 
# str(surv3)
# 
# surv4 <- surv3 %>% 
#   filter(t2 > 400, event == 1)
# 
# surv4 %>% 
#   count(event)
# 
# 
###########################################################################
# Graphing relationship of Hazard, Time & Covariate -----------------------
###########################################################################

###The Following graph the relationship between the hazard, time, and covariate
#Looking at the visual relationship between the birth hazard and time since end of education
#by sex
surv2 %>%
  group_by(t2, sex) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = t2, y = log(-log(1-hazard)))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~sex)

#Running the full Gompertz model
#First plots to see the effect of covariates
#Model Fit
#Sex and time since end of education
surv6 %>%
  mutate(sex = as.character(sex)) %>%
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>%
  group_by(t2, sex) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = t2,
             y = log(-log(1-hazard)),
             col = sex)) +
  geom_point() +
  geom_smooth() +
  labs(col = "Sex") +
  ylab("log(-log(1 - hazard)))") +
  xlab("Months since end of formal education") +
  ggsave("glm_hazard_sex.png")

#Sex and age
surv3 %>%
  mutate(sex = as.character(sex)) %>%
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>%
  group_by(agemn, sex) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = agemn,
             y = log(-log(1-hazard)),
             col = sex)) +
  geom_point() +
  geom_smooth() +
  labs(col = "Sex") +
  ylab("log(-log(1 - hazard)))") +
  xlab("Age in months") +
  ggsave("glm_hazard_age_sex.png")

#Education and Time Model Fit
surv2 %>%
  group_by(t2, edu_cat) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  # filter(event ==1) %>% 
  ggplot(aes(x = t2,
             y = log(-log(1-hazard)),
             col = edu_cat)) +
  geom_point() +
  geom_smooth() +
  ylab("log(-log(1 - hazard)))") +
  xlab("Months since end of formal education") +
  ggsave("flm_hazard_edu.png")

surv3 %>%
  mutate(worse = as.character(worse)) %>% 
  group_by(t2, worse, sex) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  # filter(event ==1) %>% 
  ggplot(aes(x = t2,
             y = log(-log(1-hazard)),
             col = worse)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~sex)

surv3 %>%
  mutate(comf = as.character(comf)) %>% 
  group_by(t2, comf, sex) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  # filter(event ==1) %>% 
  ggplot(aes(x = t2,
             y = log(-log(1-hazard)),
             col = comf)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~sex)

surv3 %>%
  group_by(t2, se_ee, sex) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  # filter(event ==1) %>% 
  ggplot(aes(x = t2,
             y = log(-log(1-hazard)))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~sex)


#Fit of Age and Education
surv2 %>%
  group_by(agemn, edu_cat) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = agemn,
             y = log(-log(1-hazard)),
             col = edu_cat)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~sex)

#Fit of Time since Edu and finnow
survm %>%
  group_by(t2, finnow.imp) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = t2,
             y = log(-log(1-hazard)),
             col = finnow.imp)) +
  geom_point() +
  geom_smooth()



############################################################################
## Testing Gompertz GLM Model ----------------------------------------------
############################################################################

#Creating a baseline to see the time since education and event
baselineglm <- glm(formula = event ~ t2,
                   family = binomial(link = "cloglog"),
                   data = surv3)

summary(baselineglm)
#The strong relationship between t2 and event in this models
#signifies that the baseline hazard is the same for all individuals ( :-) )
summ(baselineglm, exp = TRUE) #takes a minute to process


testglm <- glm(formula = event ~ t2 + agemn + agesq + se_ee + comf + worse*employed + edu_cat,
                family = binomial(link = "cloglog"),
                data = surv5)
testglm2 <- glm(formula = event ~ t2 + agesq + se_ee + finnow.imp + worse + employed + edu_cat,
               family = binomial(link = "cloglog"),
               data = surv3)

summary(testglm)
summ(testglm, exp = TRUE, scale = TRUE) #exp = TRUE means that we want exponentiated estimates
summ(testglm2, exp = TRUE, scale = TRUE) #Creates a scale effect using the SD (aka how much individual things effect the hazard)
plot_summs(testglm, exp = T, scale = T)
plot_summs(testglm2, exp = T, scale = T) #Note: "agemn" is so important that in order to see variation I needed to remove it

#Likelihood Ratio Test
anova(baselineglm, testglm, test = "Chisq")

#AIC
baselineglm$aic
testglm$aic



plot(allEffects(testglm))
ggsave("paramater_effect.png")

surv4 <- surv3 %>% filter(!is.na(event))


###This doesn't work for whatever reason!!!
#Deviance Residuals
Data_DevResid <- tibble(Pred_Haz = predict(mglm, type = "response"),
                        Event = pull(survm, event),
                        ID = pull(survm, pidp))%>%
  mutate(DevRes = if_else(Event == 0, 
                          -sqrt(-2*log(1-Pred_Haz)),
                          sqrt(-2*log(Pred_Haz))))

Data_DevResid %>%
  ggplot(aes(x = ID, y = DevRes)) +
  geom_point()

###########################################################################
# Gender Specific models & Model fit testing ------------------------------
###########################################################################


####Model for men
baseline_mglm <- glm(formula = event ~ t2,
                     family = binomial(link = "cloglog"),
                     data = survm)
summ(baseline_mglm, exp = TRUE, scale = TRUE)
mglm <- glm(formula = event ~ t2 + agemn + agesq + se_ee + finnow.imp*employed + finfut.imp*employed + edu_cat,
               family = binomial(link = "cloglog"),
               data = survm)
summary(mglm)
summ(mglm, exp = TRUE, scale = TRUE) #exp = TRUE means that we want exponentiated estimates

#Likelihood Ratio Test
anova(baseline_mglm, mglm, test = "Chisq")

#AIC
baseline_mglm$aic
mglm$aic

#Deviance Residuals
Data_DevResid_m <- tibble(Pred_Haz = predict(mglm, type = "response"),
                        Event = pull(survm, event),
                        ID = pull(survm, pidp))%>%
  mutate(DevRes = if_else(Event == 0, 
                          -sqrt(-2*log(1-Pred_Haz)),
                          sqrt(-2*log(Pred_Haz))))

Data_DevResid_m %>%
  ggplot(aes(x = ID, y = DevRes)) +
  geom_point() +
  ggtitle("Deviance Residuals Men - end of education") +
  ggsave("dev_resid_men_endedu.png")


#######
#Test Age versus end of education
mglmage <- glm(formula = event ~ agemn + agesq + se_ee + finnow.imp*employed + finfut.imp*employed + edu_cat,
               family = binomial(link = "cloglog"),
               data = survm)

anova(mglm, mglmage, test = "Chisq")

mglm$aic
mglmage$aic

Data_DevResid_m_age <- tibble(Pred_Haz = predict(mglmage, type = "response"),
                              Event = pull(survm, event),
                              ID = pull(survm, pidp))%>%
  mutate(DevRes = if_else(Event == 0, 
                          -sqrt(-2*log(1-Pred_Haz)),
                          sqrt(-2*log(Pred_Haz))))

Data_DevResid_m_age %>%
  ggplot(aes(x = ID, y = DevRes)) +
  geom_point() +
  ggtitle("Deviance Residuals Men - age") +
  ggsave("dev_resid_men_age.png")



###########################################################################
####Model for women
baseline_fglm <- glm(formula = event ~ t2,
                     family = binomial(link = "cloglog"),
                     data = survf)
summ(baseline_fglm, exp = TRUE, scale = TRUE)
fglm <- glm(formula = event ~ t2 + agemn + agesq + se_ee + finnow.imp*employed + finfut.imp*employed + edu_cat,
            family = binomial(link = "cloglog"),
            data = survf)
summary(fglm)
summ(fglm, exp = TRUE, scale = TRUE) #exp = TRUE means that we want exponentiated estimates

#Likelihood Ratio Test
anova(baseline_fglm, fglm, test = "Chisq")

#AIC
baseline_fglm$aic
fglm$aic

#Deviance Residuals
Data_DevResid_f <- tibble(Pred_Haz = predict(fglm, type = "response"),
                        Event = pull(survf, event),
                        ID = pull(survf, pidp))%>%
  mutate(DevRes = if_else(Event == 0, 
                          -sqrt(-2*log(1-Pred_Haz)),
                          sqrt(-2*log(Pred_Haz))))

Data_DevResid_f %>%
  ggplot(aes(x = ID, y = DevRes)) +
  geom_point() +
  ggtitle("Deviance Residuals Women - time since end of education") +
  ggsave("dev_resid_women_endedu.png")


#######
#Test Age versus end of education
fglmage <- glm(formula = event ~ agemn + agesq + se_ee + finnow.imp*employed + finfut.imp*employed + edu_cat,
            family = binomial(link = "cloglog"),
            data = survf)

anova(fglm, fglmage, test = "Chisq")

fglm$aic
fglmage$aic

Data_DevResid_f_age <- tibble(Pred_Haz = predict(fglmage, type = "response"),
                          Event = pull(survf, event),
                          ID = pull(survf, pidp))%>%
  mutate(DevRes = if_else(Event == 0, 
                          -sqrt(-2*log(1-Pred_Haz)),
                          sqrt(-2*log(Pred_Haz))))

Data_DevResid_f_age %>%
  ggplot(aes(x = ID, y = DevRes)) +
  geom_point() +
  ggtitle("Deviance Residuals Women - age") +
  ggsave("dev_resid_women_age.png")




###########################################################################
# Covariate Testing -------------------------------------------------------
###########################################################################




###Goodness-of-Fit tests
#AIC Test (comparison)
testglm$aic
mglm$aic
fglm$aic


plot_models(mglm, fglm, 
            title = "Hazard Ratios",
            m.labels = c("Men", "Women"),
            legend.title = "Model",
            # axis.labels = c(
            #  "finfut.num:employed", "finnow.num:employed",
            # #   # "Married - unknown", "Married - non-employed","Married - employed",
            # #   # "Cohab - non-employed", "Cohab - employed","Single",
            #   "Edu. Low", "Edu. Medium", "Edu. High",
            #   # "Job security",
            #   "Future Financial Sit",
            #   "Employed",
            #   "Present Financial Sit",
            #   "PJI",
            #   "Age Squared", "Age, in months", "Time"),
            # axis.lim = c(0.5, 1.4),
            dot.size = 6,
            #colors  = c("#2E9FDF", "#E7B800"), #in case you wanna change to the gold blue set
            p.shape = TRUE,
            grid = TRUE)

 ggsave("glm_m_f.png")


 
 
 
 
###########################################################################
# glmer discrete time models ----------------------------------------------
###########################################################################

#first step is to plot the Baseline Gompertz Regression model

surv2 %>%
  group_by(t2) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = t2, 
             y = log(-log(1-hazard)))) +
  geom_point() +
  geom_smooth()


testmultglm_baseline <- glmer(formula = event ~ t2 + (1|pidp),
                               family = binomial(cloglog),
                               data = surv3,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 2e5)))

summary(testmultglm_baseline)


#The similarity between the AIC in this model and the above GLM model suggest this "Basic Frailty Model" is unneccary 
testmultglm <- glmer(formula = event ~ t2 + se_ee*t2 + finnow.num*employed + finfut.num*employed + edu_cat + (1|pidp),
                     family = binomial(cloglog),
                     data = surv3,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5))) #This is to control for the warning "Model is nearly unidentifiable"

summary(testmultglm)

#Likelihood Ratio Test
anova(baselineglm, testglm, testmultglm, test = "Chisq")

#Test with newly transformed times

coxph <- coxph(formula = Surv(t1, t2, event) ~ tt(se_ee) + agemn + agesq + finnow.num*employed + finfut.num*employed + edu_cat, data = surv2, cluster = pidp, method = "breslow")
summary(coxph)
testph <- cox.zph(coxph)
summary(testph)

kmtest <- survfit(Surv(t1, t2, event) ~ strata (sex), data = surv2, cluster = pidp)
summary(kmtest)
plot(kmtest)

ggsurvplot(kmtest, size = 1,   # change line size
           #ylim = c(0.69,1),
           # palette = c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = TRUE,          # Add confidence interval
           # pval = TRUE,              # Add p-value
           risk.table = TRUE,        # Add risk table
           # risk.table.col = "strata",# Risk table color by groups
           legend.labs =
           c("Men", "Women"),    # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw()      # Change ggplot2 theme
           ) 


# jbsec stats -------------------------------------------------------------

  
  
testemp <- surv3 %>% 
  group_by(pidp, sex, kdob) %>% 
  summarise(emp = mean(employed)) %>% 
  mutate(fullemp = ifelse(emp == 1, 1, 0)) %>% 
  ungroup() %>% 
  mutate(fb = ifelse(is.na(kdob), 0, 1))

testemp %>% count(sex, fullemp, fb)  

testemp %>% summary(emp)

testemp %>% 
  ggplot(aes(x = emp)) +
  geom_histogram(binwidth = 0.1)



# Subjective Measures -----------------------------------------------------

surv3 %>%
  group_by(t2, finnow.imp, employed) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
interaction.plot(x.factor = finnow.imp,
                 trace.factor = employed,
                 response = hazard,
                 fun = median)

surv3 %>% 
  ggplot(aes(x = ))
                 








