#Coded by: Brian Buh
#Started on: 15.07.2021
#Last Updated: 20.07.2021

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


###########################################################################
# Upload data files from previous scripts ---------------------------------
###########################################################################

surv5 <- file.choose()
surv5 <- readRDS(surv5)

#From script 7; allows for adding start/end dates to the data frames
intdates <- file.choose()
intdates <- readRDS(intdates)

#PJI First 3 years post-education
pji_3yr <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/pji_busetta_mendola/panel_pji_3yr_run.dta")
#PJI First 2 years post-education
pji_2yr <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/pji_busetta_mendola/panel_pji_2yr_run.dta")
#PJI First 2.5 years post-education
pji_2.5yr <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/pji_busetta_mendola/panel_pji_2-5yr_run.dta")
#PJI First 1 years post-education
pji_1yr <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/pji_busetta_mendola/panel_pji_1yr_run.dta")
#PJI First 1.5 years post-education
pji_1.5yr <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/pji_busetta_mendola/panel_pji_1-5yr_run.dta")


#Inspecting the data file for adapting

surv5 %>% 
  mutate(event = as.character(event)) %>% 
  ggplot(aes(x = t1, fill = event)) +
  geom_bar() +
  geom_vline(xintercept = 36, size = 1) +
  theme_minimal() +
  ggsave("t1_dist_surv5_15072021.png")
# Wave interviews that occur before 3 years after end of edu

loss_obs_3yr <- surv5 %>% 
  mutate(early = ifelse(t1 < 36, 1, 0)) %>% 
  count(early, event)
#12374 observations are excluded, 184 birth events (43951 included, 2021 events)

loss_obs_2.5yr <- surv5 %>% 
  mutate(early = ifelse(t1 < 30, 1, 0)) %>% 
  count(early, event)
#11110 observations are excluded, 158 birth events (45215 included, 2047 events)

loss_obs_2yr <- surv5 %>% 
  mutate(early = ifelse(t1 < 24, 1, 0)) %>% 
  count(early, event)
#9189 observations are excluded, 123 birth events (47136 included, 2082 events)

loss_obs_1.5yr <- surv5 %>% 
  mutate(early = ifelse(t1 < 18, 1, 0)) %>% 
  count(early, event)
#7989 observations are excluded, 101 birth events (48336 included, 2104 events)

loss_obs_1yr <- surv5 %>% 
  mutate(early = ifelse(t1 < 12, 1, 0)) %>% 
  count(early, event)
#6443 observations are excluded, 77 birth events (49882 included, 2128 events)

#Testing averages
###Note: In script 3.3 there is a DF called "sen_anal" with this file already created.

avg_pji <- surv5 %>% 
  arrange(pidp, desc(wave)) %>% 
  group_by(pidp) %>% 
  mutate(obsnum = row_number()) %>% 
  select(pidp, obsnum, wave, se_ee, event) %>% 
  ungroup() %>% 
  filter(obsnum == 1) %>% 
  rename("pjifull" = "se_ee") %>% 
  select(pidp, pjifull, event) %>% 
  left_join(., sen_anal, by = "pidp") %>% 
  mutate(diff = pjifull-pji3) %>% 
  mutate(diff2 = pjifull-pji2) %>% 
  mutate(event = as.character(event))

sum_avg_pji <- avg_pji %>% 
  summarise(meanfull = mean(pjifull), mean3 = mean(pji3), mean2.5 = mean(pji2.5),
            mean2 = mean(pji2), mean1.5 = mean(pji1.5), mean1 = mean(pji1),
            medianfull = median(pjifull), median3 = median(pji3), median2.5 = median(pji2.5),
            median2 = median(pji2), median1.5 = median(pji1.5), median1 = median(pji1),
            sdfull = sd(pjifull), sd3 = sd(pji3), sd2.5 = sd(pji2.5),
            sd2 = sd(pji2), sd1.5 = sd(pji1.5), sd1 = sd(pji1),
            n_distinctfull = n_distinct(pjifull), n_distinct3 = n_distinct(pji3), n_distinct2.5 = n_distinct(pji2.5),
            n_distinct2 = n_distinct(pji2), n_distinct1.5 = n_distinct(pji1.5), n_distinct1 = n_distinct(pji1))

summary(avg_pji$diff)

#Histogram of individual change from full PJI to 3 yr PJI
avg_pji %>% 
  ggplot(aes(diff, fill = event)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = "Full PJI - PJI3") +
  ggsave("pjifull-pji3_20072021.png")
#Histogram of individual change from full PJI to 2 yr PJI
avg_pji %>% 
  ggplot(aes(diff2, fill = event)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = "Full PJI - PJI2") +
  ggsave("pjifull-pji2_20072021.png")

avg_pji %>% 
  ggplot() +
  geom_histogram(aes(diff), fill = "blue", alpha = 0.7, binwidth = 0.05) +
  geom_histogram(aes(diff2), fill = "yellow", alpha = 0.5, binwidth = 0.05) +
  labs(title = "Difference between early career and fully observed PJI",
       subtitle = "3 years (blue) and 2 years (yellow)",
         x = "Full PJI - PJI2") +
  ggsave("pjifull-pji2andpji3_20072021.png")

# -------------------------------------------------------------------------
# Making a dataset fit for new model --------------------------------------
# -------------------------------------------------------------------------

pji_3yr_2 <- pji_3yr %>% 
  rename("pidp" = "id", "pji3" = "se_ee") %>% 
  select(pidp, pji3) %>% 
  filter(!is.na(pji3))

#DF for 3 years after the end of education
surv6 <- surv5 %>% 
  filter(t1 >= 36) %>% 
  left_join(. , pji_3yr_2, by = "pidp") 

#DF for 2 years after the end of education
surv7 <- surv5 %>% 
  filter(t1 >= 24) %>% 
  left_join(. , pji_2yr_2, by = "pidp")

#DF for 2.5 years after the end of education
surv8 <- surv5 %>% 
  filter(t1 >= 30) %>% 
  left_join(. , pji_2.5yr_2, by = "pidp")

#DF for 1 years after the end of education
surv9 <- surv5 %>% 
  filter(t1 >= 12) %>% 
  left_join(. , pji_1yr_2, by = "pidp")
 
#DF for 1.5 years after the end of education
surv10 <- surv5 %>% 
  filter(t1 >= 18) %>% 
  left_join(. , pji_1.5yr_2, by = "pidp")

surv5 %>% count(isced97)


testglm3 <- glm(formula = event ~ t2 + agemn + agesq + pji3 + employed + isced97,
               family = binomial(link = "cloglog"),
               data = surv6)
testglm2 <- glm(formula = event ~ t2 + agemn + agesq + pji2 + employed + isced97,
               family = binomial(link = "cloglog"),
               data = surv7)
testglm2.5 <- glm(formula = event ~ t2 + agemn + agesq + pji2.5 + employed + edu,
               family = binomial(link = "cloglog"),
               data = surv8)
testglm1 <- glm(formula = event ~ t2 + agemn + agesq + pji1 + employed + edu,
                  family = binomial(link = "cloglog"),
                  data = surv9)
testglm1.5 <- glm(formula = event ~ t2 + agemn + agesq + pji1.5 + employed + edu,
                family = binomial(link = "cloglog"),
                data = surv10)
summ(testglm3, exp = TRUE)
summ(testglm2, exp = TRUE)
summ(testglm2.5, exp = TRUE)
summ(testglm1, exp = TRUE)
summ(testglm1.5, exp = TRUE)
plot_summs(testglm, exp = TRUE)

