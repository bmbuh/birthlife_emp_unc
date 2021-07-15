#Coded by: Brian Buh
#Started on: 15.07.2021
#Last Updated: 

surv5 <- file.choose()
surv5 <- readRDS(surv5)

#PJI First 3 years post-education
pji_3yr <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/pji_busetta_mendola/panel_pji_3yr_run.dta")


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
library(jtools)

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
#12190 observations are excluded, 184 birth events (41930 included, 2021 events)

loss_obs_2.5yr <- surv5 %>% 
  mutate(early = ifelse(t1 < 30, 1, 0)) %>% 
  count(early, event)
#10952 observations are excluded, 158 birth events (43168 included, 2047 events)

loss_obs_2yr <- surv5 %>% 
  mutate(early = ifelse(t1 < 24, 1, 0)) %>% 
  count(early, event)
#9066 observations are excluded, 123 birth events (45054 included, 2082 events)

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



testglm3 <- glm(formula = event ~ t2 + agemn + agesq + pji3 + finnow3cat + finfut.imp + employed + edu_cat,
               family = binomial(link = "cloglog"),
               data = surv6)
testglm2 <- glm(formula = event ~ t2 + agemn + agesq + pji2 + finnow3cat + finfut.imp + employed + edu_cat,
               family = binomial(link = "cloglog"),
               data = surv7)
testglm2.5 <- glm(formula = event ~ t2 + agemn + agesq + pji2.5 + finnow3cat + finfut.imp + employed + edu_cat,
               family = binomial(link = "cloglog"),
               data = surv8)
testglm1 <- glm(formula = event ~ t2 + agemn + agesq + pji1 + employed + edu_cat,
                  family = binomial(link = "cloglog"),
                  data = surv9)
testglm1.5 <- glm(formula = event ~ t2 + agemn + agesq + pji1.5 + employed + edu_cat,
                family = binomial(link = "cloglog"),
                data = surv10)
summ(testglm3, exp = TRUE)
summ(testglm2, exp = TRUE)
summ(testglm2.5, exp = TRUE)
summ(testglm1, exp = TRUE)
summ(testglm1.5, exp = TRUE)
plot_summs(testglm, exp = TRUE)

