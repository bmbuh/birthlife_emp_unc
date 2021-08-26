#Coded by: Brian Buh
#Started on: 26.08.2021
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
# library(plyr)

surv5 <- file.choose()
surv5 <- readRDS(surv5)

#DF for first 3 years after the end of education
first3 <- surv5 %>% 
  filter(t1 < 36) 


# %>% 
  left_join(. , pji_3yr_2, by = "pidp") %>% 
  #Here I start to add my final controls
  ### Control cohort
  mutate(byr = year(dob)) %>% 
  mutate(cohort = ifelse(byr <= 1979, 0, 1)) %>% 
  mutate(cohort = as.character(cohort)) %>% 
  mutate(cohort2 = ifelse(byr <= 1975, "<=1975", ifelse(byr >= 1990, ">=1990", "1976-1989"))) %>% 
  mutate(cohort2 = as.character(cohort2)) %>% 
  mutate(cohort2 = fct_relevel(cohort2, c("1976-1989", "<=1975", ">=1990"))) %>% 
  mutate(cohort3 = ifelse(byr <= 1969, 0, ifelse(byr >= 1990, 3, ifelse(byr >= 1970 & byr <= 1979, 1, 2)))) %>% 
  mutate(cohort3 = as.character(cohort3)) %>% 
  left_join(., cci, by = "startdate")