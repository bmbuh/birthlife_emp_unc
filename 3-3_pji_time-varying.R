#Coded by: Brian Buh
#Started on: 08.07.2021
#Last Updated: 

# install.packages("padr")
# install.packages("data.table")
library(data.table)
library(padr)
library(tidyverse)
library(haven)
library(lubridate)
library(arsenal)

###########################################################################
# Dataframes --------------------------------------------------------------
###########################################################################

#combined life and annual histories
emp_his <- file.choose()
emp_his<- readRDS(emp_his)

# It removes spells before the end of formal education
# Creates a lagged kdob for 9 months before the birth
# Sets the upper age limit of 45 for women and 50 for men
# Checks to make sure that there is no one in the sample that has claimed to have a child but the info about the child’s DOB is not available.
pji_sample <- file.choose()
pji_sample<- readRDS(pji_sample)


#From script 6_subj_control
#Useful for extracting info needed for cutting the material
all_sample <- file.choose()
all_sample <- readRDS(all_sample)


###########################################################################
# Editing the data frames -------------------------------------------------
###########################################################################

wave_dates <- all_sample %>% 
  dplyr::select(pidp, wave, intdatm_dv, intdaty_dv, fwintvd_dv, lwintvd_dv) %>% 
  unite(intdate_ck, c(intdatm_dv, intdaty_dv), sep = "-") %>% #combines the mnth & yr var to make int dates
  mutate(intdate_ck = parse_date_time(intdate_ck, "my")) %>% 
  mutate(date = intdate_ck)

#It is possible to add the interview dates from the all_sample 
checkpji <- 
  left_join(pji4, wave_dates, by = c("pidp", "date")) %>% 
  group_by(pidp) %>% 
  fill(c("wave", "intdate_ck","fwintvd_dv", "lwintvd_dv"), .direction = "up") %>% 
  ungroup()

checkpji %>% count(wave)


###########################################################################
# PJI data transformation -------------------------------------------------
###########################################################################


pji1 <- transform(pji_sample, from = as.Date(start_date), to = as.Date(end_date), lagfb = as.Date(lagged_kdob))

pji2 <- pji1 %>%
  dplyr::select(pidp, unemp, to, from, lagfb, age45f, age50m, sex, hhorig, dob, kdob)

dt <- data.table(pji2)
pji3 <- dt[, list(pidp, unemp, lagfb, age45f, age50m, sex, hhorig, dob, kdob, date = seq(from, to, by = "month")), by = 1:nrow(dt)] %>% 
  mutate(age_start = (dob %--% date)/dyears(1)) %>% 
  filter(age_start >= 16) 

###Here is out problem with losing first borns
pji4 <- pji3 %>% 
  # mutate(date = as.POSIXct(date)) %>% 
  # mutate(age49 = as.POSIXct(age49)) %>% 
  # mutate(lagfb = as.POSIXct(lagfb)) %>% 
  # mutate(dob = as.POSIXct(dob)) %>% 
  # mutate(kdob = as.POSIXct(kdob)) %>% 
  # mutate(diff_fb = date - kdob) %>% #test to see if the lagged dates cause the problem
  # mutate(diff_lag = date - lagfb) %>%
  # mutate(diff_age = date - age49) %>% 
  distinct(pidp, date, .keep_all = TRUE) %>% 
  filter(date <= lagfb | is.na(lagfb)) %>% 
  filter(case_when(sex == 1 ~ age_start <= 50, sex == 2 ~ age_start <= 45)) #Updated 24.02.21 to change cut off ages for men and women based on ONS Stats
#filter(diff_fb <= 1| is.na(diff_fb)) %>% 
# mutate(fb_check = diff_fb == 0) %>% 
# filter(diff_age <= 0 | is.na(diff_age))%>% 
# dplyr::select(-diff_fb, -diff_age)
# There was previously a "pji5" and "pji6" which got rid of unneeded collumns and repeated rows. 
#Those functions have been put into "pji4"


#Used to create LP (unemp) and IP (emp_ratio) vectors
panel_pji <- pji4 %>% 
  mutate(yr = year(date), mn = month(date))%>% 
  group_by(pidp, yr) %>% 
  summarize(mn_amt = length(mn), mn_unemp = sum(unemp)) %>% 
  mutate(unemp = mn_amt * mn_unemp, 
         unemp = ifelse(unemp > 0, 1, unemp)) %>% 
  mutate(emp_ratio = (mn_unemp/mn_amt)) %>% 
  mutate(time = row_number()) %>%
  arrange(pidp, desc(time)) %>% 
  mutate(rev_time = row_number()) %>% 
  ungroup() 

