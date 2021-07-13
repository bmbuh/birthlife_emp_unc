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



###########################################################################
# PJI data transformation -------------------------------------------------
###########################################################################
#pji1-pji4 help prepares the data in the same way I would need to make the time varying pji

#preps the dates for transformation
pji1 <- transform(pji_sample, from = as.Date(start_date), to = as.Date(end_date), lagfb = as.Date(lagged_kdob))

#Selects the variables needed
pji2 <- pji1 %>%
  dplyr::select(pidp, unemp, to, from, lagfb, age45f, age50m, sex, hhorig, dob, kdob)

#Using the data.table package transforms from spells to months
dt <- data.table(pji2)
pji3 <- dt[, list(pidp, unemp, lagfb, age45f, age50m, sex, hhorig, dob, kdob, date = seq(from, to, by = "month")), by = 1:nrow(dt)] %>% 
  mutate(age_start = (dob %--% date)/dyears(1)) %>% 
  filter(age_start >= 16) 

# The transformed data creates overlapping months, removes the second month 
#Removes individuals' histories after first birth or over 45/50
pji4 <- pji3 %>% 
  distinct(pidp, date, .keep_all = TRUE) %>% 
  filter(date <= lagfb | is.na(lagfb)) %>% 
  filter(case_when(sex == 1 ~ age_start <= 50, sex == 2 ~ age_start <= 45)) #Updated 24.02.21 to change cut off ages for men and women based on ONS Stats



# Adaptations July 2021 ---------------------------------------------------


#It is possible to add the interview dates from the all_sample 
checkpji <- 
  left_join(pji4, wave_dates, by = c("pidp", "date")) %>% 
  mutate(waveck = ifelse(!is.na(wave), wave, 0)) %>% 
  mutate(waveck = as.character(waveck)) %>% 
  group_by(pidp) %>% 
  fill(c("wave", "intdate_ck","fwintvd_dv", "lwintvd_dv"), .direction = "up") %>% 
  mutate(num = row_number()) %>% 
  arrange(pidp, desc(num)) %>% 
  mutate(revnum = row_number()) %>% 
  ungroup()

#There is a large amount of NA. 
# This seems to be mainly individuals who did not appear in the UKHLS during childbearing years
checkpji %>% count(wave)

#Check for quality of sample
#Only looks at last month available
checkpji2 <- checkpji %>% 
  filter(!is.na(wave)) %>% 
  mutate(fb = ifelse(!is.na(kdob), 1, 0)) %>% 
  mutate(fb = as.character(fb)) %>% 
  filter(revnum == 1) 

checkpji2 %>% 
  ggplot(aes(x = num, fill = fb)) +
  geom_bar()+
  theme_minimal()+
  ggsave("checkpji2_fb_distribution_13072021.png")
#I have a left leaning distribution with a very long tail. I also have a significant number of births in month 1

#This allows us to see where the interview waves are taking place in months since the end of education
#Note: uses checkpji (to have all months included)
wave1check <- checkpji %>% 
  filter(num < 36) %>% 
  count(waveck)

checkpji %>% 
  filter(waveck != "0") %>% 
  ggplot(aes(x = num, fill = waveck)) +
  geom_bar()+
  theme_minimal()+
  ggsave("checkpji2_wave_distribution_13072021.png")

#Check to see if I make an indicator of first three years, how many would I lose
checkpji3 <- checkpji2 %>% 
  mutate(early = ifelse(fb == 1 & num < 36, 1, 0)) %>% 
  mutate(late = ifelse(fb == 1 & num >= 36, 1, 0))

checkpji3 %>% count(early)
checkpji3 %>% count(late)

wave1check <- checkpji %>% 
  filter(num < 36) %>% 
  count(waveck)
  
  
#Checking to see interview placement in timeline

###########################################################################
# Back to setting up the process ------------------------------------------
###########################################################################

#Creating PJI for first three years after finishing education
pji5 <- checkpji %>% 
  filter(num <= 36)
###

#Used to create LP (unemp) and IP (emp_ratio) vectors
panel_pji_3yr <- pji5 %>% 
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

write_dta(panel_pji_3yr, "panel_pji_3yr.dta")

###################################################################
# Information about coding of PJI -----------------------------------------
###################################################################
# The main goal of the PJI is to create an index focused on NONEMPLOYMENT
# The PJI has 2 vectors:
#   LP [A vector of binary coded 1 and 0]
#     0 = a year spent 100% employed
#     1 = a year with any bout of nonemployment
#   IP is a vector of ratio of months spent in nonemployment
#     0 - .99999 = Sometime spent in employment
#     1 = a year spent 100% nonemployed (note this key difference)
#   LP vector is computed in the variable Nonemp
#     Each year should be given a 0 or a 1
#   IP vector is computed mn_emp
#     This is calculated by summing the months unemployed
#     divided by the number of months in the year (careful as starting and
#     ending years may not be 12 months)


pji_3yr <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/pji_busetta_mendola/panel_pji_3yr_run.dta")

pji_dem <- pji2 %>% 
  dplyr::select(-unemp, -to, -from)

pji_var <- pji_3yr %>% 
  rename( "pidp" = "id") %>% 
  left_join(., pji_dem, by = "pidp") %>% 
  distinct(pidp, yr, .keep_all = TRUE) %>% 
  mutate(sex = as.character(sex)) %>% 
  mutate(fbyear = year(lagfb)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Male",
                      "2" = "Female")) %>% 
  mutate(fb = ifelse(is.na(fbyear), 0, 1))  %>% 
  mutate(fb = recode(fb,
                        "0" = "No birth",
                        "1" = "Birth")) %>% 
  mutate(year45f = year(age45f)) %>% 
  mutate(year50m = year(age50m))


pji_var %>% 
  filter(!is.na(se_ee)) %>% 
  dplyr::filter(fbyear >= 2008 | is.na(fbyear), year45f >= 2008, year50m >= 2008) %>% 
  ggplot(aes(se_ee, fill = fb)) +
  geom_histogram(binwidth = 0.05) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Sex", caption = "Not shown, 0 = continiously employed") +
  xlab("Persistent Joblessness Index: 1 = Continiously Jobless") + 
  ggtitle("Persistent Joblessness Index", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) +
  ggsave("pji_3yr_histogram_13072021.png")


