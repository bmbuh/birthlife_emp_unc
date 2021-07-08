#Coded by: Brian Buh
#Started on: 25.01.2020
#Last Updated: 24.02.2021

# install.packages("padr")
# install.packages("data.table")
library(data.table)
library(padr)
library(tidyverse)
library(haven)
library(lubridate)
library(arsenal)


#This script takes the UKHLS employment histories as complied by Liam Wright
#and transforms them in a way that helps create variables for our project

# install.packages("sjlabelled")
# library(sjlabelled)

#life_his is a retrospective reconstruction of employment status
#annual_his is a prospective reconstruction of employment status (through Wave 10)
emp_his_com <- 
  bind_rows(life_his, annual_his) %>%
  dplyr::select(1:19) %>%
  mutate(Wave = Wave-18) %>%
  mutate(unemp = ifelse(Status == 1 , 0, ifelse(Status == 2, 0, ifelse(Status == 100, 0, 1)))) %>%
  mutate(student = ifelse(Status == 7, 1, 0)) %>% 
  group_by(pidp, Wave) %>%
  mutate(Wave = as.integer(Wave))

#rename columns to make them R friendly

oldnames = c("pidp", "Wave", "Spell", "Status", "start_date", "start_m", "start_y",
             "end_date", "end_m", "end_y", "int_date", "int_m", "int_y",              
             "Job_Hours", "Job_Change", "Start_Flag", "End_Flag",
             "End_Ind", "Status_Spells", "unemp", "student")
newnames = c("pidp", "wave", "spell", "status", "start_date", "start_m", "start_y",
             "end_date", "end_m", "end_y", "int_date", "int_m", "int_y",
             "job_hours", "job_change", "start_flag", "end_flag",
             "end_und", "status_spells", "unemp", "student")

emp_his_com2 <- emp_his_com %>%
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>% 
  dplyr::select(-start_m, -start_y, -end_m, -end_y, -int_y, -int_m) %>% 
  arrange(pidp, start_date) %>% 
  group_by(pidp) %>% 
  mutate(spell2 = row_number()) %>% 
  ungroup() %>% 
  relocate("spell2", .after = "spell") %>% 
  dplyr::select(-spell) %>% 
  mutate(stu_spell = student * spell2) #creates a number which identifies which spell was the education spell

emp_his_com2 %>% 
  count(unemp)

###########################################################################
# Saving Employment/Education Histories -----------------------------------
###########################################################################
remove(emp_his)

saveRDS(emp_his_com2, file = "emp_his.rds")
emp_his <- file.choose()
emp_his<- readRDS(emp_his)


#labels
# str(emp_his)
# 
# get_labels(emp_his$status)
# sjlabelled::label_to_colnames(emp_his$status)



###########################################################################
# Descriptives of dateframe -----------------------------------------------
###########################################################################

xtabs(~employed + status, emp_his)


testemp <- emp_his %>% 
  arrange(pidp, wave)

testemp %>% 
  group_by(pidp) %>% 
  summarize(unique_obs = n_distinct("pidp"))

testemp %>% 
  mutate(status = as.character(status)) %>% 
  # group_by(pidp) %>% 
  count(status)

###########################################################################
# Fertility and Emp -----------------------------------------------
###########################################################################
#Use xwave data to find the data needed to filter the sample
# x_sample2 <- x_sample %>% 
#   rename("pidp" = "mpidp")

#merges the data from 
#x_sample comes from script "2_fert_hist"
emp_fb <- 
  left_join(emp_his, x_sample, by = "pidp") %>% 
  left_join(., first_born, by = "pidp") %>% 
  rename("hhorig" = "hhorig.x") %>% 
  dplyr::select(-hhorig.y)

emp_fb %>% 
  mutate(year = year(kdob)) %>% 
  filter(year >= 2008) %>% 
  count(year)


#removes samples from BHPS and starting wave 6
emp_fb_ukhls <- emp_fb %>% 
  filter(hhorig == 1 | hhorig == 2 | hhorig == 7) %>% 
  dplyr::select(-job_hours, -job_change, -start_flag, -end_flag, -end_und, -status_spells)


#The sample that will be used to produce the PJI
#Left censors: a.) finished education (not including those who go back) [lca]
#             b.) Age 16 [lcb]
#Right censors: a.) birth of first child [rca]
#              b.) age 45 [rcb]
#               c.) end of data [rcc]
pji_sample <- emp_fb_ukhls %>% 
  filter(stu_spell != 1) %>% #This removes spells before the entrance into the labor market [lca]
  mutate(start_date = as.POSIXct(start_date)) %>%
  #mutate(age_start = (dob %--% start_date)/dyears(1)) %>%  #This is the process to remove respondents less than 16 [lcb]
  # filter(age_start >= 16) %>% #this filter is too early! This needs to be down when we have monthly records or we remove whole spells!
  mutate(lagged_kdob = kdob - dmonths(8)) %>% 
  mutate(age45f = dob + dyears(45)) %>% 
  mutate(age50m = dob + dyears(50)) %>% 
  rename("sex" = "sex.x") %>% 
  select(-sex.y) %>% 
  mutate(childcheck = ifelse(is.na(kdob), 1, 0)) %>% #This is to remove people who have a child but no listed kdob
  mutate(childcheck2 = childcheck * anychild_dv) %>% 
  filter(childcheck2 != 1)

saveRDS(pji_sample, file = "pji_sample.rds")
pji_sample <- file.choose()
pji_sample<- readRDS(pji_sample)


############################################################################
# Creation of panel data for PJI ------------------------------------------
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


###########################################################################
# Export as dta -----------------------------------------------------------
###########################################################################

write_dta(panel_pji, "panel_pji.dta")

###########################################################################
# Import PJI Complete -----------------------------------------------------
###########################################################################

#Last update 24.02.2021
pji_complete <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/rds/panel_pji_run.dta")


pji_dem <- pji2 %>% 
  dplyr::select(-unemp, -to, -from)

pji_var <- pji_complete %>% 
  rename( "pidp" = "id") %>% 
  left_join(., pji_dem, by = "pidp") %>% 
  distinct(pidp, yr, .keep_all = TRUE) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(fbyear = year(lagfb)) %>% 
  filter(sex != -9) %>% 
  mutate(sex = recode(sex,
                      "1" = "Male",
                      "2" = "Female")) %>% 
  mutate(fbyes = ifelse(is.na(fbyear), 0, 1))  %>% 
  mutate(fbyes = recode(fbyes,
                      "0" = "No birth",
                      "1" = "Birth")) %>% 
  mutate(year45f = year(age45f)) %>% 
  mutate(year50m = year(age50m))
  # mutate(fbyear = ifelse(is.na(fbyear), 3000, fbyear))

###Why did I do this?
# pji_var_na <- pji_var %>% 
#   filter(is.na(fbyear)) %>% 
#   mutate(year49 = year(age49)) %>% 
#   filter(year49 >= 2008)

#Note: this needs to be updated if rerun to reflect change in difference in age limit of men and women
#This is a test to check for number of newborns
test_pji_var <- pji_var %>% 
  mutate(year49 = year(age49)) %>% 
  filter(year49 >= 2008) %>% 
  group_by(pidp) %>% 
  mutate(time = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(pidp, fbyear, time) %>% 
  mutate(wave = "wave") %>% 
  unite(waves, wave, time, remove = TRUE) %>% 
  pivot_wider(names_from = waves, values_from = fbyear) %>% 
  mutate(fb = ifelse(wave_1 > 0, 1, 0))

test_pji_var %>% 
  filter(wave_1 >= 2008 | is.na(wave_1)) %>% 
  count(wave_1)

#Histogram of PJI of sample (truncated)
pji_var %>% 
  dplyr::filter(fbyear >= 2008 | is.na(fbyear), year45f >= 2008, year50m >= 2008, se_ee != 0) %>% 
  ggplot(aes(se_ee, fill = fbyes)) +
  geom_histogram(binwidth = 0.05) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Sex", caption = "Not shown, 0 = continiously employed") +
  xlab("Persistent Joblessness Index: 1 = Continiously Jobless") + 
  ggtitle("Truncated Persistent Joblessness Index", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) +
  ggsave("pji_hist_sex.png")

#Histogram of PJI of sample (NOT truncated)
pji_var %>% 
  dplyr::filter(fbyear >= 2008 | is.na(fbyear), year49 >= 2008) %>% 
  ggplot(aes(se_ee, fill = fbyes)) +
  geom_histogram(binwidth = 0.03) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Sex") +
  xlab("Persistent Joblessness Index: 1 = Continiously Jobless") + 
  ggtitle("Persistent Joblessness Index", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) +
  ggsave("pji_hist_sex_full.png")

# pji_var %>% 
#   mutate(fbyes = as.character(fbyes)) %>% 
#   dplyr::filter(is.na(fbyear) | fbyear >= 2008) %>% 
#   ggplot(aes(se_ee, fill = fbyes)) +
#   geom_histogram(binwidth = 0.05) +
#   scale_fill_brewer(palette = "Set1") +
#   theme(aspect.ratio = 1) +
#   labs(fill = "Sex") +
#   xlab("Persistent Joblessness Index: 1 = Continiously Jobless") + 
#   ggtitle("Persistent Joblessness Index", subtitle =  "UKHLS = Measured 9 months before first birth") +
#   ggsave("pji_hist_second.png")

pji_var %>% 
  count(fbyes)

pji_var_cut <- pji_var %>% 
  dplyr::filter(is.na(fbyear) | fbyear >= 2008) %>% 
  mutate(year49 = year(age49)) %>% 
  filter(year49 > 2008)

wide_pji_f <- pji_var_cut %>% 
  filter(sex == "Female") %>% 
  dplyr::select(pidp, fbyear, time) %>% 
  mutate(wave = "wave") %>% 
  unite(waves, wave, time, remove = TRUE) %>% 
  pivot_wider(names_from = waves, values_from = fbyear) %>% 
  mutate(fb = ifelse(wave_1 > 0, 1, 0))

wide_pji_m <- pji_var_cut %>% 
  filter(sex == "Male") %>% 
  dplyr::select(pidp, fbyear, time) %>% 
  mutate(wave = "wave") %>% 
  unite(waves, wave, time, remove = TRUE) %>% 
  pivot_wider(names_from = waves, values_from = fbyear) %>% 
  mutate(fb = ifelse(wave_1 > 0, 1, 0))


wide_pji_f %>% 
  count(wave_1)

wide_pji_m %>% 
  count(wave_1)

pji_var_cut <- pji_var %>% 
  filter(!is.na(se_ee), fbyear >= 2008 | is.na(fbyear), year49 >= 2008)

pji_var_cut %>% 
  count(hhorig)

mycontrols <- tableby.control(test = FALSE)
pji_table <-arsenal::tableby(sex ~ fbyes + fbyear + se_ee + yr + year49, data = pji_var_cut, control = mycontrols)
labels(pji_table) <-  c(sex = "Sex", fbyes = "First birth", fbyear =  "Year first born", se_ee = "PJI",
                    yr = "Year first obs of emp hist", year49 =  "Year turn 49")
summary(pji_table)
write2word(pji_table, "pji_table.doc")
