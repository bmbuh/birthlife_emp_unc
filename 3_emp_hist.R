#Coded by: Brian Buh
#Started on: 25.01.2020
#Last Updated: 10.02.2021

# install.packages("padr")
# install.packages("data.table")
library(data.table)
library(padr)
library(tidyverse)
library(haven)
library(lubridate)


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


#test to see where we are losing first births. Here!
#The answer is that some first borns came from BHSP or later waves
test_emp_fb <- emp_fb %>% 
  mutate(year = year(kdob)) %>% 
  group_by(pidp) %>% 
  mutate(time = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(pidp, year, time) %>% 
  mutate(wave = "wave") %>% 
  unite(waves, wave, time, remove = TRUE) %>% 
  pivot_wider(names_from = waves, values_from = year) %>% 
  mutate(fb = ifelse(wave_1 > 0, 1, 0))

count_emp_fb <- test_emp_fb %>% 
  filter(wave_1 >= 2008 | is.na(wave_1)) %>% 
  count(wave_1)

emp_fb %>% 
  count(hhorig)

#removes samples from BHPS and starting wave 6
emp_fb_ukhls <- emp_fb %>% 
  filter(hhorig == 1 | hhorig == 2 | hhorig == 7) %>% 
  dplyr::select(-job_hours, -job_change, -start_flag, -end_flag, -end_und, -status_spells)


#test to see where we are losing first births. Here!
test_emp_fb_ukhls <- emp_fb_ukhls %>% 
  mutate(year = year(kdob)) %>% 
  group_by(pidp) %>% 
  mutate(time = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(pidp, year, time) %>% 
  mutate(wave = "wave") %>% 
  unite(waves, wave, time, remove = TRUE) %>% 
  pivot_wider(names_from = waves, values_from = year) %>% 
  mutate(fb = ifelse(wave_1 > 0, 1, 0))

count_emp_fb_ukhls <- test_emp_fb_ukhls %>% 
  filter(wave_1 >= 2008 | is.na(wave_1)) %>% 
  count(wave_1)

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
  mutate(lagged_kdob = kdob - dmonths(9) + ddays(4)) %>% 
  mutate(age49 = dob + dyears(49)) %>% 
  rename("sex" = "sex.x") %>% 
  select(-sex.y)

pji_sample %>% 
  count(age_start)

str(pji_sample)

pji_sample %>% 
  count(stu_spell)

#I am losing first births between these steps, why???
pji_sample %>% 
  mutate(year = year(kdob)) %>% 
  filter(year >= 2008) %>% 
  count(year)


saveRDS(pji_sample, file = "pji_sample.rds")
pji_sample <- file.choose()
pji_sample<- readRDS(pji_sample)

test_pji_sample <- pji_sample %>% 
  mutate(year = year(kdob)) %>% 
  group_by(pidp) %>% 
  mutate(time = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(pidp, year, time) %>% 
  mutate(wave = "wave") %>% 
  unite(waves, wave, time, remove = TRUE) %>% 
  pivot_wider(names_from = waves, values_from = year) %>% 
  mutate(fb = ifelse(wave_1 > 0, 1, 0))

count_pji_sample <- test_pji_sample %>% 
  filter(wave_1 >= 2008 | is.na(wave_1)) %>% 
  count(wave_1)

count_pji_sample2 <- test_pji_sample %>% 
  filter(wave_1 >= 2008 | is.na(wave_1)) %>% 
  count(wave_1)

############################################################################
# Creation of panel data for PJI ------------------------------------------
###########################################################################

# Attempt to reorganize data
# Delete if not needed
# panel_pji <- function(emp_his) with(emp_his, data.frame(pidp, wave, spell2,
#                                                         status, int_date,
#                                                         job_hours, job_change, start_flag,
#                                                         end_flag, end_und, status_spells, employed,
#                                     date = seq(start_date, end_date, by = "month")))
# 
# NSA2 <- do.call("rbind", by(NSA1, 1:nrow(NSA1), panel_pji))


# Attempt to reorganize data
# Delete if not needed
# 
# f <- function(x) with(x, data.frame(pidp, employed, date = seq(from, to, by = "day")))
# do.call("rbind", by(data, 1:nrow(data), f))

#Functioning data reorganize
pji1 <- transform(pji_sample, from = as.Date(start_date), to = as.Date(end_date), lagfb = as.Date(lagged_kdob))

pji2 <- pji1 %>%
  dplyr::select(pidp, unemp, to, from, lagfb, age49, sex, hhorig, dob, kdob)

dt <- data.table(pji2)
pji3 <- dt[, list(pidp, unemp, lagfb, age49, sex, hhorig, dob, kdob, date = seq(from, to, by = "month")), by = 1:nrow(dt)] %>% 
  mutate(age_start = (dob %--% date)/dyears(1)) 

test_pji3 <- pji3 %>% 
  mutate(fbyear = year(lagfb)) %>% 
  group_by(pidp) %>% 
  mutate(time = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(pidp, fbyear, time) %>% 
  mutate(wave = "wave") %>% 
  unite(waves, wave, time, remove = TRUE) %>% 
  pivot_wider(names_from = waves, values_from = fbyear) %>% 
  mutate(fb = ifelse(wave_1 > 0, 1, 0))

count_pji3 <- test_pji3 %>% 
  filter(wave_1 >= 2008 | is.na(wave_1)) %>% 
  count(wave_1)

#This data set is to discover why there are fewer first births after filtering the data set
#The answer is that someone people first birth comes before first employment observation
fert_check_pji3 <- pji3 %>% 
  group_by(pidp) %>% 
  mutate(spell = row_number())%>% 
  filter(spell == 1, !is.na(kdob)) %>% 
  mutate(fbcheck = ifelse(date > kdob, 0, 1)) %>% 
  mutate(fbyear = year(kdob)) %>% 
  filter(fbyear >= 2008)

%>% 
  ungroup() 

fert_check_pji3 %>% 
  count(fbcheck = 1)




###Here is out problem with losing first borns
pji4 <- pji3 %>% 
  mutate(date = as.POSIXct(date)) %>% 
  mutate(age49 = as.POSIXct(age49)) %>% 
  mutate(lagfb = as.POSIXct(lagfb)) %>% 
  mutate(lagfb = as.POSIXct(dob)) %>% 
  mutate(lagfb = as.POSIXct(kdob)) %>% 
  mutate(diff_fb = date - kdob) %>% #test to see if the lagged dates cause the problem
  mutate(diff_lag = date - lagfb) %>%
  mutate(diff_age = date - age49) %>% 
  distinct(pidp, date, .keep_all = TRUE) %>% 
  filter(date <= kdob | is.na(kdob)) 

str(pji4)
  
  
  # filter(diff_fb <= 0| is.na(diff_fb)) %>% 
  filter(diff_fb2 <= 1| is.na(diff_fb2)) %>% 
  # mutate(fb_check = diff_fb == 0) %>% 
  filter(diff_age <= 0 | is.na(diff_age))

test_pji4 <- pji4 %>% 
  mutate(fbyear = year(lagfb)) %>% 
  group_by(pidp) %>% 
  mutate(time = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(pidp, fbyear, time) %>% 
  mutate(wave = "wave") %>% 
  unite(waves, wave, time, remove = TRUE) %>% 
  pivot_wider(names_from = waves, values_from = fbyear) %>% 
  mutate(fb = ifelse(wave_1 > 0, 1, 0))

pji4 %>% 
  mutate(fbcheck = ifelse(is.na(kdob), 0, 1)) %>% 
  group_by(pidp) %>% 
  count(fbcheck = 1)

count_pji4 <- test_pji4 %>% 
  filter(wave_1 >= 2008 | is.na(wave_1)) %>% 
  count(wave_1)

pji5 <- pji4 %>% 
  dplyr::select(-fb_check, -diff_fb, -diff_age)
  # mutate(year = year(date),
  #        month = month(date)) %>% 
  # group_by(pidp) %>% 
  # complete(year, month)

# Fixing missing periods --------------------------------------------------
#Work on 03.02 but not on 09.02!!!
# grouping_pji5 <- pji5 %>% 
#   group_by(pidp) %>% 
#   do(pad(.))
# 
# grouping_pji5 <- pji5 %>% 
#   pad(group = "pidp")

# saveRDS(grouping_dt2, file = "grouping_dt2.rds")
# test_grouping_dt2 <- file.choose()
# test_grouping_dt2<- readRDS(test_grouping_dt2)

###Start here
#This should be working!!! Why isn't it!!!!!!
# test_grouping_dt3 <- test_grouping_dt2 %>% 
#   fill(pidp, employed)

#gets rid of overlapping months between spells (same end and start month)
pji6 <- pji5 %>% 
  distinct(pidp, date, .keep_all = TRUE) 

pji6_2 <- pji6 %>% 
  dplyr::select(pidp, lagfb)

#Used to create LP (unemp) and IP (emp_ratio) vectors
panel_pji <- pji6 %>% 
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

str(panel_pji)

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


pji_complete <- read_dta("S:/R Files/Emp_Unc_Fertility_Birthlife/panel_pji_run.dta")


pji_dem <- pji2 %>% 
  dplyr::select(-unemp, -to, -from)

pji_var <- pji_complete %>% 
  rename( "pidp" = "id") %>% 
  left_join(., pji_dem, by = "pidp") %>% 
  distinct(pidp, yr, .keep_all = TRUE) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(fbyear = year(lagfb)) %>% 
  mutate(sex = recode(sex,
                      "-9"="Missing",
                      "1" = "Male",
                      "2" = "Female")) %>% 
  mutate(fbyes = ifelse(is.na(fbyear), 0, 1))

#This is a test to check for number of newborns
test_pji_var <- pji_var %>% 
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

pji_var %>% 
  dplyr::filter(is.na(fbyear) | fbyear > 2008) %>% 
  ggplot(aes(se_ee, fill = sex)) +
  geom_histogram(binwidth = 0.03) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Sex") +
  xlab("Persistent Joblessness Index: 1 = Continiously Jobless") + 
  ggtitle("Persistent Joblessness Index", subtitle =  "UKHLS = Measured 9 months before first birth") +
  ggsave("pji_hist_sex.png")

pji_var_cut %>% 
  mutate(fbyes = as.character(fbyes)) %>% 
  dplyr::filter(is.na(fbyear) | fbyear >= 2008) %>% 
  ggplot(aes(se_ee, fill = fbyes)) +
  geom_histogram(binwidth = 0.03) +
  scale_fill_brewer(palette = "Set1") +
  theme(aspect.ratio = 1) +
  labs(fill = "Sex") +
  xlab("Persistent Joblessness Index: 1 = Continiously Jobless") + 
  ggtitle("Persistent Joblessness Index", subtitle =  "UKHLS = Measured 9 months before first birth") +
  ggsave("pji_hist_sex.png")

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
