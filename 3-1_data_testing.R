# Created by: Brian Buh
# Created on: 11.02.2021

#This is left over code from data testing to find the reason for falling first birth numbers

#Code fit to script: "3_emp_hist
#Code follows the creation of said data frame


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



pji4 %>% 
  mutate(fbcheck = ifelse(is.na(kdob), 0, 1)) %>% 
  group_by(pidp) %>% 
  count(fbcheck = 1)

count_pji4 <- test_pji4 %>% 
  filter(wave_1 >= 2008 | is.na(wave_1)) %>% 
  count(wave_1)



test_pji4 <- pji4 %>% 
  mutate(fbyear = year(lagfb)) %>% 
  group_by(pidp) %>% 
  mutate(time = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(pidp, fbyear, time) %>% 
  mutate(wave = "wave") %>% 
  unite(waves, wave, time, remove = TRUE) %>% 
  pivot_wider(names_from = waves, values_from = fbyear) %>% 
  mutate(fb = ifelse(wave_1 > 0, 1, 0)) %>% 
  dplyr::select(-diff_fb, -diff_age)

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



###Testing number of first born in pji final set


pji_dem <- pji2 %>% 
  dplyr::select(-unemp, -to, -from)

pji_var_panel <- panel_pji %>% 
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
test_pji_var_panel <- pji_var_panel %>% 
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
  count(wave_1, sex)
mutate

str(panel_pji)



