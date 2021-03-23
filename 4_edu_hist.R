#Coded by: Brian Buh
#Started on: 26.01.2021
#Last Updated: 23.03.2021

#This script takes the UKHLS education histories as complied by Liam Wright
#and transforms them in a way that helps create variables for our project

remove(edu_his_edit)

edu_his_edit <- edu_his %>% 
  dplyr::select(1:19) %>%
  mutate(Wave = Wave-18) %>%
  mutate(employed = ifelse(Status == 1 , 1, ifelse(Status == 2, 1, ifelse(Status == 100, 1, 0)))) %>%
  group_by(pidp, Wave) %>%
  mutate(Wave = as.integer(Wave))


#rename columns to make them R friendly

oldnames = c("pidp", "Wave", "Spell", "Status", "start_date", "start_m", "start_y",
             "end_date", "end_m", "end_y", "int_date", "int_m", "int_y",              
             "Job_Hours", "Job_Change", "Start_Flag", "End_Flag",
             "End_Ind", "Status_Spells", "employed")
newnames = c("pidp", "wave", "spell", "status", "start_date", "start_m", "start_y",
             "end_date", "end_m", "end_y", "int_date", "int_m", "int_y",
             "job_hours", "job_change", "start_flag", "end_flag",
             "end_und", "status_spells", "employed")

edu_his_edit <- edu_his_edit %>%
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>% 
  dplyr::select(-start_m, -start_y, -end_m, -end_y, -int_y, -int_m) %>% 
  arrange(pidp, start_date)




#append the education and the employment history data
####Important
#This is ultimately unnecessary. The prospective education spells are included 
#in the employment status file

emp_edu_his <- 
  bind_rows(emp_his, edu_his_edit) %>%
  arrange(pidp, start_date) %>% 
  group_by(pidp, start_date) %>% 
  mutate(spell3 = row_number()) %>% 
  relocate("spell3", .after = "spell2")
  
#Using the dataset "left_edu" to create the time from
#end of formal education to first interview in UKHLS

left_edu2 <- left_edu %>% 
  dplyr::select(pidp, end_edu_m, end_edu_yr) %>% 
  unite(end_edu, c(end_edu_m, end_edu_yr), sep = "-") %>% #creates a date of end of edu
  mutate(end_edu = parse_date_time(end_edu, "my"))

saveRDS(left_edu2, file = "end_edu.rds")
end_edu <- file.choose()
end_edu <- readRDS(end_edu)

###Test 1
#Non-chronological test to see if all end of education dates are in the set
#There are missing dates - see fill 9 for continuation
test_cover <- 
  left_join(surv, end_edu, by = "pidp")

###Test 2
# Test finished education (from discrete time script)
edu_his_cut <- edu_his %>% 
  dplyr::select(pidp, start_date, end_date) %>% 
  group_by(pidp) %>% 
  mutate(spellnum = row_number()) %>% 
  arrange(pidp, desc(spellnum)) %>% 
  mutate(resnum = row_number()) %>% 
  filter(resnum == 1) %>% 
  dplyr::select(pidp, end_date) #This data set uses the term "end_date" as final choice

surv_edu <- surv %>% 
  left_join(., edu_his_cut, by = "pidp") %>% 
  left_join(., end_edu, by = "pidp") %>% 
  mutate(gap = as.duration(end_date %--% startdate) / dmonths(1))

###Test 3
#Test two, extracting from the annual/life history file
emp_his_com2 <- emp_his_com %>% 
  filter(Status == 7) %>% 
  group_by(pidp) %>% 
  mutate(spellnum = row_number()) %>% 
  dplyr::select(pidp, Wave, Spell, Status, start_date, end_date, spellnum)

emp_his_com3 <- emp_his_com2 %>% 
  filter(spellnum == 1) %>% 
  rename("end_edu_la" = "end_date") %>% 
  dplyr::select(pidp, end_edu_la)

surv_edu2 <- surv_edu %>% 
  left_join(., emp_his_com3, by = "pidp")
#It is cleart that there are time mismatches
#Since I consider returning to education a jobless period rather than the employed periods
#during breaks from education as not important, I assume the end of formal education starts at the end of the education 
#from childhood and spells that include returning to education are jobless

#Furthermore, when comparing the three above tests, it is clear that missing data for the most  part is covered by either Test 1 or Test 3
#Test 2 violates my assumption from the above text.
#Therefore, I need to combine the data from Test 1 and 3 to find the end of formal education

surv_edu3 <- surv_edu2 %>% 
  dplyr::select(pidp, wave, dob, startdate, enddate, time2, time1, event, end_edu, end_edu_la) %>% 
  mutate(end_edu_la = as.Date(end_edu_la)) %>% 
  mutate(gap = as.duration(end_edu %--% end_edu_la) / dmonths(1)) %>% 
  mutate(end_edu_la = ifelse(!is.na(gap), NA, end_edu_la)) %>% 
  mutate(end_edu_la = as.Date(end_edu_la, origin = "1970-01-01")) %>% 
  mutate(end_edu = as.Date(end_edu)) %>% 
  mutate(end_edu_com = coalesce(end_edu, end_edu_la)) %>%  #this combines the two columns into 1
  mutate(gapt1 = as.duration(end_edu_com %--% startdate) / dmonths(1)) %>% 
  mutate(gapt1 = round(gapt1, digits = 0)) %>% 
  mutate(gapt2 = as.duration(end_edu_com %--% enddate) / dmonths(1)) %>% 
  mutate(gapt2 = round(gapt2, digits = 0)) %>% 
  mutate(t1 = time1 + gapt1) %>% 
  mutate(t2 = time2 + gapt2) %>% 
  mutate(t1 = ifelse(is.na(t1), time1, t1)) %>% 
  mutate(t2 = ifelse(is.na(t2), time2, t2))

str(surv_edu3)

edu_adj <- surv_edu3 %>% 
  dplyr::select(pidp, t1, t2)

saveRDS(edu_adj, file = "edu_adj.rds")
edu_adj <- file.choose()
edu_adj <- readRDS(edu_adj)

summary(edu_adj$t2)

#Non-chronological model test to make sure a model can converge with this data
testmultglm <- glmer(formula = event ~time2 + (1|pidp),
                     family = binomial(cloglog),
                     data = surv_edu3,
                     control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl = list(maxfun = 2e5)))
summary(testmultglm)

kmtest <- survfit(Surv(t1, t2, event) ~ 1, data = surv_edu3, cluster = pidp)
summary(kmtest)
plot(kmtest)

#Using this I see that the most cases have the same date for both variables, but a few have a discrepancy
#Of the discrepancies, the amount of months off is relatively small
#It seems to be that "end_edu" is more reliable than end_edu_la
#This is based on the logical age of finishing education versus the date give in the life/annual history

  
  
  
  
  
  
  
