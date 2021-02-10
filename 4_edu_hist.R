#Coded by: Brian Buh
#Started on: 26.01.2021
#Last Updated: 28.01.2021

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
  

