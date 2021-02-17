#Coded by: Brian Buh
#Started on: 17.02.2021
#Last Updated: 


#the first task is filling in missing interview dates from missing internal waves
#This is probably best accomplished by simply picking the month halfway between the existing waves
#This process will be less extensive after eliminating the rows before the first observed waves and after the last observed wave

com_panel <- file.choose()
com_panel <- readRDS(com_panel)

com_panel2 <- com_panel %>% 
  filter(fwtest >= 0, lwtest >= 0) %>% 
  unite(intdate, c(intdatm_dv, intdaty_dv), sep = "-") %>% 
  mutate(intdate = parse_date_time(intdate, "my")) %>% 
  unite(dob, c(birthm, birthy), sep = "-") %>% 
  mutate(dob = parse_date_time(dob, "my")) %>% 
  mutate(age = round((intdate - dob)/(365*24*60*60)))
