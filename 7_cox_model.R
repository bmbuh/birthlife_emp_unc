#Coded by: Brian Buh
#Started on: 17.02.2021
#Last Updated: 22.02.2021

# install.packages("zoo")

library(data.table)
library(padr)
library(tidyverse)
library(haven)
library(lubridate)
library(arsenal)
library(zoo)

#the first task is filling in missing interview dates from missing internal waves
#This is probably best accomplished by simply picking the month halfway between the existing waves
#This process will be less extensive after eliminating the rows before the first observed waves and after the last observed wave

com_panel <- file.choose()
com_panel <- readRDS(com_panel)

enddate <- com_panel %>% 
  unite(intdate, c(intdatm_dv, intdaty_dv), sep = "-") %>% #combines the mnth & yr var to make int dates
  mutate(intdate = parse_date_time(intdate, "my")) %>% 
  dplyr::select(pidp, wave, intdate)

enddate2 <- enddate %>% 
  mutate(wave = wave - 1) %>% 
  filter(wave != 0) %>% 
  rename("enddate" = "intdate")

enddate3 <- 
  left_join(enddate, enddate2, by = c("pidp", "wave")) %>% 
  rename("startdate" = "intdate")

com_panel2 <- com_panel %>% 
  left_join(., enddate3, by = c("pidp", "wave")) %>% 
  filter(fwtest >= 0, lwtest >= 0)%>% #removes rows before the first int or after the last
  # unite(intdate, c(intdatm_dv, intdaty_dv), sep = "-") %>% #combines the mnth & yr var to make int dates
  # mutate(intdate = parse_date_time(intdate, "my")) %>% 
  # unite(dob, c(birthm, birthy), sep = "-") %>% #creates a dob
  # mutate(dob = parse_date_time(dob, "my")) %>% 
  # mutate(age = round((intdate - dob)/(365*24*60*60))) %>% 
  group_by(pidp) %>% 
  fill(fb_check) %>%
  fill(dvage) %>% 
  fill(gor_dv) %>% 
  ungroup() %>% 
  filter(fb_check == 0 | fb_check == 2) %>% #this variable takes the observed "anychild" and subtracts the binary "kdob oberseved" 1 = had child but no kdob or not had child but observed kdob
  filter(dvage <= 49) %>% 
  dplyr::select(pidp, wave, kdob, sex, dvage, racel_dv, gor_dv, ppid, jbsec, generation,
                edu_cat, se_ee, finnow.imp, finfut.imp, startdate, enddate)
  
com_panel2 %>% 
  ungroup() %>% 
  count(fb_check)

foo <- function(x) coredata(na.approx(zoo(x), na.rm = FALSE))
DT <- data.table(com_panel2)
DT[, startdate := foo(startdate), by = "pidp"]
DT[, enddate := foo(enddate), by = "pidp"]

com_panel3 <- com_panel2 %>% 
  group_by(pidp) %>% 
  do(pad(.))
  





intdate <- com_panel2 %>% 
  dplyr::select(pidp, wave, intdate) %>% 
  mutate(int = "int") %>% 
  unite(int, int, wave, sep = "") %>% 
  pivot_wider(names_from = int, values_from = intdate) 

intdate2 <- intdate %>% 
 mutate(gap1 = interval(int1, int2) %/% months(1))


com_panel2 %>% 
  count(gor_dv)

idlong <- com_panel2 %>% 
  dplyr::select(pidp, wave, intdate) %>% 
  group_by(pidp) %>% 
  summarise("stdate" = min(as.Date(intdate)),
            "eddate" = max(as.character(intdate)))
                
idlong2 <- idlong %>%
  mutate(intdate = as.POSIXct(intdate)) %>% 
  thicken()
  
  group_by(pidp) %>% 
  fill_by_function(fun = median)
  
  
  
  do(pad(.))



%>% 
  fill_by_prevalent()
