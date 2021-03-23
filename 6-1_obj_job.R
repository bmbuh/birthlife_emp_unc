#Coded by: Brian Buh
#Started on: 23.03.2021
#Last Updated: 


library(tidyverse)



# Objective Job Measures --------------------------------------------------

wave_var <- c("hhorig", "jbstat", "jbterm1", "jbterm2", "jbhrs", "jbot", "jbsect", "jbsectpub", "jbpl", "jbsat")


#Add the wave prefix to the variable list
w1_var <- paste0('a_', wave_var)

w2_var <- paste0('b_', wave_var)

w3_var <- paste0('c_', wave_var)

w4_var <- paste0('d_', wave_var)

w5_var <- paste0('e_', wave_var)

w6_var <- paste0('f_', wave_var)

w7_var <- paste0('g_', wave_var)

w8_var <- paste0('h_', wave_var)

w9_var <- paste0('i_', wave_var)

w10_var <- paste0('j_', wave_var)

col_order <- c("pidp", "hhorig", "jbstat", "jbterm1", "jbterm2", "jbhrs", "jbot", "jbsect", "jbsectpub", "jbpl", "jbsat")

#Preparing the variables for merging
a_ind <- a_indresp %>% 
  dplyr::select("pidp", w1_var) %>% 
  rename_with(~ wave_var[which(w1_var == .x)], .cols = w1_var) %>% 
  mutate(wave = 1)

b_ind <- b_indresp %>% 
  dplyr::select("pidp", w2_var)%>% 
  rename_with(~ wave_var[which(w2_var == .x)], .cols = w2_var) %>% 
  mutate(wave = 2)

c_ind <- c_indresp %>% 
  dplyr::select("pidp", w3_var) %>% 
  rename_with(~ wave_var[which(w3_var == .x)], .cols = w3_var) %>% 
  mutate(wave = 3)

d_ind <- d_indresp %>% 
  dplyr::select("pidp", w4_var)%>% 
  rename_with(~ wave_var[which(w4_var == .x)], .cols = w4_var) %>% 
  mutate(wave = 4)

e_ind <- e_indresp %>% 
  dplyr::select("pidp", w5_var) %>% 
  rename_with(~ wave_var[which(w5_var == .x)], .cols = w5_var) %>% 
  mutate(wave = 5)

f_ind <- f_indresp %>% 
  dplyr::select("pidp", w6_var)%>% 
  rename_with(~ wave_var[which(w6_var == .x)], .cols = w6_var) %>% 
  mutate(wave = 6)

g_ind <- g_indresp %>% 
  dplyr::select("pidp", w7_var) %>% 
  rename_with(~ wave_var[which(w7_var == .x)], .cols = w7_var) %>% 
  mutate(wave = 7)

h_ind <- h_indresp %>% 
  dplyr::select("pidp", w8_var)%>% 
  rename_with(~ wave_var[which(w8_var == .x)], .cols = w8_var) %>% 
  mutate(wave = 8)

i_ind <- i_indresp %>% 
  dplyr::select("pidp", w9_var) %>% 
  rename_with(~ wave_var[which(w9_var == .x)], .cols = w9_var) %>% 
  mutate(wave = 9)

j_ind <- j_indresp %>% 
  dplyr::select("pidp", w10_var)%>% 
  rename_with(~ wave_var[which(w10_var == .x)], .cols = w10_var) %>% 
  mutate(wave = 10)

obj_measure <-
  bind_rows(a_ind, b_ind) %>%
  bind_rows(., c_ind) %>%
  bind_rows(., d_ind) %>%
  bind_rows(., e_ind) %>%
  bind_rows(., f_ind) %>%
  bind_rows(., g_ind) %>%
  bind_rows(., h_ind) %>%
  bind_rows(., i_ind) %>%
  bind_rows(., j_ind) %>% 
  relocate("wave", .after = "pidp") %>%
  relocate("hhorig", .after = "wave") %>% 
  arrange(pidp, wave) %>% 
  filter(hhorig <= 2 | hhorig == 7)  #This filters out non-UKHLS Wave 1 respondents

str(obj_measure)  

saveRDS(obj_measure, file = "obj_measure.rds")
obj_measure <- file.choose()
obj_measure <- readRDS(obj_measure)

