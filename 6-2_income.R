#Coded by: Brian Buh
#Started on: 04.10.2021
#Last Updated: 


library(tidyverse)

#Raw data needs to be loaded from script 1 - all indresp and hhresp files

###########################################################################
# Selecting variables -----------------------------------------------------
###########################################################################

#Sorting out needed variables from indresp
#Changes in these lists allow for much quick adding and subtracting variables
wave_var <- c( "hidp", "hhorig", "sex", "birthm", "birthy", "dvage", "fimnnet_dv")


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

col_order <- c("pidp", "wave", "hidp", "hhorig", "sex", "birthm", "birthy", "dvage", "fimnnet_dv")


#Preparing the variables for merging
a_inc <- a_indresp %>% 
  dplyr::select("pidp", w1_var) %>% 
  rename_with(~ wave_var[which(w1_var == .x)], .cols = w1_var) %>% 
  mutate(wave = 1)

b_inc <- b_indresp %>% 
  dplyr::select("pidp", w2_var)%>% 
  rename_with(~ wave_var[which(w2_var == .x)], .cols = w2_var) %>% 
  mutate(wave = 2)

c_inc <- c_indresp %>% 
  dplyr::select("pidp", w3_var) %>% 
  rename_with(~ wave_var[which(w3_var == .x)], .cols = w3_var) %>% 
  mutate(wave = 3)

d_inc <- d_indresp %>% 
  dplyr::select("pidp", w4_var)%>% 
  rename_with(~ wave_var[which(w4_var == .x)], .cols = w4_var) %>% 
  mutate(wave = 4)

e_inc <- e_indresp %>% 
  dplyr::select("pidp", w5_var) %>% 
  rename_with(~ wave_var[which(w5_var == .x)], .cols = w5_var) %>% 
  mutate(wave = 5)

f_inc <- f_indresp %>% 
  dplyr::select("pidp", w6_var)%>% 
  rename_with(~ wave_var[which(w6_var == .x)], .cols = w6_var) %>% 
  mutate(wave = 6)

g_inc <- g_indresp %>% 
  dplyr::select("pidp", w7_var) %>% 
  rename_with(~ wave_var[which(w7_var == .x)], .cols = w7_var) %>% 
  mutate(wave = 7)

h_inc <- h_indresp %>% 
  dplyr::select("pidp", w8_var)%>% 
  rename_with(~ wave_var[which(w8_var == .x)], .cols = w8_var) %>% 
  mutate(wave = 8)

i_inc <- i_indresp %>% 
  dplyr::select("pidp", w9_var) %>% 
  rename_with(~ wave_var[which(w9_var == .x)], .cols = w9_var) %>% 
  mutate(wave = 9)

j_inc <- j_indresp %>% 
  dplyr::select("pidp", w10_var)%>% 
  rename_with(~ wave_var[which(w10_var == .x)], .cols = w10_var) %>% 
  mutate(wave = 10)

inc_sample <-
  bind_rows(a_inc, b_inc) %>%
  bind_rows(., c_inc) %>%
  bind_rows(., d_inc) %>%
  bind_rows(., e_inc) %>%
  bind_rows(., f_inc) %>%
  bind_rows(., g_inc) %>%
  bind_rows(., h_inc) %>%
  bind_rows(., i_inc) %>%
  bind_rows(., j_inc) %>% 
  relocate("wave", .after = "pidp") %>%
  relocate("hhorig", .after = "wave") %>% 
  arrange(pidp, wave) %>% 
  filter(hhorig <= 2 | hhorig == 7)  #This filters out non-UKHLS Wave 1 respondents

str(inc_sample)  


# -------------------------------------------------------------------------
# Income variables --------------------------------------------------------
# -------------------------------------------------------------------------

test <- a_hhresp %>% 
  select(a_hidp, a_fihhmnnet4_dv)

#Sorting out needed variables from indresp
#Changes in these lists allow for much quick adding and subtracting variables
wave_var_inc <- c( "hidp", "fihhmnnet1_dv", "fihhmnnet3_dv", "fihhmnnet4_dv")


#Add the wave prefix to the variable list
w1_var_inc <- paste0('a_', wave_var_inc)

w2_var_inc <- paste0('b_', wave_var_inc)

w3_var_inc <- paste0('c_', wave_var_inc)

w4_var_inc <- paste0('d_', wave_var_inc)

w5_var_inc <- paste0('e_', wave_var_inc)

w6_var_inc <- paste0('f_', wave_var_inc)

w7_var_inc <- paste0('g_', wave_var_inc)

w8_var_inc <- paste0('h_', wave_var_inc)

w9_var_inc <- paste0('i_', wave_var_inc)

w10_var_inc <- paste0('j_', wave_var_inc)

col_order_inc <- c("hidp", "fihhmnnet1_dv", "fihhmnnet3_dv", "fihhmnnet4_dv")


#Preparing the variables for merging
a_hh <- a_hhresp %>% 
  dplyr::select(w1_var_inc) %>% 
  rename_with(~ wave_var_inc[which(w1_var_inc == .x)], .cols = w1_var_inc) %>% 
  mutate(wave = 1)

b_hh <- b_hhresp %>% 
  dplyr::select(w2_var_inc)%>% 
  rename_with(~ wave_var_inc[which(w2_var_inc == .x)], .cols = w2_var_inc) %>% 
  mutate(wave = 2)

c_hh <- c_hhresp %>% 
  dplyr::select(w3_var_inc) %>% 
  rename_with(~ wave_var_inc[which(w3_var_inc == .x)], .cols = w3_var_inc) %>% 
  mutate(wave = 3)

d_hh <- d_hhresp %>% 
  dplyr::select(w4_var_inc)%>% 
  rename_with(~ wave_var_inc[which(w4_var_inc == .x)], .cols = w4_var_inc) %>% 
  mutate(wave = 4)

e_hh <- e_hhresp %>% 
  dplyr::select(w5_var_inc) %>% 
  rename_with(~ wave_var_inc[which(w5_var_inc == .x)], .cols = w5_var_inc) %>% 
  mutate(wave = 5)

f_hh <- f_hhresp %>% 
  dplyr::select(w6_var_inc)%>% 
  rename_with(~ wave_var_inc[which(w6_var_inc == .x)], .cols = w6_var_inc) %>% 
  mutate(wave = 6)

g_hh <- g_hhresp %>% 
  dplyr::select(w7_var_inc) %>% 
  rename_with(~ wave_var_inc[which(w7_var_inc == .x)], .cols = w7_var_inc) %>% 
  mutate(wave = 7)

h_hh <- h_hhresp %>% 
  dplyr::select(w8_var_inc)%>% 
  rename_with(~ wave_var_inc[which(w8_var_inc == .x)], .cols = w8_var_inc) %>% 
  mutate(wave = 8)

i_hh <- i_hhresp %>% 
  dplyr::select(w9_var_inc) %>% 
  rename_with(~ wave_var_inc[which(w9_var_inc == .x)], .cols = w9_var_inc) %>% 
  mutate(wave = 9)

j_hh <- j_hhresp %>% 
  dplyr::select(w10_var_inc)%>% 
  rename_with(~ wave_var_inc[which(w10_var_inc == .x)], .cols = w10_var_inc) %>% 
  mutate(wave = 10)

hh_sample <-
  bind_rows(a_hh, b_hh) %>%
  bind_rows(., c_hh) %>%
  bind_rows(., d_hh) %>%
  bind_rows(., e_hh) %>%
  bind_rows(., f_hh) %>%
  bind_rows(., g_hh) %>%
  bind_rows(., h_hh) %>%
  bind_rows(., i_hh) %>%
  bind_rows(., j_hh) %>% 
  relocate("wave", .after = "hidp") %>%
  arrange(hidp, wave) 
  # %>% 
  # filter(hhorig <= 2 | hhorig == 7)  #This filters out non-UKHLS Wave 1 respondents

str(hh_sample)  

# -------------------------------------------------------------------------
# Combined  ---------------------------------------------------------------
# -------------------------------------------------------------------------


hh_inc <- 
  left_join(inc_sample, hh_sample, by = c("hidp", "wave")) %>% 
  mutate(test = ifelse(is.na(fihhmnnet4_dv), 1, 0))

saveRDS(hh_inc, "hhinc.rds")





