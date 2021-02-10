#Coded by: Brian Buh
#Started on: 27.01.2020
#Last Updated: 10.02.2020

a_indresp %>% 
  count(a_ppid)


###########################################################################
# Variables from the w_indresp files --------------------------------------
###########################################################################


#Sorting out needed variables from indresp
#Changes in these lists allow for much quick adding and subtracting variables
even_wave_var <- c("sex", "birthm", "birthy", "dvage", "qfhigh_dv", "racel_dv", "gor_dv", "finfut", "finnow", "hhorig", "ppid", "intdatm_dv", "intdaty_dv")

odd_wave_var <- c("sex", "birthm", "birthy", "dvage", "qfhigh_dv", "racel_dv", "gor_dv", "finfut", "finnow", "hhorig", "jbsec", "ppid", "intdatm_dv", "intdaty_dv")

#Add the wave prefix to the variable list
w1_even_var <- paste0('a_', even_wave_var)

w2_odd_var <- paste0('b_', odd_wave_var)

w3_even_var <- paste0('c_', even_wave_var)

w4_odd_var <- paste0('d_', odd_wave_var)

w5_even_var <- paste0('e_', even_wave_var)

w6_odd_var <- paste0('f_', odd_wave_var)

w7_even_var <- paste0('g_', even_wave_var)

w8_odd_var <- paste0('h_', odd_wave_var)

w9_even_var <- paste0('i_', even_wave_var)

w10_odd_var <- paste0('j_', odd_wave_var)

col_order <- c("pidp", "wave", "sex", "birthm", "birthy", "dvage", "qfhigh_dv", "racel_dv", "gor_dv", "finfut", "finnow", "jbsec")


#Preparing the variables for merging
a_ind <- a_indresp %>% 
  dplyr::select("pidp", w1_even_var) %>% 
  rename_with(~ even_wave_var[which(w1_even_var == .x)], .cols = w1_even_var) %>% 
  mutate(wave = 1) %>% 
  mutate(jbsec = NA)

b_ind <- b_indresp %>% 
  dplyr::select("pidp", w2_odd_var)%>% 
  rename_with(~ odd_wave_var[which(w2_odd_var == .x)], .cols = w2_odd_var) %>% 
  mutate(wave = 2)

c_ind <- c_indresp %>% 
  dplyr::select("pidp", w3_even_var) %>% 
  rename_with(~ even_wave_var[which(w3_even_var == .x)], .cols = w3_even_var) %>% 
  mutate(wave = 3) %>% 
  mutate(jbsec = NA)

d_ind <- d_indresp %>% 
  dplyr::select("pidp", w4_odd_var)%>% 
  rename_with(~ odd_wave_var[which(w4_odd_var == .x)], .cols = w4_odd_var) %>% 
  mutate(wave = 4)

e_ind <- e_indresp %>% 
  dplyr::select("pidp", w5_even_var) %>% 
  rename_with(~ even_wave_var[which(w5_even_var == .x)], .cols = w5_even_var) %>% 
  mutate(wave = 5) %>% 
  mutate(jbsec = NA)

f_ind <- f_indresp %>% 
  dplyr::select("pidp", w6_odd_var)%>% 
  rename_with(~ odd_wave_var[which(w6_odd_var == .x)], .cols = w6_odd_var) %>% 
  mutate(wave = 6)

g_ind <- g_indresp %>% 
  dplyr::select("pidp", w7_even_var) %>% 
  rename_with(~ even_wave_var[which(w7_even_var == .x)], .cols = w7_even_var) %>% 
  mutate(wave = 7) %>% 
  mutate(jbsec = NA)

h_ind <- h_indresp %>% 
  dplyr::select("pidp", w8_odd_var)%>% 
  rename_with(~ odd_wave_var[which(w8_odd_var == .x)], .cols = w8_odd_var) %>% 
  mutate(wave = 8)

i_ind <- i_indresp %>% 
  dplyr::select("pidp", w9_even_var) %>% 
  rename_with(~ even_wave_var[which(w9_even_var == .x)], .cols = w9_even_var) %>% 
  mutate(wave = 9) %>% 
  mutate(jbsec = NA)

j_ind <- j_indresp %>% 
  dplyr::select("pidp", w10_odd_var)%>% 
  rename_with(~ odd_wave_var[which(w10_odd_var == .x)], .cols = w10_odd_var) %>% 
  mutate(wave = 10)

ind_sample <-
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

str(ind_sample)  


###########################################################################
# Variables from the xwavedat file ----------------------------------------
###########################################################################

x_sample <- xwave %>% 
  dplyr::select(pidp, hhorig, generation, lwenum_dv, fwenum_dv, fwintvd_dv, lwintvd_dv ) %>% 
  filter(hhorig <= 2 | hhorig == 7) %>% 
  dplyr::select(-hhorig)
  
all_sample <- 
  left_join(ind_sample,x_sample, by = "pidp")


#Save and load the combined individual data file as an RDS
saveRDS(all_sample, file = "all_sample.rds")
all_sample <- file.choose()
all_sample <- readRDS(all_sample)


###########################################################################
# Test statistics of the sample -------------------------------------------
###########################################################################

xtabs(~fwenum_dv + fwintvd_dv, x_sample)

test <- all_sample %>% 
  arrange(pidp, wave) %>% 
  filter(dvage <= 55, sex == 2)

test %>%  
  group_by(pidp) %>% 
  summarize(unique_obs = n_distinct("pidp"))

test %>% 
  mutate(hhorig = as.factor(hhorig)) %>% 
  group_by(pidp) %>% 
  summarise(unique_obs = n_distinct("hhorig"))

aggregate(hhorig ~ pidp, test, function(x) length(unique(x)))

test %>% 
  group_by(hhorig) %>% 
  summarize_if(pidp)

test2 <- test %>% 
  select(pidp, hhorig, wave) %>% 
  pivot_wider(names_from =wave, values_from = hhorig) %>% 
  mutate(sum = rowSums(.[2:11], na.rm = TRUE), #This creates a sum of all the years with positive or negative outlook
         mean = rowMeans(.[2:11], na.rm = TRUE))

test2 %>% 
  count(mean)


ggplot(unique(test), aes(x = as.factor(hhorig)))+
  stat_count()+
  labs(x='sample group', y= 'count of sample')


###########################################################################
# Creating panel data -----------------------------------------------------
###########################################################################

panel_all_sample <- all_sample %>% 
  complete(pidp, wave) %>% 
  left_join(., first_born, by = "pidp") %>% 
  relocate("kdob", .after = "wave") %>% 
  dplyr:: select(-sex.y) %>% 
  rename("sex" = "sex.x") %>% 
  group_by(pidp) %>% 
  fill(sex, .direction = "down") %>% 
  fill(sex, .direction = "up") %>% 
  fill(qfhigh_dv, .direction = "down") %>% 
  ungroup() %>% 
  mutate(edu_cat = case_when(
    qfhigh_dv <= 6 ~ "high",
    qfhigh_dv <= 12 & qfhigh_dv >=7 ~ "medium",
    qfhigh_dv >=13 & qfhigh_dv <= 15 ~ "low",
    qfhigh_dv >= 16 & qfhigh_dv <= 0 ~ "NA")) 

str(panel_all_sample)

%>% 
  relocate("edu_cat", .after = "qfhigh_dv")

%>% 
  mutate(edu_cat = fct_relevel(edu_cat, c("high", "medium", "low"))) # this is in order to keep graphs in the correct high, medium, low, unknown order

test_impute %>% 
  ggplot(aes(se_ee)) +
  geom_histogram(binwidth = 0.05)

