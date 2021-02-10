#Coded by: Brian Buh
#Started on: 20.01.2020
#Last Updated: 10.02.2021

library(tidyverse)
library(haven)
library(lubridate)

###########################################################################
# Collecting retrospective Fertility Histories ----------------------------
###########################################################################
# This has been saved as an RDS


#This was updates 09.02.2021 to reflect that people enter waves later
# a_sample <- a_indall %>% 
#   dplyr::select(pidp, a_hhorig, a_sex, a_doby_dv, a_dobm_dv) %>% 
#   rename("mpidp" = "pidp") %>% 
#   rename("mdoby" = "a_doby_dv") %>% 
#   rename("mdobm" = "a_dobm_dv") %>% 
#   unite(mdob, c(mdobm, mdoby), sep = "-") %>% 
#   mutate(mdob = parse_date_time(mdob, "my"))

#Using xwave data to catch all possible respondents
x_sample <- xwave %>% 
  dplyr::select(pidp, hhorig, sex, birthm, birthy) %>% 
  rename("mpidp" = "pidp") %>% 
  rename("doby" = "birthy") %>% 
  rename("dobm" = "birthm") %>% 
  unite(dob, c(dobm, doby), sep = "-") %>% 
  mutate(dob = parse_date_time(dob, "my"))

#The DF a_parent comes from the dataset Stata code from Alita Nandi
parent_sample <- a_parent %>% 
  filter(a_mnpid >0) %>% 
  rename("kpidp" = "pidp") %>% 
  rename("mpidp" = "a_mnpid") %>%
  rename("kbirthm" = "a_birthm") %>% 
  rename("kbirthy" = "a_birthy")


res_child <- a_natchild %>% 
  dplyr::select(pidp, a_lchlv, a_lchdoby, a_lchdobm, a_childno) %>%
  filter(a_lchlv == 2) %>% 
  rename("mpidp" = "pidp") %>% 
  rename("kbirthm" = "a_lchdobm") %>% 
  rename("kbirthy" = "a_lchdoby") %>% 
  mutate(kbirthm = ifelse(kbirthm < 0, NA, kbirthm)) %>% 
  mutate(kbirthy = ifelse(kbirthy < 0, NA, kbirthy)) %>% 
  dplyr::select(-a_lchlv)

remove(combined_child)

combined_child <-
  bind_rows(res_child, parent_sample) %>% 
  dplyr::select(mpidp, kbirthy, kbirthm, a_childno) %>% 
  mutate(wave = 1)

###########################################################################
# Building prospective fertility histories --------------------------------

b_newchild <- b_child %>% 
  filter(b_ynew == 1) %>% 
  dplyr::select(pidp, b_ynew, b_birthy, b_birthm, b_mnpid) %>% 
  rename("ynew" =  "b_ynew") %>% 
  rename("kbirthy" =  "b_birthy") %>% 
  rename("kbirthm" =  "b_birthm") %>% 
  rename("mpidp" =  "b_mnpid") %>% 
  mutate(wave = 2)

# b_newchild_test <- b_child %>% 
#   filter(b_ynew == 1) %>% 
#   dplyr::select(pidp, b_ynew, b_birthy, b_doby_dv)
# remove(b_newchild_test)

c_newchild <- c_child %>% 
  filter(c_ynew == 1) %>% 
  dplyr::select(pidp, c_ynew, c_birthy, c_birthm, c_mnpid) %>% 
  rename("ynew" =  "c_ynew") %>% 
  rename("kbirthy" =  "c_birthy") %>% 
  rename("kbirthm" =  "c_birthm") %>% 
  rename("mpidp" =  "c_mnpid") %>% 
  mutate(wave = 3)

d_newchild <- d_child %>% 
  filter(d_ynew == 1) %>% 
  dplyr::select(pidp, d_ynew, d_birthy, d_birthm, d_mnpid) %>% 
  rename("ynew" =  "d_ynew") %>% 
  rename("kbirthy" =  "d_birthy") %>% 
  rename("kbirthm" =  "d_birthm") %>% 
  rename("mpidp" =  "d_mnpid") %>% 
  mutate(wave = 4)

e_newchild <- e_child %>% 
  filter(e_ynew == 1) %>% 
  dplyr::select(pidp, e_ynew, e_birthy, e_birthm, e_mnpid) %>% 
  rename("ynew" =  "e_ynew") %>% 
  rename("kbirthy" =  "e_birthy") %>% 
  rename("kbirthm" =  "e_birthm") %>% 
  rename("mpidp" =  "e_mnpid") %>% 
  mutate(wave = 5)

f_newchild <- f_child %>% 
  filter(f_ynew == 1) %>% 
  dplyr::select(pidp, f_ynew, f_birthy, f_birthm, f_mnpid) %>% 
  rename("ynew" =  "f_ynew") %>% 
  rename("kbirthy" =  "f_birthy") %>% 
  rename("kbirthm" =  "f_birthm") %>% 
  rename("mpidp" =  "f_mnpid") %>% 
  mutate(wave = 6)

g_newchild <- g_child %>% 
  filter(g_ynew == 1) %>% 
  dplyr::select(pidp, g_ynew, g_birthy, g_birthm, g_mnpid) %>% 
  rename("ynew" =  "g_ynew") %>% 
  rename("kbirthy" =  "g_birthy") %>% 
  rename("kbirthm" =  "g_birthm") %>% 
  rename("mpidp" =  "g_mnpid") %>% 
  mutate(wave = 7)

h_newchild <- h_child %>% 
  filter(h_ynew == 1) %>% 
  dplyr::select(pidp, h_ynew, h_birthy, h_birthm, h_mnpid) %>% 
  rename("ynew" =  "h_ynew") %>% 
  rename("kbirthy" =  "h_birthy") %>% 
  rename("kbirthm" =  "h_birthm") %>% 
  rename("mpidp" =  "h_mnpid") %>% 
  mutate(wave = 8)

i_newchild <- i_child %>% 
  filter(i_ynew == 1) %>% 
  dplyr::select(pidp, i_ynew, i_birthy, i_birthm, i_mnpid) %>% 
  rename("ynew" =  "i_ynew") %>% 
  rename("kbirthy" =  "i_birthy") %>% 
  rename("kbirthm" =  "i_birthm") %>% 
  rename("mpidp" =  "i_mnpid") %>% 
  mutate(wave = 9)

j_newchild <- j_child %>% 
  filter(j_ynew == 1) %>% 
  dplyr::select(pidp, j_ynew, j_birthy, j_birthm, j_mnpid) %>% 
  rename("ynew" =  "j_ynew") %>% 
  rename("kbirthy" =  "j_birthy") %>% 
  rename("kbirthm" =  "j_birthm") %>% 
  rename("mpidp" =  "j_mnpid") %>% 
  mutate(wave = 10)

child <- 
  bind_rows(b_newchild, c_newchild) %>% 
  bind_rows(., d_newchild) %>% 
  bind_rows(., e_newchild) %>% 
  bind_rows(., f_newchild) %>% 
  bind_rows(., g_newchild) %>% 
  bind_rows(., h_newchild) %>% 
  bind_rows(., i_newchild) %>% 
  bind_rows(., j_newchild) %>% 
  dplyr::select(-ynew, -pidp)


# Combined fertility histories --------------------------------------------

combined_child <- 
  bind_rows(combined_child, child) %>% 
  group_by(mpidp, kbirthy) 

remove(combined_child2)

#This was updates on 09.02.2021 to use the xwave data and leave fathers in the data set
combined_child2 <- 
  left_join(combined_child, x_sample, by = "mpidp") %>% 
  unite(kdob, c(kbirthm, kbirthy), sep = "-") %>% 
  mutate(kdob = parse_date_time(kdob, "my")) %>% 
  arrange(mpidp, kdob) %>% 
  group_by(mpidp) %>% 
  mutate(bno = row_number()) %>% 
  mutate(check = a_childno - bno) %>% 
  ungroup()

#Work to be done: This data set needs to include all things relevant to my work.
#Check to see all the data is correct and that there is no obvious errors or missing work

# <- <- %>% %>% %>% %>% 
# \%>% 
# pivot_wider(names_from = bno, values_from = c(bno, kdob))

saveRDS(combined_child2, file = "m_k_dob.rds")
m_k_dob <- file.choose()
m_k_dob <- readRDS(m_k_dob)

#########################################################################
# Descriptives of the data frame ------------------------------------------


xtabs(~employed + status, emp_his)


###########################################################################
# First born children -----------------------------------------------------
###########################################################################

first_born <- m_k_dob %>% 
  filter(bno == 1) %>% 
  rename("pidp" = "mpidp") %>% 
  ungroup() %>% 
  filter(check == 0 | is.na(check)) %>% 
  dplyr::select(pidp, kdob, sex)

first_born %>%
  mutate(year = year(kdob)) %>% 
  count(year > 2008, sex)



