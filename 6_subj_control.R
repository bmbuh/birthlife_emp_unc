#Coded by: Brian Buh
#Started on: 27.01.2021
#Last Updated: 24.02.2021

# install.packages("mice")

library(tidyverse)
library(mice)

a_indresp %>% 
  count(a_ppid)


###########################################################################
# Variables from the w_indresp files --------------------------------------
###########################################################################


#Sorting out needed variables from indresp
#Changes in these lists allow for much quick adding and subtracting variables
even_wave_var <- c("sex", "birthm", "birthy", "dvage", "qfhigh_dv", "hiqual_dv", "racel_dv", "gor_dv", "finfut", "finnow", "hhorig", "ppid", "intdatm_dv", "intdaty_dv", "marstat_dv", "jbstat", "jbisco88_cc")

odd_wave_var <- c("sex", "birthm", "birthy", "dvage", "qfhigh_dv", "hiqual_dv", "racel_dv", "gor_dv", "finfut", "finnow", "hhorig", "jbsec", "ppid", "intdatm_dv", "intdaty_dv", "marstat_dv", "jbstat", "jbisco88_cc")

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

col_order <- c("pidp", "wave", "sex", "birthm", "birthy", "dvage", "qfhigh_dv", "hiqual_dv", "racel_dv", "gor_dv", "finfut", "finnow", "jbsec", "ppid", "intdatm_dv", "intdaty_dv", "jbisco88_cc")


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
  dplyr::select("pidp", "f_qfhighoth", w6_odd_var)%>% 
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


#adding partnership/employment details
ind_sample2 <- ind_sample %>% 
  dplyr::select(ppid, wave) %>% 
  rename("pidp" = "ppid") %>% 
  filter(pidp >= 0) %>% 
  left_join(., ind_sample, by = c("pidp", "wave")) %>% 
  dplyr::select(pidp, wave, jbstat) %>% 
  mutate(parjbstat = ifelse(is.na(jbstat), "unknown", ifelse(jbstat == 2 | jbstat == 1, "employed", "non-employed" ))) %>% 
  # 1 = employed, 2 = non-employed, 3 = unknown
  rename("ppid" = "pidp") %>% 
  dplyr::select(ppid, wave, parjbstat)

ind_sample3 <- 
  left_join(ind_sample, ind_sample2, by = c("ppid", "wave")) %>% 
  mutate(parjbstat = ifelse(is.na(parjbstat), "unknown", parjbstat)) %>% 
  mutate(marstat = ifelse(marstat_dv == 1, "married", ifelse(marstat_dv == 2, "cohab", "single"))) %>% 
  unite(combo, marstat, parjbstat, sep = "-", remove = FALSE)

ind_sample3 %>% 
  count(combo)

###########################################################################
# Variables from the xwavedat file ----------------------------------------
###########################################################################

x_sample2 <- xwave %>% #not to be confused with x_sample located in scripts 2 and 3
  dplyr::select(pidp, hhorig, generation, ukborn, lwenum_dv, fwenum_dv, fwintvd_dv, lwintvd_dv, anychild_dv) %>% 
  filter(hhorig <= 2 | hhorig == 7) %>% 
  dplyr::select(-hhorig)


  
all_sample <- 
  left_join(ind_sample3,x_sample2, by = "pidp")
  


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

test3 <- all_sample %>% 
  group_by(pidp) %>% 
  mutate(wavenum = row_number()) %>% 
  filter(wavenum == 1, dvage <= 50) %>% 
  ungroup

test3 %>% 
  count(sex)
  


###########################################################################
# Creating panel data -----------------------------------------------------
###########################################################################

#ISCED97 variable added 20.07.2021
panel_all_sample <- all_sample %>% 
  complete(pidp, wave) %>% 
  left_join(., first_born, by = "pidp") %>% 
  relocate("kdob", .after = "wave") %>% 
  dplyr:: select(-sex.y) %>% 
  rename("sex" = "sex.x") %>% 
  dplyr:: select(-hhorig.y) %>% 
  rename("hhorig" = "hhorig.x") %>% 
  group_by(pidp) %>% 
  fill(sex, .direction = "down") %>% 
  fill(sex, .direction = "up") %>% 
  fill(qfhigh_dv, .direction = "down") %>% 
  fill(qfhigh_dv, .direction = "up") %>% 
  ungroup() %>% 
  mutate(edu_cat = case_when(
    qfhigh_dv <= 6 ~ "high",
    qfhigh_dv <= 12 & qfhigh_dv >=7 ~ "medium",
    qfhigh_dv >=13 & qfhigh_dv <= 15 ~ "low")) %>% 
  mutate(edu_cat = ifelse(is.na(edu_cat), "other", edu_cat)) %>% #other edu_qf for people at 16, 96 or missing
  mutate(hiqual_edit = ifelse(hiqual_dv == 1 | hiqual_dv == 2, 5.1, ifelse(hiqual_dv == 3, 3.1, ifelse(hiqual_dv == 4, 2.1, ifelse(hiqual_dv == 5, 2.1, NA))))) %>% 
  mutate(hiqual_edit = ifelse(hiqual_edit == 2.1, 2, ifelse(hiqual_edit == 3.1, 3, ifelse(hiqual_edit == 5.1, 5, NA)))) %>% 
  # mutate(edu = ifelse(qfhigh_dv == 96 | qfhigh_dv <= 0, hiqual_edit, qfhigh_dv)) %>% 
  mutate(isced97 = case_when(
    qfhigh_dv == 1 ~ "6",
    qfhigh_dv >= 2 & qfhigh_dv <= 4 | qfhigh_dv == 6 ~ "5",
    # qfhigh_dv == 6 ~ "5",
    qfhigh_dv == 5 ~ "4",
    qfhigh_dv >=7 & qfhigh_dv <= 12 ~ "3",
    qfhigh_dv >= 13 & qfhigh_dv <= 16 ~ "2",
    qfhigh_dv == 96 ~"96",
    qfhigh_dv == -8 ~"-8",
    qfhigh_dv == -9 ~"-9")) %>% 
  mutate(isced97 = ifelse(isced97 == 96 | isced97 <= 0, hiqual_edit, isced97)) %>% 
  mutate(immigrant = ifelse(ukborn == 5, 1, 0)) %>% 
  mutate(immedu = ifelse(f_qfhighoth >= 1 & f_qfhighoth <= 3, 6, ifelse(f_qfhighoth == 4, 5, ifelse(f_qfhighoth == 5 | f_qfhighoth == 6, 4, 
                                                                                                    ifelse(f_qfhighoth == 7 | f_qfhighoth == 8, 3, 
                                                                                                           ifelse(f_qfhighoth == 9, 2, ifelse(f_qfhighoth == 10, 1, NA)))))))
  mutate(isced97 = ifelse(is.na(isced97), immedu, isced97)) %>% 
  mutate(isced97 = ifelse(is.na(isced97), "Not available", isced97))
  

# qfhigh_dv >= 16 & qfhigh_dv <= 0 ~ "unknown"

# str(panel_all_sample)
# 
# panel_all_sample %>% 
#   count(finnow)
# 
# #Test for seeing if there is a loss of first borns
# test_panel_all_sample <- panel_all_sample %>% 
#   mutate(year = year(kdob)) %>% 
#   dplyr::select(pidp, year, wave) %>% 
#   mutate(waves = "wave") %>% 
#   unite(names, waves, wave, remove = TRUE) %>% 
#   pivot_wider(names_from = names, values_from = year) %>% 
#   mutate(fb = ifelse(wave_1 > 0, 1, 0))
# 
# count_panel_all_sample <- test_panel_all_sample %>% 
#   filter(wave_1 >= 2008 | is.na(wave_1)) %>% 
#   count(wave_1)
# 
# %>% 
#   relocate("edu_cat", .after = "qfhigh_dv")
# 
# %>% 
#   mutate(edu_cat = fct_relevel(edu_cat, c("high", "medium", "low"))) # this is in order to keep graphs in the correct high, medium, low, unknown order
# 
# test_impute %>% 
#   ggplot(aes(se_ee)) +
#   geom_histogram(binwidth = 0.05)


###########################################################################
# Adding in the PJI -------------------------------------------------------
###########################################################################
#pji_complete comes from the script 3_emp_hist

pji_clean <- pji_complete %>% 
  rename("pidp" = "id") %>% 
  filter(!is.na(se)) %>% 
  select(pidp, se, ee, se_ee)

panel_all_sample_pji <- 
  left_join(panel_all_sample, pji_clean, by = "pidp") %>% 
  filter(!is.na(se)) %>% 
  mutate(fbyear = year(kdob)) %>% 
  filter(fbyear >= 2007 | is.na(fbyear)) %>% 
  group_by(pidp) %>% 
  fill(fwintvd_dv, .direction = "down") %>% #Fills in the NA from first interview wave
  fill(fwintvd_dv, .direction = "up") %>% 
  mutate(fwtest = wave - fwintvd_dv) %>% #Creates a variable where negative numbers symbolize waves before the respondents FW
  fill(lwintvd_dv, .direction = "down") %>% #Fills in the NA from the last interview
  fill(lwintvd_dv, .direction = "up") %>% 
  mutate(lwtest = lwintvd_dv - wave) #Creates a variable where negative numbers symbolize waves after the respondents LW
  
#Save and load the combined individual data file as an RDS
saveRDS(panel_all_sample_pji, file = "panel_all_sample_pji.rds")
panel_all_sample_pji <- file.choose()
panel_all_sample_pji <- readRDS(panel_all_sample_pji)

#Mutating "jbsec" to work as a variable
#Creating three categories: 1. Likely 2. Unlikely 3. Non-employed


panel_all_sample_pji2 <- panel_all_sample_pji %>% 
  mutate(jbsec = ifelse(jbsec == 2 | jbsec == 1, 1, ifelse(jbsec >=3, 2, 3))) %>% 
  mutate(jbsec = recode(jbsec,
                        "1" = "1 likely",
                        "2" = "2 unlikely",
                        "3" = "3 non-employed"))

panel_all_sample_pji2 %>% 
  ungroup() %>% 
  count(jbsec)  

####This was used to look at the situation of the jbsec in the sample
# panel_all_sample_pji%>% 
#   mutate(jbsec = as.factor(jbsec)) %>% 
#   filter(fwtest >= 0, lwtest >= 0) %>%
#   filter(wave == 2 | wave == 4 | wave == 6 | wave == 8 | wave == 10) %>% 
#   mutate(jbsec = recode(jbsec,
#                              "1" = "1 very likely",
#                              "2" = "2 likely",
#                              "3" = "3 unlikely",
#                              "4" = "4 very unlikely",
#                              "-1" = "-1 don't know",
#                              "-8" = "-8 inapplicable",
#                               "-7" = "NA",
#                               "-2" = "NA",
#                                 "-9" = "NA")) %>% 
#   mutate(jbsec = fct_relevel(jbsec, c( "1 very likely",
#                                                   "2 likely",
#                                                   "3 unlikely",
#                                                   "4 very unlikely",
#                                                   "-1 don't know",
#                                                   "-8 inapplicable"))) %>% 
#   # filter(edu_cat == "unknown") %>% 
#   # dplyr::filter(!is.na(w10)) %>% 
#   ggplot(aes(wave, fill = jbsec)) +
#   geom_bar()+
#   theme(aspect.ratio = 1)

###########################################################################
# Impute missing finnow ---------------------------------------------------
###########################################################################
xsex <- xwave %>% 
  dplyr::select(pidp, sex)

fb_check <- first_born %>% 
  mutate(fb = 1) %>% 
  dplyr::select(pidp, fb)

#START WITH FINNOW
#creates a wide format for the "finnow" variable
wide_finnow <- panel_all_sample_pji2 %>% 
  dplyr::select(pidp, wave, finnow) %>% 
  # mutate(finnow = as.factor(finnow)) %>% 
  mutate(wn = "w") %>% 
  mutate(finnow = ifelse(finnow <= -1, NA, finnow)) %>% 
  unite(wavenum, wn, wave, sep = "", remove = TRUE) %>% 
  pivot_wider(names_from = wavenum, values_from = finnow) %>% 
  mutate_if(is.numeric, as.factor) %>% 
  left_join(., xsex, by = "pidp") %>% 
  left_join(., fb_check, by = "pidp") %>% 
  mutate(fb = ifelse(is.na(fb), 0, fb))

test_wmw <- glm(formula = fb ~ w1 + w2 + w3 + w4 + w5 + w6 + w7 + w8 + w9 + w10, data = wide_finnow)
summary(test_wmw)
#Using the mice package to perform propotional odds imputation for ordered categorical var

#First, testing variables
md.pattern(wide_finnow, plot = FALSE) #looks at the pattern of missing values
flux(wide_finnow)[,1:3]
fluxplot(wide_finnow) #Creates a plot to look at influx and outflux coefficents

#First Imputation of finnow
imp <-  mice(wide_finnow, m = 5, method = ("polr"))
densityplot(imp)
fit <- with(imp, glm(sex ~ w1 + w2 + w3 + w4 + w5 + w6 + w7 + w8 + w9 + w10, family = binomial(link = "logit")))
summary(pool(fit))
#In order to pick the best imputation you need to see the results of the analytical model
#The following codes add in confidence intervals
summary(pool(fit), conf.int = TRUE, exponentiate = TRUE)

imp$predictorMatrix

#tidy alternative
# imp2 <- wide_finnow %>% 
#   mice(method = ("polr")) %>% 
#   mice::complete("all") %>% 
#   map(glm, formula = fb ~ w1 + w2 + w3 + w4 + w5 + w6 + w7 + w8 + w9 + w10, family = binomial(link = "logit")) %>% 
#   pool()


#After examining the analytical results, the best fit imputation set is number 4
imp_wide_finnow = complete(imp, 4)

imp_finnow <- imp_wide_finnow %>% 
  dplyr::select(-sex, -fb) %>% 
  pivot_longer(cols = c("w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8", "w9", "w10"), names_to = "wavename", values_to = "finnow.imp") %>%
  group_by(pidp) %>% 
  mutate(wave = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(-wavename) %>% 
  mutate(finnow.imp = recode(finnow.imp,
                             "1" = "1 Living comfortably",
                             "2" = "2 Doing alright",
                             "3" = "3 Just getting by",
                             "4" = "4 Finding it quite difficult",
                             "5" = "5 Finding it very difficult")) %>% 
  mutate(finnow.imp = fct_relevel(finnow.imp, c( "1 Living comfortably",
                                                 "2 Doing alright",
                                                 "3 Just getting by",
                                                 "4 Finding it quite difficult",
                                                 "5 Finding it very difficult")))

imp_finnow %>% 
  # dplyr::filter(!is.na(w10)) %>% 
  ggplot(aes(finnow.imp)) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1)

panel_all_sample_pji2 %>% 
  dplyr::filter(!is.na(finnow) | finnow > 0) %>% 
  ggplot(aes(finnow)) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1)

saveRDS(imp_finnow, file = "imp_finnow.rds")
imp_finnow <- file.choose()
imp_finnow <- readRDS(imp_finnow)

#SECOND FINFUT
#creates a wide format for the "finfut" variable
wide_finfut <- panel_all_sample_pji2 %>% 
  dplyr::select(pidp, wave, finfut) %>% 
  # mutate(finfut = as.factor(finfut)) %>% 
  mutate(wn = "w") %>% 
  mutate(finfut = ifelse(finfut <= -1, NA, finfut)) %>% 
  unite(wavenum, wn, wave, sep = "", remove = TRUE) %>% 
  pivot_wider(names_from = wavenum, values_from = finfut) %>% 
  mutate_if(is.numeric, as.factor)%>% 
  left_join(., xsex, by = "pidp") %>% 
  left_join(., fb_check, by = "pidp") %>% 
  mutate(fb = ifelse(is.na(fb), 0, fb))

fluxplot(wide_finfut) #Creates a plot to look at influx and outflux coefficients

imp_ff <-  mice(wide_finfut, m = 5, method = ("polr"))
fit_ff <- with(imp_ff, glm(fb ~ w1 + w2 + w3 + w4 + w5 + w6 + w7 + w8 + w9 + w10, family = binomial(link = "logit")))
summary(pool(fit_ff))
summary(imp_ff)
summary(pool(fit_ff), conf.int = TRUE, exponentiate = TRUE)
test_finfut = complete(imp_ff, 2)

#Testing to see changes in imputed data
wide_finfut %>% 
  # dplyr::filter(!is.na(w10)) %>% 
  ggplot(aes(w5)) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1)

test_finfut %>% 
  dplyr::filter(!is.na(w10)) %>% 
  ggplot(aes(w5)) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1)

#Plots that help look at different between probability to be missing versus imputed
#Diagnostic to help evalute if the imputation worked
ps <- rep(rowMeans(sapply(fit_ff$analyses, fitted.values)),
          imp_ff$m + 1)
xyplot(imp_ff, w5 ~ ps | as.factor(.imp),
       xlab = "Probability that record is incomplete",
       ylab = "w1", pch = c(1, 19), col = mdc(1:2))

imp_finfut <- test_finfut %>% 
  dplyr::select(-sex, -fb) %>% 
  pivot_longer(cols = c("w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8", "w9", "w10"), names_to = "wavename", values_to = "finfut.imp") %>%
  group_by(pidp) %>% 
  mutate(wave = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(-wavename) %>% 
  mutate(finfut.imp = recode(finfut.imp,
                      "1" = "Better off",
                      "2" = "Worse off",
                      "3" = "About the same"))

saveRDS(imp_finfut, file = "imp_finfut.rds")
imp_finfut <- file.choose()
imp_finfut <- readRDS(imp_finfut)


###########################################################################
# Adding imputed variables ------------------------------------------------
###########################################################################

paspji_imp <- panel_all_sample_pji2 %>% 
  left_join(., imp_finnow, by = c("pidp", "wave")) %>% 
  left_join(., imp_finfut, by = c("pidp", "wave")) %>% 
  mutate(fb = ifelse(is.na(kdob), 0, 1)) %>% 
  mutate(fb_check = anychild_dv - fb) %>% #There is a significant number of people who have had children, but I don't have their child's birthdate
                                          #They need to be removed or they will bias towards the number of non-events
  fill(birthm, .direction = "down") %>% #imputation of non-time varying var
  fill(birthm, .direction = "up") %>% 
  fill(birthy, .direction = "down") %>% #best to calculate age after transforming to monthly
  fill(birthy, .direction = "up") %>% 
  fill(marstat_dv, .direction = "down") %>% #best to calculate age after transforming to monthly
  fill(marstat_dv, .direction = "up") 

#Leftover test for dealing with jbsec
# paspji_imp %>% 
#   ungroup() %>% 
#   filter(wave == 2 | wave == 4 | wave == 6 | wave == 8 | wave == 10) %>% 
#   filter(fwtest >= 0, lwtest >= 0) %>% 
#   count(jbsec)

#Save and load the combined individual data file as an RDS
saveRDS(paspji_imp, file = "com_panel.rds")
com_panel <- file.choose()
com_panel <- readRDS(com_panel)


#shortcut for adding in partner details without rerunning imputation
#not necessary if reruning the entire code with imputations
# parjbstat <- all_sample %>% 
#   dplyr::select(pidp, wave, marstat, parjbstat, combo) 
# 
# com_panel <-com_panel %>% 
#   left_join(., parjbstat, by = c("pidp", "wave"))
