#Coded by: Brian Buh
#Started on: 03.03.2021
#Last Updated: 04.03.2021

library(tidyverse)
library(haven)

surv <- file.choose()
surv <- readRDS(surv)


#Graph to look at the age of first birth conception
agetest <- surv %>% 
  dplyr::select(sex, agemn, event) %>% 
  mutate(age = agemn/12) %>% 
  filter(event == 1) %>% 
  arrange(age) %>% 
  group_by(age) %>% 
  add_tally() %>% 
  mutate(sex = as.factor(sex))

agetest %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Wormen",)) %>% 
  ggplot(aes(x = age, y= n, group = sex, color = sex)) +
  geom_smooth() +
  # geom_label(group_by(age) %>% filter(x == max(x)), aes(label = sprintf('%0.2f', y)), hjust = -0.5) +
  ylim(0, 17) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(color = "Sex") +
  xlab("Age of conception of first child") + 
  ylab("Count") +
  ggtitle("Age of First birth", subtitle =  "UKHLS = Measured 9 months before first birth") +
  #facet_wrap(~sex) +
  ggsave("surv_age_fb.png")

###########################################################################
# PJI Histograms ----------------------------------------------------------
###########################################################################

#Histogram of PJI of sample (truncated)
surv %>% 
  mutate(cutsamp = time1 == 0) %>%
  mutate(fbyes = ifelse(is.na(kdob), 0, 1)) %>% 
  mutate(fbyes = recode(fbyes,
                        "0" = "No birth",
                        "1" = "First birth")) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                        "1" = "Men",
                        "2" = "Women")) %>% 
  dplyr::filter(cutsamp == TRUE, se_ee != 0) %>% 
  ggplot(aes(se_ee, fill = fbyes)) +
  geom_histogram(binwidth = 0.05) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Sex", caption = "Not shown, 0 = continuously employed") +
  xlab("Persistent Joblessness Index: 1 = Continuously Jobless") + 
  ggtitle("Truncated Persistent Joblessness Index", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) +
  ggsave("pji_hist_sex_final.png")

#Histogram of PJI of sample (NOT truncated)
surv %>% 
  mutate(cutsamp = time1 == 0) %>%
  mutate(fbyes = ifelse(is.na(kdob), 0, 1)) %>% 
  mutate(fbyes = recode(fbyes,
                        "0" = "No birth",
                        "1" = "First birth")) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  dplyr::filter(cutsamp == TRUE) %>% 
  ggplot(aes(se_ee, fill = fbyes)) +
  geom_histogram(binwidth = 0.03) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Sex") +
  xlab("Persistent Joblessness Index: 1 = Continuously Jobless") + 
  ggtitle("Persistent Joblessness Index", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) +
  ggsave("pji_hist_sex_full_final.png")


###########################################################################
# Subjective measures -----------------------------------------------------
###########################################################################

#finnow

surv %>% 
  ggplot(aes(wave, fill = finnow.imp)) +
  geom_bar()+
  theme(aspect.ratio = 1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) +
  labs(fill = "finnow") +
  # xlab("Persistent Joblessness Index: 1 = Continuously Jobless") + 
  ggtitle("Present Financial Situation", subtitle =  "Sample - waves 1 to 10") +
  # facet_wrap(~sex) +
  ggsave("finnow_surv.png")

surv %>% 
  ggplot(aes(wave, fill = finfut.imp)) +
  geom_bar()+
  theme(aspect.ratio = 1) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) +
  labs(fill = "finfut") +
  # xlab("Persistent Joblessness Index: 1 = Continuously Jobless") + 
  ggtitle("Future Financial Situation", subtitle =  "Sample - waves 1 to 10") +
  # facet_wrap(~sex) +
  ggsave("finfut_surv.png")

spsurv %>% 
  filter(wave == 2 | wave == 4 | wave == 6 | wave == 8 | wave == 10) %>% 
  mutate(jbsec = recode(jbsec,
                        "3 non-employed" = "Non-employed",
                        "1 likely" = "Likely",
                        "2 unlikely" = "Unlikely")) %>%
  mutate(jbsec = fct_relevel(jbsec, c( "Likely", "Unlikely", "Non-employed"))) %>% 
  ggplot(aes(wave, fill = jbsec)) +
  geom_bar()+
  theme(aspect.ratio = 1) +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) +
  labs(fill = "jbsec") +
  # xlab("Persistent Joblessness Index: 1 = Continuously Jobless") + 
  ggtitle("Future Financial Situation", subtitle =  "Sample - waves 1 to 10") +
  # facet_wrap(~sex) +
  ggsave("jbsec_surv.png")


# Descriitive stats -------------------------------------------------------

statsurv <- surv %>% 
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(fb = ifelse(is.na(kdob), 0, 1)) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women"))


mycontrols <- tableby.control(test = FALSE)
desstats <-arsenal::tableby(fb ~ sex + se_ee + finnow.imp + finfut.imp + jbsec + edu_cat + combo + time2, data = statsurv, control = mycontrols)
labels(desstats ) <-  c(sex = "Sex", se_ee = "PJI", finnow.imp = "Present Financial Outlook", finfut.imp = "Future Financial Outlook",
                        jbsec = "Job Security", edu_cat = "Educational Attainment", combo = "Partnership, Partner's Job Status")
summary(desstats )
write2word(desstats , "desstats .doc")


