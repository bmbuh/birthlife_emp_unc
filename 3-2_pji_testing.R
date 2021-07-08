#Coded by: Brian Buh
#Started on: 24.06.2021
#Last Updated: 

#This script was created to reexamine the PJI variable to see if there were any other factors determining who left when.
#The different variables were examined using histograms and age delimited bar charts. There are not significantly odd distributions.

library(data.table)
library(padr)
library(tidyverse)
library(haven)
library(lubridate)
library(arsenal)


###########################################################################
# PJI descriptive --------------------------------------------------------
###########################################################################

all_sample <- file.choose()
all_sample<- readRDS(all_sample)

surv5 <- file.choose()
surv5 <- readRDS(surv5)

pji_test <- surv5 %>% 
  arrange(pidp, desc(wave)) %>%
  group_by(pidp) %>% 
  mutate(revwave = row_number()) %>% 
  ungroup() %>% 
  filter(revwave == 1) %>% 
  dplyr::select(-racel_dv, -gor_dv, -generation, -ppid, -jbpl) %>% 
  mutate(fb = ifelse(event == 1, 1, 0)) %>% 
  mutate(fb = as.character(fb)) %>% 
  mutate(fb = recode(fb, "1" = "Birth",
                      "0" = "No Birth")) %>% 
  mutate(sex = as.character(sex)) %>% 
  mutate(sex = recode(sex, "1" = "Men",
                      "2" = "Women")) %>% 
  mutate(pji_cat = ifelse(se_ee == 0, 0, 1)) %>% 
  mutate(pji_cat = recode(pji_cat, "0" = "only employed",
                          "1" = "joblessness")) %>% 
  unite(pji_fb, c(fb, pji_cat), sep = "-") %>% #creates a variable for seeing the combo
  mutate(employed = as.character(employed))
  
  
#histogram of PJI of sample
pji_test %>% 
  ggplot(aes(se_ee, fill = fb)) +
  geom_histogram(binwidth = 0.05) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Sex", caption = "Not shown, 0 = continiously employed") +
  xlab("Persistent Joblessness Index: 1 = Continiously Jobless") + 
  ggtitle("Truncated Persistent Joblessness Index", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex)

###bar chart age
pji_test %>% 
  ggplot(aes(dvage, fill = pji_fb)) +
  geom_bar() +
  facet_wrap(~sex) +
  theme_minimal() + 
  theme(aspect.ratio = 1) +
  labs(x = "Age of PJI measurement",
       y = "Percentage of the age",
       title = "Number of observations",
       fill = "End of PJI at First Birth") +
  ggsave("pji_birth_joblessness_3-2_240621.png")


#filled for percentages
pji_test %>% 
  ggplot(aes(dvage, fill = pji_fb)) +
  geom_bar(position = "fill") +
  facet_wrap(~sex) +
  theme_minimal() + 
  theme(aspect.ratio = 1) +
  labs(x = "Age of PJI measurement",
       y = "Percentage of the age",
       title = "Age distribution of PJI",
       fill = "End of PJI at First Birth") +
  ggsave("pji_birth_joblessness_fill_3-2_240621.png")
  
###bar chart employed
pji_test %>% 
  ggplot(aes(dvage, fill = employed)) +
  geom_bar() +
  facet_wrap(~sex) +
  theme_minimal() + 
  theme(aspect.ratio = 1) +
  labs(x = "Age of PJI measurement",
       y = "Percentage of the age",
       title = "Number of observations",
       fill = "Employed Dummy") +
  ggsave("pji_employed_3-2_250621.png")


#filled for percentages
pji_test %>% 
  ggplot(aes(dvage, fill = employed)) +
  geom_bar(position = "fill") +
  facet_wrap(~sex) +
  theme_minimal() + 
  theme(aspect.ratio = 1) +
  labs(x = "Age of PJI measurement",
       y = "Percentage of the age",
       title = "Age distribution of PJI",
       fill = "Employed Dummy") +
  ggsave("pji_employed_fill_3-2_250621.png")
  
  
  
  
  
  
  
  




