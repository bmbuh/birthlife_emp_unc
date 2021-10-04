#Coded by: Brian Buh
#Started on: 26.08.2021
#Last Updated: 30.09.2021

# install.packages("aod")

library(data.table)
library(tidyverse)
library(haven)
library(effects)
library(huxtable) #for the export_summs function
library(sandwich) #for the export_summs function, robust SE
library(officer) #for the export_summs function, export to word
library(flextable) #for the export_summs function, export to word
library(modelsummary)
library(jtools)
library(broom.mixed)
library(arsenal)
library(survival)
library(aod)
# library(plyr)

###########################################################################
# Loading DF --------------------------------------------------------------
###########################################################################

surv5 <- file.choose()
surv5 <- readRDS(surv5)

#PJI First 3 years post-education
pji_3yr <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/pji_busetta_mendola/panel_pji_3yr_run.dta")

pji_3yr_2 <- pji_3yr %>% 
  rename("pidp" = "id", "pji3" = "se_ee") %>% 
  select(pidp, pji3) %>% 
  filter(!is.na(pji3))

summary(pji_3yr_2$pji3)

hhinc <- file.choose()
hhinc <- readRDS(hhinc)

hhinc <- hhinc %>% 
  select(pidp, wave, fihhmnnet4_dv, fimnnet_dv)

#Histogram of PJI of sample (NOT truncated)
first3 %>% 
  ggplot(aes(pji3, fill = birth)) +
  geom_histogram(binwidth = 0.03) +
  # annotate("text", x=.15, y=5000, size = 6, label= "First Births are concentrated here") +
  annotate("text", x=.25, y=2000, size = 6, label= "0 = No Jobless Spells") +
  annotate("text", x=.8, y=1200, size = 6, label= "1 = Completely Jobless") +
  scale_fill_manual(values = c("#8FB339", "#3A2D32")) +
  theme_minimal()+
  theme(legend.position = c(.8,.8), plot.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust=-1), axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(fill = "Births") +
  ggtitle("Distribution of Persistent Joblessness Index for first 3 years post-Education") +
  # , subtitle =  "Combined duration, number and proximity of jobless spells - UKHLS Waves 1-10") +
  xlab("Persistent Joblessness Index") +
  ylab("Count") 

# +
#   ggsave("paper_pji_figure_26-08-21.png")

###########################################################################
# Create DF for Logisitic Regression --------------------------------------
###########################################################################

#DF for first 3 years after the end of education
first3 <- surv5 %>% 
  left_join(. , pji_3yr_2, by = "pidp") %>% 
  left_join(., hhinc, by = c("pidp", "wave")) %>% 
  filter(t1 < 36) %>% 
  # jbless creates a variable if the individual had any months of joblessness in the first 3 years
  mutate(jbless = ifelse(pji3 > 0, 1, 0)) %>%
  mutate(jbless2 = ifelse(pji3 == 0, "None", ifelse(pji3 > 0 & pji3 <=.5, "Some", "Much"))) %>%
  mutate(jbless2 = fct_relevel(jbless2, c("None", "Some", "Much"))) %>% 
  mutate(female = ifelse(sex == 2, 1, 0)) %>% 
  group_by(pidp) %>% 
  # the dependent variable a binary of a child in the first 3 years post education
  mutate(birthcheck = ifelse(event == 1, 1, 0)) %>% 
  add_tally(birthcheck, name = "birth_count") %>% 
  mutate(birth = ifelse(birth_count > 0, 1, 0)) %>% 
  # creates the variable "presbad" which is a dummy if they had any waves in the first 3 years where they found their pres. fin. sit. difficult
  mutate(finnowbad = ifelse(finnow3cat == "finddifficult", 1, 0)) %>% 
  add_tally(finnowbad, name = "finnowbad_count") %>% 
  mutate(presbad = ifelse(finnowbad_count > 0, 1, 0)) %>% 
  # creates the variable "futbad" which is a dummy if they had any waves in the first 3 years where they perceived their fut. fin. sit. as worse off
  mutate(finfutbad = ifelse(finfut.imp == "Worse off", 1, 0)) %>% 
  add_tally(finfutbad, name = "finfutbad_count") %>% 
  mutate(futbad = ifelse(finfutbad_count > 0, 1, 0)) %>% 
  # create a variable if they were married or got married during the first 3 years
  separate(combo, into = c("combomar", "combojob"), sep = "-", convert = TRUE) %>% 
  mutate(marstat = ifelse(is.na(marstat), combomar, marstat)) %>% 
  mutate(mar = ifelse(marstat == "married", 1, 0)) %>% 
  add_tally(mar, name = "mar_count") %>% 
  # married is a dummy if they got married or were married
  mutate(married = ifelse(mar_count > 0, 1, 0)) %>% 
  # cohabnm is a variable if the cohabited but did not marry in first 3 years
  mutate(cohab = ifelse(marstat == "cohab", 1, 0)) %>% 
  add_tally(cohab, name = "cohab_count") %>% 
  mutate(cohabnm = ifelse(mar_count == 0 & cohab_count > 0, 1, 0)) %>% 
  mutate(obsnum = row_number()) %>% 
  arrange(desc(obsnum)) %>% 
  mutate(obsnumrev = row_number()) %>% 
  ungroup() %>% 
  # creates 3 cohorts
  mutate(byr = year(dob)) %>%
  mutate(cohort3 = ifelse(byr >= 1990, ">=1990", ifelse(byr <= 1975, "<=1975", "1976-1989"))) %>% 
  mutate(cohort3 = as.character(cohort3)) %>% 
  # creates 2 cohorts
  mutate(cohort2 = ifelse(byr >= 1986, ">=1986", "<=1985")) %>% 
  mutate(cohort2 = as.character(cohort2)) %>% 
  # Make the DF manageable by cutting out unneeded variable names
  select(pidp, obsnumrev, birth, sex, female, dvage, edu, immigrant, gor_dv, cohort3, cohort2, pji3, jbless, jbless2, presbad, futbad, married, cohabnm, fihhmnnet4_dv, fimnnet_dv) %>% 
  # Cut the sample to just line per individual, removing the long format
  filter(obsnumrev == 1) %>% 
  mutate(cohort3 = fct_relevel(cohort3, c(">=1990", "1976-1989", "<=1975"))) %>% 
  mutate(cohort2 = fct_relevel(cohort2, c(">=1986", "<=1985")))
  # %>% filter(cohort3 != "<=1975")


first3 %>% count(jbless2)

first3m <- first3 %>% filter(sex == 1)
first3m %>% count(birth)
first3f <- first3 %>% filter(sex == 2)
first3f %>% count(birth)

###########################################################################
# Logistic Regression -----------------------------------------------------
###########################################################################

# Outcome variable is called "birth"


logit <- glm(birth ~ female + dvage + edu + immigrant + gor_dv + jbless + presbad + futbad + fihhmnnet4_dv, data = first3, family = "binomial")
summary(logit)
#Wald test to check if the effect of education is overall statisitcally significant
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 3:4)
exp(cbind(OR = coef(logit), confint(logit)))
with(logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) #Test to see if the model fits better than the null model
logit2 <- glm(birth ~ female + dvage + edu + immigrant + gor_dv + jbless + presbad + futbad + married + cohabnm, data = first3, family = "binomial")
summary(logit2)

###Not currently included: cohort and gov't office
# Men
logitm <- glm(birth ~ dvage + jbless2 + presbad + futbad, data = first3m, family = "binomial")
summary(logitm)
logitm1 <- glm(birth ~ dvage + edu + immigrant + jbless2 + presbad + futbad, data = first3m, family = "binomial")
summary(logitm1)
logitm2 <- glm(birth ~ dvage + edu + immigrant + jbless2 + presbad + futbad + fimnnet_dv, data = first3m, family = "binomial")
summary(logitm)
wald.test(b = coef(logitm2), Sigma = vcov(logitm), Terms = 2:3)
logitm3 <- glm(birth ~ dvage + edu +immigrant + jbless2 + presbad + futbad + married + cohabnm, data = first3m, family = "binomial")
summary(logitm3)

# Women
logitf <- glm(birth ~ dvage + jbless2 + presbad + futbad , data = first3f, family = "binomial")
summary(logitf)
logitf1 <- glm(birth ~ dvage + edu + immigrant + jbless2 + presbad + futbad , data = first3f, family = "binomial")
summary(logitf1)
logitf2 <- glm(birth ~ dvage + edu +immigrant + jbless2 + presbad + futbad + fimnnet_dv , data = first3f, family = "binomial")
summary(logitf2)
logitf3 <- glm(birth ~ dvage + edu +immigrant + jbless2 + presbad + futbad + married + cohabnm, data = first3f, family = "binomial")
summary(logitf3)


###########################################################################
# Model Output ------------------------------------------------------------
###########################################################################

#Export to a html file
export_summs(logitm, logitm1, logitm2, logitm3, logitf, logitf1, logitf2, logitf3,
             model.names = c("Men 1", "Men 2", "Men 3", "Men 4", "Women 1", "Women 2", "Women 3", "Women 4"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("At least 1 month jobless" = "jbless",
             #           "Age" = "dvage",
             #           "Finding it difficult" = "presbad",
             #           "Worse off" = "futbad",
             #           "1976-1989" = "cohort31976-1989",
             #           "<= 1975" = "cohort3<=1975",
             #           "Education Low" = "edulow",
             #           "Education Medium" = "edumedium",
             #           "Immigrant" = "immigrant1",
             #           "Cohab - Never Married" = "cohabnm",
             #           "Married" = "married"),
             exp = TRUE,
             to.file = "html",
             file.name = "logit_model_paper1_04-10-21.html")

#Export to a docx file
export_summs(logitm, logitm1, logitm2, logitm3, logitf, logitf1, logitf2, logitf3,
             model.names = c("Men 1", "Men 2", "Men 3", "Men 4", "Women 1", "Women 2", "Women 3", "Women 4"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("At least 1 month jobless" = "jbless",
             #           "Age" = "dvage",
             #           "Finding it difficult" = "presbad",
             #           "Worse off" = "futbad",
             #           "1976-1989" = "cohort31976-1989",
             #           "<= 1975" = "cohort3<=1975",
             #           "Education Low" = "edulow",
             #           "Education Medium" = "edumedium",
             #           "Immigrant" = "immigrant1",
             #           "Cohab - Never Married" = "cohabnm",
             #           "Married" = "married"),
             exp = TRUE,
             to.file = "docx",
             file.name = "logit_model_paper1_30-09-21.docx")

# Table using Arsenal
mycontrols <- tableby.control(test = FALSE)
logitstats <-arsenal::tableby(birth ~ sex + dvage + edu + cohort2 +immigrant + jbless2 + presbad + futbad + married + cohabnm, data = first3, control = mycontrols)
# labels(fullstats) <-  c(t2 = "Time since end of education (months)", sex = "Sex", pji3 = "PJI", employed = "Employed",
#                         finnow3cat = "Present Finacial", finfut.imp = "Future Finacial", edu = "Educational Attainment")
summary(logitstats)
write2word(logitstats , "logitstats_first3_30-09-2021.docx") 
write2html(logitstats , "logitstats_first3_30-09-2021.html")
