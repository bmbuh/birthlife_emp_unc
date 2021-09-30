#Coded by: Brian Buh
#Started on: 21.07.2021
#Last Updated: 31.08.2021

# install.packages("plyr")

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
# library(plyr)

###########################################################################
# Load DF -----------------------------------------------------------------
###########################################################################

surv5 <- file.choose()
surv5 <- readRDS(surv5)

# test <- surv5 %>% mutate(byr = year(dob))%>% 
#   group_by(pidp) %>% 
#   arrange(pidp, desc(wave)) %>% 
#   mutate(rev_time = row_number()) %>% 
#   filter(rev_time == 1) %>% 
#   ungroup() %>% 
#   mutate(fb = ifelse(is.na(kdob), 0, 1)) %>% 
#   mutate(fb = as.factor(fb))
# test %>% count(fb)
# summary(test$byr)
# test %>% 
#   ggplot(aes(byr, fill = fb)) +
#   geom_bar()

survemp<- file.choose()
survemp<- readRDS(survemp)

#PJI First 3 years post-education
pji_3yr <- read_dta("S:/r_projects/Emp_Unc_Fertility_Birthlife/pji_busetta_mendola/panel_pji_3yr_run.dta")

cci <- file.choose()
cci <- readRDS(cci)


###########################################################################
# Full Sample - 3 Years ---------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Making a dataset fit for new models -------------------------------------
# -------------------------------------------------------------------------

pji_3yr_2 <- pji_3yr %>% 
  rename("pidp" = "id", "pji3" = "se_ee") %>% 
  select(pidp, pji3) %>% 
  filter(!is.na(pji3))


#DF for 3 years after the end of education
surv6 <- surv5 %>% 
  filter(t1 >= 36) %>% 
  left_join(. , pji_3yr_2, by = "pidp") %>% 
  #Here I start to add my final controls
  ### Control cohort
  mutate(byr = year(dob)) %>% 
  mutate(cohort = ifelse(byr <= 1979, 0, 1)) %>% 
  mutate(cohort = as.character(cohort)) %>% 
  mutate(cohort2 = ifelse(byr <= 1975, "<=1975", ifelse(byr >= 1990, ">=1990", "1976-1989"))) %>% 
  mutate(cohort2 = as.character(cohort2)) %>% 
  mutate(cohort2 = fct_relevel(cohort2, c("1976-1989", "<=1975", ">=1990"))) %>% 
  mutate(cohort3 = ifelse(byr <= 1969, 0, ifelse(byr >= 1990, 3, ifelse(byr >= 1970 & byr <= 1979, 1, 2)))) %>% 
  mutate(cohort3 = as.character(cohort3)) %>% 
  left_join(., cci, by = "startdate") %>% 
  mutate(empalt = ifelse(jbstat == "Paid employed" | jbstat == "Self-employed", "employed", 
                         ifelse(jbstat == "Unemployed", "unemployed", "inactive"))) %>% 
  mutate(empalt = ifelse(is.na(empalt), "inactive", empalt))

saveRDS(surv6, "surv6.rds")

surv5 %>% count(jbstat)
surv6 %>% count(empalt)
str(surv6)

#Men
surv6m <- surv6 %>% filter(sex == 1) 
# surv6m %>% count(combo)
# pjitestm <- surv6m %>% 
#   mutate(jbless = pji3 > 0)
# pjitestm %>% count(jbless)
#Women
surv6f <- surv6 %>% filter(sex == 2)
# surv6f %>% count(combo)
# pjitestf <- surv6f %>% 
#   mutate(jbless = pji3 > 0)
# pjitestf %>% count(jbless)




# -------------------------------------------------------------------------
# Full Sample Model Run ---------------------------------------------------
# -------------------------------------------------------------------------
 
####Model for men
baseline_mglm <- glm(formula = event ~ t2,
                     family = binomial(link = "cloglog"),
                     data = surv6m)
summ(baseline_mglm, exp = TRUE, scale = TRUE)

mglm1 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant,
             family = binomial(link = "cloglog"),
             data = surv6m)
summary(mglm1)
summ(mglm1, exp = TRUE)

mglm2 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant + combo,
            family = binomial(link = "cloglog"),
            data = surv6m)
summary(mglm2)
summ(mglm2, exp = TRUE)

####Model for women
baseline_fglm <- glm(formula = event ~ t2,
                     family = binomial(link = "cloglog"),
                     data = surv6f)
summ(baseline_fglm, exp = TRUE)
fglm1 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant,
             family = binomial(link = "cloglog"),
             data = surv6f)
summary(fglm1)
summ(fglm1, exp = TRUE)

fglm2 <- glm(formula = event ~ t2 + pji3 + employed + finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant + combo,
            family = binomial(link = "cloglog"),
            data = surv6f)
summary(fglm2)
summ(fglm2, exp = TRUE)

# -------------------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# -------------------------------------------------------------------------

export_summs(mglm1, empmglm1, mglm2, empmglm2, fglm1, empfglm1, fglm2, empfglm2,
             model.names = c("Men 1", "Employed Men 1", "Men 2", "Employed Men 2", "Women 1", "Employed Women 1", "Women 2", "Employed Women 2"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2",
                       "PJI" = "pji3",
                       "Employed" = "employed",
                       "Finding it difficult" = "finnow3catfinddifficult",
                       "Getting by" = "finnow3catgetby",
                       "Worse off" = "finfut.impWorse off",
                       "Better off" = "finfut.impBetter off",
                       "CCI" = "cci",
                       "Age in Months" = "agemn",
                       "Age Squared" = "agesq",
                       "< = 1975" = "cohort2<=1975",
                       ">= 1990" = "cohort2>=1990",
                       "Education Low" = "edulow",
                       "Education Medium" = "edumedium",
                       "Immigrant" = "immigrant1",
                       "Time 2" = "t2_3",
                       "Likely lose job next 12 months" = "jbsec.dummy1",
                       "Permanent Contract" = "permcon",
                       "Part-Time" = "parttime",
                       "Cohab - Employed" = "combocohab-employed",
                       "Cohab - Non-employed" = "combocohab-non-employed",
                       "Cohab - Unknown" = "combocohab-unknown",
                       "Married - Employed" = "combomarried-employed",
                       "Married - Non-employed" = "combomarried-non-employed",
                       "Married - Unknown" = "combomarried-unknown"),
             exp = TRUE,
             to.file = "html",
             file.name = "full_model_paper1_31-08-21.html")

#Export to a PDF file
export_summs(mglm1, empmglm1, mglm2, empmglm2, fglm1, empfglm1, fglm2, empfglm2,
             model.names = c("Men 1", "Employed Men 1", "Men 2", "Employed Men 2", "Women 1", "Employed Women 1", "Women 2", "Employed Women 2"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2",
                       "PJI" = "pji3",
                       "Employed" = "employed",
                       "Finding it difficult" = "finnow3catfinddifficult",
                       "Getting by" = "finnow3catgetby",
                       "Worse off" = "finfut.impWorse off",
                       "Better off" = "finfut.impBetter off",
                       "CCI" = "cci",
                       "Age in Months" = "agemn",
                       "Age Squared" = "agesq",
                       "< = 1975" = "cohort2<=1975",
                       ">= 1990" = "cohort2>=1990",
                       "Education Low" = "edulow",
                       "Education Medium" = "edumedium",
                       "Immigrant" = "immigrant1",
                       "Time 2" = "t2_3",
                       "Likely lose job next 12 months" = "jbsec.dummy1",
                       "Permanent Contract" = "permcon",
                       "Part-Time" = "parttime",
                       "Cohab - Employed" = "combocohab-employed",
                       "Cohab - Non-employed" = "combocohab-non-employed",
                       "Cohab - Unknown" = "combocohab-unknown",
                       "Married - Employed" = "combomarried-employed",
                       "Married - Non-employed" = "combomarried-non-employed",
                       "Married - Unknown" = "combomarried-unknown"),
             exp = TRUE,
             to.file = "pdf",
             file.name = "full_model_paper1_31-08-21.pdf")

#Export to a docx file
export_summs(mglm1, empmglm1, mglm2, empmglm2, fglm1, empfglm1, fglm2, empfglm2,
             model.names = c("Men 1", "Employed Men 1", "Men 2", "Employed Men 2", "Women 1", "Employed Women 1", "Women 2", "Employed Women 2"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             coefs = c("Time since Education" = "t2",
                       "PJI" = "pji3",
                       "Employed" = "employed",
                       "Finding it difficult" = "finnow3catfinddifficult",
                       "Getting by" = "finnow3catgetby",
                       "Worse off" = "finfut.impWorse off",
                       "Better off" = "finfut.impBetter off",
                       "CCI" = "cci",
                       "Age in Months" = "agemn",
                       "Age Squared" = "agesq",
                       "< = 1975" = "cohort2<=1975",
                       ">= 1990" = "cohort2>=1990",
                       "Education Low" = "edulow",
                       "Education Medium" = "edumedium",
                       "Immigrant" = "immigrant1",
                       "Time 2" = "t2_3",
                       "Likely lose job next 12 months" = "jbsec.dummy1",
                       "Permanent Contract" = "permcon",
                       "Part-Time" = "parttime",
                       "Cohab - Employed" = "combocohab-employed",
                       "Cohab - Non-employed" = "combocohab-non-employed",
                       "Cohab - Unknown" = "combocohab-unknown",
                       "Married - Employed" = "combomarried-employed",
                       "Married - Non-employed" = "combomarried-non-employed",
                       "Married - Unknown" = "combomarried-unknown"),
             exp = TRUE,
             to.file = "docx",
             file.name = "full_model_paper1_31-08-21.docx")


 # plot_summs(mglm, fglm, 
#            model.names = c("Men", "Women"),
#            coefs = c("Time since Education" = "t2",
#                      "PJI" = "pji3",
#                      "Finding it difficult" = "finnow3catfinddifficult",
#                      "Getting by" = "finnow3catgetby",
#                      "Employed" = "employed",
#                      "Worse off" = "finfut.impWorse off",
#                      "Better off" = "finfut.impBetter off",
#                      "Education Low" = "edulow",
#                      "Education Medium" = "edumedium",
#                      "Age in Months" = "agemn",
#                      "Age Squared" = "agesq"),
#            exp = TRUE) +
#             ggsave("full_model_paper1_21-07-21.png")


###########################################################################
# Employed Sample - 3 Years -----------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Making a dataset fit for new models -------------------------------------
# -------------------------------------------------------------------------

#DF for 3 years after the end of education
survemp3 <- survemp %>% 
  filter(t1 >= 36) %>% 
  left_join(. , pji_3yr_2, by = "pidp") %>% 
  mutate(parttime = ifelse(is.na(parttime), 0, parttime)) %>% 
  mutate(permcon = ifelse(is.na(permcon), 1, permcon)) %>% 
  mutate(cohort2 = ifelse(byr <= 1975, "<=1975", ifelse(byr >= 1990, ">=1990", "1976-1989"))) %>% 
  mutate(cohort2 = as.character(cohort2)) %>% 
  mutate(cohort2 = fct_relevel(cohort2, c("1976-1989", "<=1975", ">=1990")))
  # mutate(permcon = as.character(permcon)) %>% 
  # mutate(parttime = as.character(parttime))

str(survemp3)

#Men
survemp3m <- survemp3 %>% filter(sex == 1) 
#Women
survemp3f <- survemp3 %>% filter(sex == 2)

# #Men
# survemp3m %>% count(isco)
# #Women
# survemp3f %>% count(isco)
# pjitestmemp <- survemp3m %>% 
#   mutate(jbless = pji3 > 0)
# pjitestmemp %>% count(jbless)
# pjitestfemp <- survemp3f %>% 
#   mutate(jbless = pji3 > 0)
# pjitestfemp %>% count(jbless)

# -------------------------------------------------------------------------
# Employed Sample Model Run -----------------------------------------------
# -------------------------------------------------------------------------


####Model for men
empmglm1 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + edu + immigrant,
                family = binomial(link = "cloglog"),
                data = survemp3m)
summary(empmglm1)
summ(empmglm1, exp = TRUE)

empmglm2 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + edu + immigrant + combo,
               family = binomial(link = "cloglog"),
               data = survemp3m)
empmglm3 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + edu + immigrant,
                family = binomial(link = "cloglog"),
                data = survemp3m)
summary(empmglm2)
summ(empmglm2, exp = TRUE)

####Model for women
empfglm1 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + edu + immigrant,
                family = binomial(link = "cloglog"),
                data = survemp3f)
summary(empfglm1)
summ(empfglm1, exp = TRUE)

empfglm2 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + edu + immigrant + combo,
                family = binomial(link = "cloglog"),
                data = survemp3f)
summary(empfglm2)
summ(empfglm2, exp = TRUE)
empmglf3 <- glm(formula = event2 ~ t2_3 + pji3 + finnow3cat + finfut.imp + cci + jbsec.dummy + permcon + parttime + agemn + agesq + cohort2 + edu + immigrant,
                family = binomial(link = "cloglog"),
                data = survemp3f)

# -------------------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# -------------------------------------------------------------------------
export_summs(empmglm1, empmglm3, empmglm2, empfglm1, empmglf3, empfglm2,
             # model.names = c("Employed Men 1", "Employed Men 2", "Employed Women 1", "Employed Women 2"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t2_3",
             #           "PJI - 3yrs" = "pji3",
             #           "Permanent Contract" = "permcon1",
             #           "Job Security" = "jbsec.dummy1",
             #           "Education Low" = "edulow",
             #           "Education Medium" = "edumedium",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq"),
             exp = TRUE,
             to.file = "html",
             file.name = "employed_model_output_25-08-21.html")


survemp %>% count(parttime)
survemp3 %>% count(parttime)

test <- survemp3 %>% 
  filter(is.na(parttime))

test %>% count(event)
