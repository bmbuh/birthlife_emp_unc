#Coded by: Brian Buh
#Started on: 17.02.2021
#Last Updated: 25.02.2021

# install.packages("zoo")
# install.packages("survminer")
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/survminer", build_vignettes = FALSE)
# install.packages("survPen")
# install.packages("flexsurv")
# install.packages("coxme")

library(data.table)
library(padr)
library(tidyverse)
library(haven)
library(lubridate)
library(arsenal)
library(zoo)
library(survival)
library(survminer)
library(survPen)
library(flexsurv)
library(coxme)


#the first task is filling in missing interview dates from missing internal waves
#This is probably best accomplished by simply picking the month halfway between the existing waves
#This process will be less extensive after eliminating the rows before the first observed waves and after the last observed wave

com_panel <- file.choose()
com_panel <- readRDS(com_panel)

enddate <- com_panel %>% 
  unite(intdate, c(intdatm_dv, intdaty_dv), sep = "-") %>% #combines the mnth & yr var to make int dates
  mutate(intdate = parse_date_time(intdate, "my")) %>% 
  dplyr::select(pidp, wave, intdate)

foo <- function(x) coredata(na.approx(zoo(x), na.rm = FALSE))
enddate2 <- data.table(enddate)
enddate2[, intdate := foo(intdate), by = "pidp"]
# DT[, enddate := foo(enddate), by = "pidp"]

enddate3 <- enddate2 %>% 
  mutate(wave = wave - 1) %>% 
  filter(wave != 0) %>% 
  rename("enddate" = "intdate")

enddate4 <- 
  left_join(enddate, enddate3, by = c("pidp", "wave")) %>% 
  rename("startdate" = "intdate")

com_panel2 <- com_panel %>% 
  mutate(imp = ifelse(is.na(dvage), 1, 0)) %>% 
  left_join(., enddate4, by = c("pidp", "wave")) %>% 
  filter(fwtest >= 0, lwtest >= 0)%>% #removes rows before the first int or after the last
  # unite(intdate, c(intdatm_dv, intdaty_dv), sep = "-") %>% #combines the mnth & yr var to make int dates
  # mutate(intdate = parse_date_time(intdate, "my")) %>% 
  unite(dob, c(birthm, birthy), sep = "-") %>% #creates a dob
  mutate(dob = parse_date_time(dob, "my")) %>% 
  # mutate(age = (intdate - dob)/(365*24*60*60)) %>% #Done so I can use age at each wave
  group_by(pidp) %>% 
  fill(fb_check) %>%
  fill(dvage) %>% 
  fill(gor_dv) %>% 
  fill(racel_dv) %>% 
  fill(generation) %>% 
  ungroup() %>% 
  filter(fb_check == 0 | fb_check == 2) %>% #this variable takes the observed "anychild" and subtracts the binary "kdob oberseved" 1 = had child but no kdob or not had child but observed kdob
  # filter(case_when(sex == 1 ~ age_start <= 50, sex == 2 ~ age_start <= 45) %>% 
  dplyr::select(pidp, wave, imp, kdob, sex, dvage, dob, racel_dv, gor_dv, ppid, marstat_dv, jbsec, generation,
                edu_cat, se_ee, finnow.imp, finfut.imp, startdate, enddate)


#In order to fill in start dates from missing waves
#Calculated as the median point between the two waves
foo <- function(x) coredata(na.approx(zoo(x), na.rm = FALSE))
com_panel3 <- data.table(com_panel2)
com_panel3[, startdate := foo(startdate), by = "pidp"]
# DT[, enddate := foo(enddate), by = "pidp"]


#Some median dates are halfway between the month
#The "floor_date" function in lubridate didn't work, so this is a function to round to start of month
firstOfMonth <- function(dates) {
  as.Date(strftime(dates, format="%Y-%m-01"))}


com_panel4 <- com_panel3 %>% 
  mutate(startdate = firstOfMonth(startdate)) %>% 
  mutate(enddate = firstOfMonth(enddate)) %>% 
  mutate(gap = as.duration(startdate %--% enddate) / dmonths(1)) %>% 
  mutate(gap = round(gap, digits = 0)) %>% 
  group_by(pidp) %>% 
  mutate(rollgap = cumsum(gap)) %>% 
  mutate(condate = as.Date(kdob %m-% months(9))) %>% 
  mutate(fbgap = as.duration(startdate %--% condate) / dmonths(1)) %>% 
  mutate(test = ifelse(is.na(enddate), 1, 0)) %>% 
  fill(enddate) %>% 
  mutate(enddate = ifelse(test == 1, enddate %m+% months(1), enddate)) %>% 
  mutate(enddate = as.Date(enddate))

com_panel5 <- com_panel4 %>% 
  mutate(condate = ifelse(enddate <= condate, NA, condate)) %>% #The next several lines is done to remove waves after first birth
  mutate(condate = as.Date(condate)) %>% 
  mutate(condate = ifelse(is.na(condate), 1900-01-01, condate)) %>% 
  mutate(condate = as.Date(condate)) %>% 
  mutate(enddate2 = ifelse(condate <= enddate, condate, enddate)) %>% 
  mutate(enddate2 = ifelse(enddate2 == 1898, enddate, enddate2)) %>% 
  mutate(enddate2 = as.Date(enddate2)) %>% 
  mutate(cut = ifelse(startdate > enddate2, 1, 0)) %>% 
  filter(cut != 1) %>% 
  mutate(gap2 = as.duration(startdate %--% enddate2) / dmonths(1)) %>% 
  mutate(gap2 = round(gap2, digits = 0)) %>% 
  group_by(pidp) %>% 
  mutate(rollgap2 = cumsum(gap2)) %>% 
  mutate(time1 = rollgap2 - gap2) %>% 
  rename("time2" = "rollgap2") %>% 
  mutate(event = ifelse(condate == enddate2, 1, 0)) %>% 
  dplyr::select(-gap2, -cut, -enddate, -gap, -rollgap, -fbgap, -condate, -test) %>% 
  rename("enddate" = "enddate2") %>% 
  mutate(dob = as.Date(dob)) %>% 
  mutate(agemn = as.duration(dob %--% startdate) / dmonths(1)) %>% 
  mutate(agemn = round(agemn, digits = 0)) %>% 
  mutate(agesq = agemn*agemn) %>% 
  filter(case_when(sex == 1 ~ agemn <= 600, sex == 2 ~ agemn <= 540)) #filters men over 50 and women over 45

  
###########################################################################
# Cox Prop Haz Model ------------------------------------------------------
###########################################################################

#There is an issue where some end dates are the same as the start dates because the baby was born the same month as the interview
#This is solved by adding one month to the end date
com_panel6 <- com_panel5 %>% 
  mutate(test = time2 - time1) %>% 
  mutate(test2 = ifelse(test <= 0, 1, 0)) %>% 
  ungroup() %>% 
  mutate(time2 = test2 + time2) %>% 
  dplyr::select(-test, -test2)

#Save and load the combined individual data file as an RDS
saveRDS(com_panel6, file = "surv.rds")
surv <- file.choose()
surv <- readRDS(surv)

surv %>% 
  count(marstat_dv)

msurv <- surv %>% #data set for men
  filter(sex == 1)

fsurv <- surv %>% #data set for women
  filter(sex == 2)

#Kaplan-Meier non-parametric analysis
kmsurv_sex <- survfit(Surv(time1, time2, event) ~ se_ee + strata(sex), data = surv, cluster = pidp)
summary(kmsurv_sex)
plot(kmsurv_sex, xlab = "Time in months since first interview", ylab = "First Birth Probability by Sex")
ggsurvplot(kmsurv_sex, size = 1,   # change line size
           ylim = c(0.69,1) ,
           palette = c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = TRUE,          # Add confidence interval
          # pval = TRUE,              # Add p-value
           risk.table = TRUE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs =
             c("Male", "Female"),    # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw()      # Change ggplot2 theme 
           ) + labs(caption = "Survival probaility cut at 0.7")



kmsurv_edu <- survfit(Surv(time1, time2, event) ~ strata(edu_cat) + se_ee, data = surv, cluster = pidp)
summary(kmsurv_edu)
plot(kmsurv_edu, xlab = "Time", ylab = "First Birth Probability by Education")
ggsurvplot(kmsurv_edu, size = 1,   # change line size
           ylim = c(0.6,1),
           # palette = c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = TRUE,          # Add confidence interval
           # pval = TRUE,              # Add p-value
           risk.table = TRUE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs =
           c("High", "Low", "Medium", "Unknown"),    # Change legend labels
           risk.table.height = 0.4, # Useful to change when you have multiple groups
           ggtheme = theme_bw()      # Change ggplot2 theme 
) + labs(caption = "Survival probaility cut at 0.6") #+
  # ggsave("cox_edu.png")

coxph <- coxph(formula = Surv(time1, time2, event) ~ sex + se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat, data = surv, cluster = pidp, method = "breslow")
summary(coxph)
fcoxph <- coxph(formula = Surv(time1, time2, event) ~ sex + se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat, data = fsurv, cluster = pidp, method = "breslow")
summary(fcoxph)

survfit(Surv(time1, time2, event) ~ 1, data = surv, cluster = pidp)



#Testing splines for jbsec
spsurv <- surv %>% 
  fill(jbsec, .direction = "downup") %>% 
  mutate(jbsec = fct_relevel(jbsec, c("3 non-employed", "1 likely", "2 unlikely"))) %>%
  mutate(edu_cat = fct_relevel(edu_cat, c("other", "high", "medium", "low"))) %>% 
  mutate(jbsec2 = as.numeric(jbsec))

summary(spsurv)

table(spsurv$event, spsurv$finfut.imp)
survaov <- aov(event ~ finnow.imp, data = spsurv)
summary(survaov)

table(spsurv$finnow.imp, spsurv$finfut.imp)
chisqsurv <- chisq.test(table(spsurv$finnow.imp, spsurv$finfut.imp))
summary(chisqsurv)

# %>% 
#   timeSplitter(by = .5,
#                event_var = "status",
#                event_start_status = "Alive",
#                time_var = "time",
#                time_related_vars = c("age", "year"))

sp1 <- flexsurvreg(Surv(time1, time2, event) ~ jbsec, data = surv, k = 23, scale = "hazard")
summary(sp1)

#This model uses a ridge regression on jbsec 
#The ridge regression pushes the likelihood estimator towards zero(aka making the known effect of jbsec shrink from the measurement)
coxphridge <- coxph(formula = Surv(time1, time2, event) ~ sex + se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat + ridge(jbsec, theta = 23, scale = TRUE), data = spsurv, cluster = pidp, method = "breslow")
summary(coxphridge)
cox.zph(coxphridge)

#Frality function allows for adding a simple random effect to a term
coxphfrailty <- coxph(formula = Surv(time1, time2, event) ~ sex + se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat + frailty.gaussian(jbsec2), data = spsurv, cluster = pidp, method = "breslow")
summary(coxphfrailty)
cox.zph(coxphfrailty)

coxphpsp<- coxph(formula = Surv(time1, time2, event) ~ sex + se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat + pspline(jbsec2), data = spsurv, cluster = pidp, method = "breslow")
summary(coxphpsp)
cox.zph(coxphpsp)

#this allows for mixed-effects modeling. The variable in the bracket (this case jbsec) is used to create the mixed effects (partial pooling effect)
coxme <- coxme(formula = Surv(time1, time2, event) ~ sex + se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat + (1 | jbsec), data = spsurv)
summary(coxme)

#Graph to look at the age of first birth conception
agetest <- surv %>% 
  dplyr::select(sex, agemn, event) %>% 
  mutate(age = agemn/12) %>% 
  filter(event == 1) %>% 
  arrange(age) %>% 
  group_by(age) %>% 
  add_tally() %>% 
  mutate(sex = as.factor(sex))

str(agetest)


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
# Couple data set ---------------------------------------------------------
###########################################################################

partnerid <- surv %>% 
  dplyr::select(ppid, wave) %>% 
  fill(ppid) %>% 
  rename("pidp" = "ppid") %>% 
  left_join(., com_panel, by = c("pidp", "wave"))%>% 
  filter(!is.na(sex)) %>% 
  dplyr::select(pidp, wave, sex, birthm, birthy, racel_dv,
                jbsec, generation, edu_cat, se_ee, finnow.imp, finfut.imp) %>%
  unite(dob, c(birthm, birthy), sep = "-") %>% #creates a dob
  mutate(dob = parse_date_time(dob, "my")) %>% 
  fill(racel_dv, generation) %>% 
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames)


  oldnames = c("pidp", "wave", "sex", "dob", "racel_dv", "jbsec", "generation", 
               "edu_cat", "se_ee", "finnow.imp", "finfut.imp")
  newnames = c("ppid", "wave", "sexp", "dobp", "racel_dvp", "jbsecp", "generationp", 
               "edu_catp", "se_eep", "finnow.impp", "finfut.impp")
  
couplesurv <- 
  left_join(surv, partnerid, by = c("ppid", "wave")) %>% 
  filter(!is.na(sexp)) %>% 
  mutate(dobp = as.Date(dobp)) %>% 
  mutate(agemnp = as.duration(dobp %--% startdate) / dmonths(1)) %>% 
  mutate(agemnp = round(agemnp, digits = 0)) 

fcouplesurv <- couplesurv %>% 
  filter(sex == 2)

couplecoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + se_eep + agemn + agemnp, data = couplesurv, cluster = pidp, method = "breslow")
summary(couplecoxph)

##### Leftovers

time1 <- com_panel5 %>% 
  dplyr::select(pidp, wave, rollgap2) %>% 
  mutate(rollgap3 = )
  
test %>% 
  ungroup() %>% 
  count(cut)
  
  mutate(rollsum = rollapply(gap, FUN = sum, fill = NA))
  


str(com_panel3)


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
