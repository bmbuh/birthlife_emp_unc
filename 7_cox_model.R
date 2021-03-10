#Coded by: Brian Buh
#Started on: 17.02.2021
#Last Updated: 04.03.2021

# install.packages("zoo")
# install.packages("survminer")
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/survminer", build_vignettes = FALSE)
# install.packages("survPen")
# install.packages("flexsurv")
# install.packages("coxme")
# install.packages("texreg")
# install.packages("forestplot")
# install.packages("sjPlot")


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
library(stargazer)
library(texreg)
library(forestplot)
library(sjPlot)


###########################################################################
# Creating starting and ending dates --------------------------------------
###########################################################################


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
  fill(combo) %>% 
  ungroup() %>% 
  filter(fb_check == 0 | fb_check == 2) %>% #this variable takes the observed "anychild" and subtracts the binary "kdob oberseved" 1 = had child but no kdob or not had child but observed kdob
  # filter(case_when(sex == 1 ~ age_start <= 50, sex == 2 ~ age_start <= 45) %>% 
  dplyr::select(pidp, wave, imp, hhorig, kdob, sex, dvage, dob, racel_dv, gor_dv, ppid, marstat, parjbstat, combo, jbsec, generation,
                edu_cat, se_ee, finnow.imp, finfut.imp, startdate, enddate)

com_panel2 %>% 
  count(combo)

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

com_panel5 %>% 
  ungroup() %>% 
  count(combo)

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

###########################################################################
# Cox Prop Haz Model ------------------------------------------------------
###########################################################################

#Descriptive Stats of Sample
#Note: needed to switch to "spsurv" data set to include imputed jbsec

mycontrols <- tableby.control(test = FALSE)
survstats <-arsenal::tableby(sex ~ se_ee + finnow.imp + finfut.imp + jbsec + edu_cat + combo, data = spsurv, control = mycontrols)
labels(survstats) <-  c(se_ee = "PJI", finnow.imp = "Present Financial Outlook", finfut.imp = "Future Financial Outlook",
                    jbsec = "Job Security", edu_cat = "Educational Attainment", combo = "Partnership, Partner's Job Status")
summary(survstats)
write2word(survstats, "survstats.doc")


#Seperate datasets for men and women for the survival analysis
msurv <- surv %>% #data set for men
  filter(sex == 1)

fsurv <- surv %>% #data set for women
  filter(sex == 2)

#Kaplan-Meier non-parametric analysis
kmsurv <- surv %>% 
  mutate(sex = ifelse(sex == 2, 1, 2))
kmsurv_sex <- survfit(Surv(time1, time2, event) ~ strata(sex), data = kmsurv, cluster = pidp)
summary(kmsurv_sex)
plot(kmsurv_sex, xlab = "Time in months since first interview", ylab = "First Birth Probability by Sex")
ggsurvplot(kmsurv_sex, size = 1,   # change line size
           ylim = c(0.69,1),
           # palette = c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = TRUE,          # Add confidence interval
          # pval = TRUE,              # Add p-value
           risk.table = TRUE,        # Add risk table
           # risk.table.col = "strata",# Risk table color by groups
           legend.labs =
           c("Women", "Men"),    # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw()      # Change ggplot2 theme
           ) + labs(caption = "Survival probaility cut at 0.7")
# 
# 
# 
# kmsurv_edu <- survfit(Surv(time1, time2, event) ~ strata(edu_cat) + se_ee, data = surv, cluster = pidp)
# summary(kmsurv_edu)
# plot(kmsurv_edu, xlab = "Time", ylab = "First Birth Probability by Education")
# ggsurvplot(kmsurv_edu, size = 1,   # change line size
#            ylim = c(0.6,1),
#            # palette = c("#E7B800", "#2E9FDF"),# custom color palettes
#            conf.int = TRUE,          # Add confidence interval
#            # pval = TRUE,              # Add p-value
#            risk.table = TRUE,        # Add risk table
#            risk.table.col = "strata",# Risk table color by groups
#            # legend.labs =
#            # c("High", "Low", "Medium", "Unknown"),    # Change legend labels
#            risk.table.height = 0.4, # Useful to change when you have multiple groups
#            ggtheme = theme_bw()      # Change ggplot2 theme 
# ) + labs(caption = "Survival probaility cut at 0.6") #+
#   # ggsave("cox_edu.png")

#First run - Does not include jbsec
coxph <- coxph(formula = Surv(time1, time2, event) ~ strata(sex) + se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat + combo, data = surv, cluster = pidp, method = "breslow")
summary(coxph)
fcoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat, data = fsurv, cluster = pidp, method = "breslow")
summary(fcoxph)
mcoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat, data = msurv, cluster = pidp, method = "breslow")
summary(mcoxph)
fparcoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat + combo, data = fsurv, cluster = pidp, method = "breslow")
summary(fcoxph)
mparcoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat + combo, data = msurv, cluster = pidp, method = "breslow")
summary(mcoxph)

#Kaplan-Meier Non-paramedic analysis of sex
survsex <- survfit(Surv(time1, time2, event) ~ strata(sex), data = surv, cluster = pidp)
summary(survsex)
plot(survsex)
ggsurvplot(survsex,
           size = 1,
           ylim = c(0.67, 1),
           #palette = c("#E7B800", "#2E9FDF"),
           conf.int = TRUE,
           risk.table = TRUE,
           risk.table.col = "strata", #changes the risk table numbers to the color of the figure
           legend.labs = c("Male", "Female"),
           risk.table.height = 0.25,
           ggtheme = theme_bw(),
           tables.theme = theme_classic()) +
  ggtitle("Non-paremetic ", subtitle =  "UKHLS = Measured 9 months before first birth")
  

#The 
# stargazer(fcoxph, fparcoxph, mcoxph, mparcoxph,
#           # title = "Ordinal logistic regression",
#           # dep.var.caption = "Change in number of intended children: Wave 5 - 10",
#           # dep.var.labels = c("Less intended", "Same intended", "More intended"),
#           # column.labels = c(),
#           # covariate.labels = c("Had a child", "Ratio of waves spent employed", "Ratio waves with positive financial outlook",
#           #                      "Age", "Stayed single", "Stayed married", "Got Married", "Medium education", "High education"),
#           notes.label = "Significance levels",
#           type = "html",
#           out = "test_stargazer.doc")

#Data spsurv mutates jbsec so that is can be used in the analysis
#Adding splines for jbsec
spsurv <- surv %>% 
  fill(jbsec, .direction = "downup") %>% 
  mutate(jbsec = fct_relevel(jbsec, c("3 non-employed", "1 likely", "2 unlikely"))) %>%
  mutate(edu_cat = fct_relevel(edu_cat, c("other", "high", "medium", "low"))) %>% 
  mutate(jbsec2 = as.numeric(jbsec))%>% 
  mutate(combo = fct_relevel(combo, c("cohab-unknown", "single-unknown", "cohab-employed", "cohab-non-employed",
                                      "married-employed", "married-non-employed", "married-unknown"))) %>% 
  mutate(finnow.imp = recode(finnow.imp,
                             "1 Living comfortably" = "Living comfortably",
                             "2 Doing alright" = "Doing alright",
                             "3 Just getting by" = "Just getting by",
                             "4 Finding it quite difficult" = "Quite difficult",
                             "5 Finding it very difficult" = "Very difficult"))

mspsurv <- spsurv %>% #data set for men
  filter(sex == 1)

fspsurv <- spsurv %>% #data set for women
  filter(sex == 2)

#Women without partner variables
fspcoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + finnow.imp + finfut.imp + ridge(jbsec, theta = 12, scale = TRUE) + agemn + agesq + edu_cat, data = fspsurv, cluster = pidp, method = "breslow")
summary(fspcoxph)
testph <- cox.zph(fspcoxph)
ggcoxzph(testph) #shows a chart for testing the prop hazard assumption. The close the line is to 0 slope the strong the assumption
zp <- cox.zph(fspcoxph, transform= function(time) log(time +20))
plot(zp[1])
abline(0,0, col=2)
abline(h= fspcoxph$coef[1], col=3, lwd=2, lty=2)
anova(zp) #Second tests of the PH Assumption. Again, se_ee and age seem to have problems, but after further tests both don't violate the assumption

#Men without partner variables
mspcoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + finnow.imp + finfut.imp + ridge(jbsec, theta = 12, scale = TRUE) + agemn + agesq + edu_cat, data = mspsurv, cluster = pidp, method = "breslow")
summary(mspcoxph)
cox.zph(mspcoxph)

#Women with partner variables
fspparcoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + finnow.imp + finfut.imp + ridge(jbsec, theta = 12, scale = TRUE) + agemn + agesq + edu_cat + combo, data = fspsurv, cluster = pidp, method = "breslow")
summary(fspparcoxph)
cox.zph(fspparcoxph)

#Men with partner variables
mspparcoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + finnow.imp + finfut.imp + ridge(jbsec, theta = 12, scale = TRUE) + agemn + agesq + edu_cat + combo, data = mspsurv, cluster = pidp, method = "breslow")
summary(mspparcoxph)
cox.zph(mspparcoxph)

#A test if geography makes a difference
geospsurv <- spsurv %>% mutate(gor_dv = as.factor(gor_dv))
spgeocoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + finnow.imp + finfut.imp + ridge(jbsec, theta = 12, scale = TRUE) + agemn + agesq + edu_cat + gor_dv, data = geospsurv, cluster = pidp, method = "breslow")
summary(mspgeocoxph)


#The stargazer package cannot handle cox.penal results so the texreg package needs to be used
extract(fspcoxph)

htmlreg(list(fspcoxph, fspparcoxph, mspcoxph, mspparcoxph),
        include.zph = FALSE,
        file = "ridge.test.html",
        single.row = TRUE,
        custom.model.names = c("Female", "Female", "Male", "Male"),
        custom.coef.names = c("PJI", "Pres. Fin. 'Doing alright'", "Pres. Fin. 'Just getting by'", "Pres. Fin. 'Finding it quite difficult'","Pres. Fin. 'Finding it very difficult'",
                               "Fut. Fin. 'Better off'", "Fut. Fin. 'Worse off'",
                              "Job security", 
                              "Age, in months", "Age Squared", 
                              "Edu. High", "Edu. Medium", "Edu. Low", "Single", "Cohab - employed", "Cohab - non-employed",
                              "Married - employed", "Married - non-employed", "Married - unknown"),
        groups = list("Employment uncertainty" = 1:8, "Controls" = 9:10, "Education (Ref = Edu. Other)" = 11:13, "Partnership, partner job status (Ref = Cohab - unknown)" = 14:19),
        bold = 0.05)

#the Forest graph looks really great but there is no possibility to edit var names without changing in the data set
ggforest(
  fspcoxph,
  data = fspsurv,
  main = "Hazard ratio - Men",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 0.7,
  refLabel = "reference",
  noDigits = 2
)

ggforest(
  mspparcoxph,
  data = mspsurv,
  main = "Hazard ratio",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 0.7,
  refLabel = "reference",
  noDigits = 2
)

#Chart for results minus partner controls
plot_models(mspcoxph, fspcoxph, 
            title = "Hazard Ratios",
            m.labels = c("Men", "Women"),
            legend.title = "Model",
            axis.labels = c(
              # "Married - unknown", "Married - non-employed","Married - employed",
              # "Cohab - non-employed", "Cohab - employed","Single",
              "Edu. Low", "Edu. Medium", "Edu. High",
              "Age Squared", "Age, in months",
              "Job security",
              "Fut. Fin. 'Worse off'", "Fut. Fin. 'Better off'",
              "Pres. Fin. 'Finding it very difficult'","Pres. Fin. 'Finding it quite difficult'", "Pres. Fin. 'Just getting by'","Pres. Fin. 'Doing alright'", "PJI"),
            axis.lim = c(0.4, 1.5),
            dot.size = 6,
            #colors  = c("#2E9FDF", "#E7B800"), #in case you wanna change to the gold blue set
            p.shape = TRUE,
            grid = TRUE)

#Chart for results plus partner controls
plot_models(mspparcoxph, fspparcoxph, 
            title = "Hazard Ratios",
            m.labels = c("Men", "Women"),
            legend.title = "Model",
            axis.labels = c(
              "Married - unknown", "Married - non-employed","Married - employed",
              "Cohab - non-employed", "Cohab - employed","Single",
              "Edu. Low", "Edu. Medium", "Edu. High",
              "Age Squared", "Age, in months",
              "Job security",
              "Fut. Fin. 'Worse off'", "Fut. Fin. 'Better off'",
              "Pres. Fin. 'Finding it very difficult'","Pres. Fin. 'Finding it quite difficult'", "Pres. Fin. 'Just getting by'","Pres. Fin. 'Doing alright'", "PJI"),
            axis.lim = c(0.3, 4.0),
            dot.size = 6,
            #colors  = c("#2E9FDF", "#E7B800"), #in case you wanna change to the gold blue set
            p.shape = TRUE,
            grid = TRUE)

#Kaplan-Meier non-paramedic analysis total model
# spsurvsex <- survfit(Surv(time1, time2, event) ~ se_ee + finnow.imp + finfut.imp + ridge(jbsec, theta = 12, scale = TRUE) + agemn + agesq + edu_cat, data = spsurv, cluster = pidp)
# summary(spsurvsex)
# plot(spsurvsex)
# ggsurvplot(spsurvsex,
#            size = 1,
#            ylim = c(0.67, 1),
#            palette = c("#E7B800", "#2E9FDF"),
#            conf.int = TRUE,
#            risk.table = TRUE,
#            risk.table.col = "strata", #changes the risk table numbers to the color of the figure
#            legend.labs = c("Male", "Female"),
#            risk.table.height = 0.25,
#            ggtheme = theme_bw(),
#            tables.theme = theme_classic())     

#ANOVA test for checking assumptions
table(spsurv$event, spsurv$finfut.imp)
survaov <- aov(event ~ finnow.imp, data = spsurv)
summary(survaov)
fit <- coxph(Surv(time1, time2, event) ~ sex + se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat + combo, data = surv)
fit <- coxph(Surv(time1, time2, event) ~ se_ee, data = spsurv)
anova(fit)



#Done higher up
#This model uses a ridge regression on jbsec 
#The ridge regression pushes the likelihood estimator towards zero(aka making the known effect of jbsec shrink from the measurement)
# coxphridge <- coxph(formula = Surv(time1, time2, event) ~ sex + se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat + ridge(jbsec, theta = 23, scale = TRUE), data = spsurv, cluster = pidp, method = "breslow")
# summary(coxphridge)
# cox.zph(coxphridge)

###############################################################################
#Frailty function allows for adding a simple random effect to a term----------
###############################################################################

coxphfrailty <- coxph(formula = Surv(time1, time2, event) ~ sex + se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat + frailty.gaussian(jbsec2), data = spsurv, cluster = pidp, method = "breslow")
summary(coxphfrailty)
cox.zph(coxphfrailty)

ffcoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + finnow.imp + finfut.imp  + frailty.gaussian(jbsec2) + agemn + agesq + edu_cat, data = fspsurv, cluster = pidp, method = "breslow")
summary(ffcoxph)
mfcoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + finnow.imp + finfut.imp + frailty.gaussian(jbsec2) + agemn + agesq + edu_cat, data = mspsurv, cluster = pidp, method = "breslow")
summary(mfcoxph)
ffparcoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + finnow.imp + finfut.imp + frailty.gaussian(jbsec2) + agemn + agesq + edu_cat + combo, data = fspsurv, cluster = pidp, method = "breslow")
summary(ffparcoxph)
mfparcoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + finnow.imp + finfut.imp + frailty.gaussian(jbsec2) + agemn + agesq + edu_cat + combo, data = mspsurv, cluster = pidp, method = "breslow")
summary(mfparcoxph)

htmlreg(list(ffcoxph, ffparcoxph, mfcoxph, mfparcoxph),
        file = "frailty.test.html",
        single.row = TRUE,
        custom.model.names = c("Female", "Female", "Male", "Male"),
        custom.coef.names = c("PJI", "Doing alright", "Just getting by", "Finding it quite difficult","Finding it very difficult",
                              "Better off", "Worse off",
                              "Non-employed", "Likely to lose job", "Unlikely to lose job", 
                              "Age, in months", "Age Squared", 
                              "Edu. High", "Edu. Medium", "Edu. Low", "Single", "Cohab - employed", "Cohab - non-employed",
                              "Married - employed", "Married - non-employed", "Married - unknown"),
        groups = list("Previous Joblessness" = 1:1, "Present Financial Outlook (Ref = Living comfortably)" = 2:5, "Future Financial Outlook (Ref = About the same)" = 6:7,
                      "Job Security" = 8:10, "Controls" = 11:12, "Education (Ref = Edu. Other)" = 13:15, "Partnership, partner job status (Ref = Cohab - unknown)" = 16:21),
        bold = 0.05,
        caption = "test")


# ggforest(
#   ffcoxph,
#   data = mspsurv,
#   main = "Hazard ratio",
#   cpositions = c(0.02, 0.22, 0.4),
#   fontsize = 0.7,
#   refLabel = "reference",
#   noDigits = 2
# )
# 
# plot_model(ffcoxph)

#It cannot plot with a frailty term!
# plot_models(ffcoxph
#             title = "Hazard Ratios",
#             #m.labels = c("Men", "Women"),
#             # axis.labels = c(
#             #   # "Married - unknown", "Married - non-employed","Married - employed",
#             #   # "Cohab - non-employed", "Cohab - employed","Single",
#             #   "Edu. Low", "Edu. Medium", "Edu. High",
#             #   "Age Squared", "Age, in months",
#             #   "Unlikely", "Likely", "Non-employed",
#             #   "Fut. Fin. 'Worse off'", "Fut. Fin. 'Better off'",
#             #   "Pres. Fin. 'Finding it very difficult'","Pres. Fin. 'Finding it quite difficult'", "Pres. Fin. 'Just getting by'","Pres. Fin. 'Doing alright'", "PJI"),
#             axis.lim = c(0.1, 5.0),
#             #colors  = c("#2E9FDF", "#E7B800"),
#             p.shape = TRUE)
# 
#             grid = TRUE)


#Splined regression test
#there is something wrong with this, doesn't work. Maybe doesn't meet PH Assumption?
coxphpsp<- coxph(formula = Surv(time1, time2, event) ~ sex + se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat + pspline(jbsec2), data = spsurv, cluster = pidp, method = "breslow")
summary(coxphpsp)
cox.zph(coxphpsp)



#this allows for mixed-effects modeling. The variable in the bracket (this case jbsec) is used to create the mixed effects (partial pooling effect)
coxme <- coxme(formula = Surv(time1, time2, event) ~ sex + se_ee + agemn + agesq + finnow.imp + finfut.imp + edu_cat + (1 | jbsec), data = spsurv)
summary(coxme)
  

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

couplecoxph <- coxph(formula = Surv(time1, time2, event) ~ se_ee + se_eep + finnow.imp + finfut.imp +  finnow.impp + finfut.impp, data = fcouplesurv, cluster = pidp, method = "breslow")
summary(couplecoxph)
summary(fcoxph)

htmlreg(list(couplecoxph),
        file = "couple.html",
        single.row = TRUE,
        # custom.model.names = c("Female", "Female", "Male", "Male"),
        # custom.coef.names = c("PJI", "Doing alright", "Just getting by", "Finding it quite difficult","Finding it very difficult",
        #                       "Better off", "Worse off",
        #                       "Non-employed", "Likely to lose job", "Unlikely to lose job", 
        #                       "Age, in months", "Age Squared", 
        #                       "Edu. High", "Edu. Medium", "Edu. Low", "Single", "Cohab - employed", "Cohab - non-employed",
        #                       "Married - employed", "Married - non-employed", "Married - unknown"),
        # groups = list("Previous Joblessness" = 1:1, "Present Financial Outlook (Ref = Living comfortably)" = 2:5, "Future Financial Outlook (Ref = About the same)" = 6:7,
        #               "Job Security" = 8:10, "Controls" = 11:12, "Education (Ref = Edu. Other)" = 13:15, "Partnership, partner job status (Ref = Cohab - unknown)" = 16:21),
        bold = 0.05,
        caption = "test")

plot_model(couplecoxph, 
            title = "Hazard Ratios",
            #m.labels = c("Men", "Women"),
            #legend.title = "Model",
            # axis.labels = c(
            #   "Married - unknown", "Married - non-employed","Married - employed",
            #   "Cohab - non-employed", "Cohab - employed","Single",
            #   "Edu. Low", "Edu. Medium", "Edu. High",
            #   "Age Squared", "Age, in months",
            #   "Job security",
            #   "Fut. Fin. 'Worse off'", "Fut. Fin. 'Better off'",
            #   "Pres. Fin. 'Finding it very difficult'","Pres. Fin. 'Finding it quite difficult'", "Pres. Fin. 'Just getting by'","Pres. Fin. 'Doing alright'", "PJI"),
            axis.lim = c(0.3, 4.0),
            show.p = TRUE,
            dot.size = 6,
            #colors  = c("#2E9FDF", "#E7B800"), #in case you wanna change to the gold blue set
            p.shape = TRUE)
            #grid = TRUE)
