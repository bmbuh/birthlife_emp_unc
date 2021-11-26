#Coded by: Brian Buh
#Started on: 23.08.2021
#Last Updated: 10.11.2021

# install.packages("cowplot")
# install.packages("inflection")

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

library(lubridate)
library(survminer) #for KM Curve printing
library(survPen)
library(flexsurv)
library(coxme)
library(stargazer)
library(texreg)
library(forestplot)
library(sjPlot)
library(cowplot)
library(inflection)



# Load DF -----------------------------------------------------------------
# From script 3-3
# Already cuts off for the first three years only
# pji5 <- file.choose()
# pji5 <- readRDS(pji5)
# 
# 
# 
# pji6 <- pji5 %>% 
#   mutate(empstat = ifelse(status <= 2 | status == 100, "Employed", 
#                           ifelse(status == 3, "Unemployed",
#                                  ifelse(status == 7, "Full-Time Student",
#                                         ifelse(status == 9 | status == 11, "Gov't Training Scheme/Appretinceship",
#                                                ifelse(status == 103, "National Service", "Inactive"))))))
# 
# pji6 %>% count(empstat)
# 
# #Attempt to make a figure that shows the variation in status for the first three years post-education
# #Does not actually display much variation and is relatively uninteresting
# pji6 %>% 
#   ggplot(aes(x = num, fill = empstat)) +
#   geom_bar(position = 'fill')


###########################################################################
# sample Description ------------------------------------------------------
###########################################################################

#Number of unique respondents
statsurv6 <- surv6 %>%
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ungroup() 

statsurv6 %>% count(sex)
#Women = 4925, Men = 5926

#Seeing individuals who changed educational level during period
testedu <- surv6 %>% 
  select(pidp, wave, edu) %>%
  group_by(pidp) %>%
  mutate(numedu = n_distinct(edu)) %>%
  ungroup()

testedu %>%
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>%
  ungroup() %>%
  count(numedu)


summary(surv6$pji3)
meanpjim <- surv6 %>% filter(sex==1) %>% summarize(mean(pji3))
meanpjif <- surv6 %>% filter(sex==2) %>% summarize(mean(pji3))
meanpjifb <- surv6 %>% filter(event==1) %>% summarize(mean(pji3))
meanpjinb <- surv6 %>% filter(event==0) %>% summarize(mean(pji3))

#full sample
summary(surv5$se_ee)

surv6mfb <- surv6 %>% filter(sex == 1, event == 1)
surv6ffb <- surv6 %>% filter(sex == 2, event == 1)

summary(surv6mfb$agebirth)
summary(surv6ffb$agebirth)
summary(surv6mfb$t2)
summary(surv6ffb$t2)

# Full Sample -------------------------------------------------------------

statsurv6 <- surv6 %>% 
  group_by(pidp) %>% 
  mutate(time = row_number()) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(fb = ifelse(is.na(kdob), 0, 1)) %>% 
  mutate(fb = as.factor(fb)) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  # mutate(edu = fct_relevel(edu, c("high", "medium", "low"))) %>% 
  ungroup() %>% 
  unite(edusex, edu, sex, sep = "-", remove = FALSE) %>%
  select(pidp, time, rev_time, edusex, sex, se_ee, pji3, event, fb)

statsurv5 <- surv5 %>% 
  group_by(pidp) %>% 
  mutate(time = row_number()) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(fb = ifelse(is.na(kdob), 0, 1)) %>% 
  mutate(fb = as.factor(fb)) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  # mutate(edu = fct_relevel(edu, c("high", "medium", "low"))) %>% 
  ungroup() %>% 
  unite(edusex, edu, sex, sep = "-", remove = FALSE) %>%
  select(pidp, time, rev_time, edusex, sex, se_ee)

#Mean of PJI for Section 3.4
statsurv6 %>% summarise(pji3 = mean(pji3))
statsurv5 %>% summarise(se_ee = mean(se_ee))
statsurv6 %>% group_by(sex) %>% summarise(pji3 = mean(pji3))
statsurv6 %>% group_by(event) %>% summarise(pji3 = mean(pji3))

summary(surv6$pji3)
meanpjim <- surv6 %>% filter(sex==1) %>% summarize(mean(pji3))
meanpjif <- surv6 %>% filter(sex==2) %>% summarize(mean(pji3))
meanpjifb <- surv6 %>% filter(event==1) %>% summarize(mean(pji3))
meanpjinb <- surv6 %>% filter(event==0) %>% summarize(mean(pji3))

summary(surv5$se_ee) #full sample

#Ever report difficult financial situation
statsurv2 <- surv6 %>% 
  filter(finnow3cat == "finddifficult") %>% 
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ungroup() 

statsurv2 %>% count(sex)

#Ever reported "Worse off"
statsurv3 <- surv6 %>% 
  filter(finfut.imp == "Worse off") %>% 
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ungroup() 

statsurv3 %>% count(sex)

#Ever reported not employed
statsurv4 <- surv6 %>% 
  filter(employed == 0) %>% 
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ungroup() 

statsurv4 %>% count(sex)

# Descrptive Stats: First Table Attempt
# mycontrols <- tableby.control(test = FALSE)
# fullstats <-arsenal::tableby(edusex ~ t2 + fb  + pji3 + finnow3cat + finfut.imp + employed + combo, data = statsurv, control = mycontrols)
# labels(fullstats) <-  c(t2 = "Time since end of education (months)", sex = "Sex", pji3 = "PJI", employed = "Employed",
#                         finnow3cat = "Present Finacial", finfut.imp = "Future Finacial", edu = "Educational Attainment")
# summary(fullstats)
# write2html(fullstats , "fullstats_surv6_18-10-2021.html") 

# Table using Arsenal
mycontrols <- tableby.control(test = FALSE)
fullstats <-arsenal::tableby(edusex ~ pji3 + finnow3cat + finfut.imp + employed + cohort2 + immigrant + combo, data = surv6, control = mycontrols)
labels(fullstats) <-  c(t2 = "Time since end of education (months)", sex = "Sex", pji3 = "PJI", employed = "Employed",
                        finnow3cat = "Present Finacial", finfut.imp = "Future Finacial", edu = "Educational Attainment")
summary(fullstats)
write2html(fullstats , "fullstats_surv6_04-11-2021.html") 
write2word(fullstats , "fullstats_surv6_04-11-2021.docx") 

# Table using Arsenal
mycontrols <- tableby.control(test = FALSE)
empstats <-arsenal::tableby(edusex ~ pji3 + finnow3cat + finfut.imp + employed + jbsec.dummy + permcon + parttime + cohort2 + immigrant + combo, data = survemp3, control = mycontrols)
labels(empstats) <-  c(t2 = "Time since end of education (months)", sex = "Sex", pji3 = "PJI", employed = "Employed",
                        finnow3cat = "Present Finacial", finfut.imp = "Future Finacial", edu = "Educational Attainment")
summary(empstats)
write2html(empstats , "empstats_surv6_04-11-2021.html") 
write2word(empstats , "empstats_surv6_04-11-2021.docx") 

# Number of babies for men over 45

over45 <- surv6 %>% filter(sex == 1, dvage >= 45) %>% count(event)

surv6 %>% count(event)
survemp3 %>% count (event)

# -----------------------------------------------------------------------------
# Correlation Education and Finnow -------------------------------------------
# -----------------------------------------------------------------------------


cor.test(surv6$edu, surv6$finnow3cat, method = c("pearson"))


### Chi-squared test
chisq <- chisq.test(surv6$edu, surv6$finnow3cat)
chisq$observed
round(chisq$expected,3)
library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)
#This clearly shows that High education is positively correlated with "doingfine"
#Low education is positively correlated with "finddifficult" and "getby"

# Contibution in percentage (%)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
# printing the p-value
chisq$p.value
# printing the mean
chisq$estimate

# -----------------------------------------------------------------------------
# Employed Sample -------------------------------------------------------------
# -----------------------------------------------------------------------------

statemp <- survemp %>% 
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(fb = ifelse(is.na(kdob), 0, 1)) %>% 
  mutate(fb = as.factor(fb)) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  mutate(edu = fct_relevel(edu, c("high", "medium", "low"))) %>% 
  ungroup()

statemp %>% count(jbsec.dummy)

statemp2 <- survemp %>% 
  filter(jbsec.dummy == 1) %>% 
  group_by(pidp) %>% 
  arrange(pidp, desc(wave)) %>% 
  mutate(rev_time = row_number()) %>% 
  filter(rev_time == 1) %>% 
  mutate(fb = ifelse(is.na(kdob), 0, 1)) %>% 
  mutate(fb = as.factor(fb)) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ungroup()

statemp2 %>% count(sex)

survemp %>% count(permcon, jbsec.dummy)
survemp %>% count(parttime, jbsec.dummy)

###########################################################################
# PJI Graphics ------------------------------------------------------------
###########################################################################

#Histogram of PJI of sample (NOT truncated)
surv6 %>% 
  group_by(pidp) %>% 
  mutate(intnum = row_number()) %>% 
  ungroup() %>% 
  mutate(fbyes = ifelse(is.na(kdob), 0, 1)) %>% 
  mutate(fbyes = recode(fbyes,
                        "0" = "No birth",
                        "1" = "First birth")) %>% 
  mutate(sex = as.factor(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  dplyr::filter(intnum == 1) %>% 
  ggplot(aes(se_ee, fill = fbyes)) +
  geom_histogram(binwidth = 0.03) +
  # annotate("text", x=.15, y=5000, size = 6, label= "First Births are concentrated here") +
  annotate("text", x=.25, y=2000, size = 6, label= "0 = No Jobless Spells") +
  annotate("text", x=.8, y=1200, size = 6, label= "1 = Completely Jobless") +
  scale_fill_manual(values = c("#8FB339", "#3A2D32")) +
  theme_minimal()+
  theme(legend.position = c(.8,.8), plot.title = element_text(size = 18),
        axis.title.x = element_text(size = 18, vjust=-1), axis.title.y = element_text(size = 18), 
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(fill = "Births") +
  ggtitle("Distribution of Persistent Joblessness Index for first 3 years post-Education") +
# , subtitle =  "Combined duration, number and proximity of jobless spells - UKHLS Waves 1-10") +
  xlab("Persistent Joblessness Index") +
  ylab("Count") +
  ggsave("paper_pji_figure_08-11-21.png", dpi = 1000)

###########################################################################
# Early-career PJI Robustness Graphic -------------------------------------
###########################################################################

pji_event <- read.csv("S:/drafts_paper_1/pji_events_data.csv")

pji_event <- pji_event %>% select(time, time2, mean, sd, events, year)

meanpji <- pji_event %>% 
  ggplot(aes(x = time, y = mean, group = 1)) +
  geom_point() +
  geom_path() +
  # annotate("text", x=.15, y=5000, size = 6, label= "First Births are concentrated here") +
  # annotate("text", x=.25, y=2000, size = 6, label= "0 = No Jobless Spells") +
  # annotate("text", x=.8, y=1200, size = 6, label= "1 = Completely Jobless") +
  # scale_fill_manual(values = c("#8FB339", "#3A2D32")) +
  theme_minimal()+
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_text(size = 18, vjust=-1), axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  ggtitle("Mean Persistent Joblessness Index",subtitle = "6-month starting point intervals post-Education") +
  xlab("Years post-education") +
  ylab("Mean PJI") +
  ggsave("paper1_pji_per_year_10-11-21.png", dpi = 1000)

#Finding inflection point (package "inflection")
pji_event2 <- pji_event %>% filter(time != "Full") %>% mutate(time = as.numeric(time))
cc <- check_curve(pji_event2$time, pji_event2$mean)
cc2 <- bede(pji_event2$time, pji_event2$mean, cc$index)
structure(cc2$iplast)
findipiterplot(pji_event2$time, pji_event2$mean, cc$index, plots = TRUE, ci = FALSE, doparallel = FALSE)
knee=d2uik(pji_event2$time, pji_event2$mean)
knee


pji_event2 %>% 
  ggplot(aes(x = time, y = mean))+
  geom_smooth() +
  geom_point() + 
  geom_vline(xintercept = cc2$iplast) + 
  labs(y = "test")

# plot(pji_event2$time, pji_event2$mean,type="l")
lo <- loess(pji_event2$mean ~ pji_event2$time)
xl <- seq(min(pji_event2$time),max(pji_event2$time), (max(pji_event2$time) - min(pji_event2$time))/1000)
out = predict(lo,xl)
check_curve(out$time, out$mean)
lines(xl, out, col='red', lwd=2)
# abline(v = cc2$iplast, col= 'blue')
# 
# infl <- c(FALSE, diff(diff(out)>0)!=0)
# points(xl[infl ], out[infl ], col="blue")

fbevents <- pji_event %>% 
  mutate(time2 = as.character(time)) %>%
  mutate(time2 = fct_relevel(time2, c("Full", "1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5"))) %>%
  ggplot(aes(x = time2, y = events, group = 1)) +
  geom_point() + 
  geom_line () +
  theme_minimal()+
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_text(size = 18, vjust=-1), axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  ggtitle("First birth events",subtitle = "6-month starting point intervals post-Education") +
  # , subtitle =  "Combined duration, number and proximity of jobless spells - UKHLS Waves 1-10") +
  xlab("Years post-education") +
  ylab("Count of first birth events") +
  ggsave("paper1_events_per_starting_point_10-11-21.png", dpi = 1000)

#Uses the cowplot package
combined <- plot_grid(meanpji, fbevents, labels = "AUTO")
ggsave("combined_meanpji_fbevents_10-11-21.png", dpi = 1000)
#Save function from cowplot, doesn't work as well as ggsave
# save_plot("combined2_meanpji_fbevents_10-11-21.pdf", combined, ncol = 2)

###########################################################################
# Risk of event by months since end of education --------------------------
###########################################################################

#Taken from Script 10. Needs to be updated to reflect current educational distribution
ageedu <- surv5 %>% 
  dplyr::select(sex, agemn, event, edu, t2, se_ee) %>% 
  mutate(edu = fct_relevel(edu, c("high", "medium", "low"))) %>% 
  mutate(age = agemn/12) %>% 
  filter(event == 1) %>% 
  arrange(age) %>% 
  group_by(age) %>% 
  add_tally() %>% 
  mutate(sex = as.factor(sex)) %>% 
  ungroup()

ageedu %>% 
  count(edu)



###Months since end of education
#Number of months since end of education until first birth (Smooth)
ageedu %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women",)) %>%
  ggplot(aes(x = t2, y= n, group = edu, color = edu)) +
  geom_smooth() +
  # geom_label(group_by(age) %>% filter(x == max(x)), aes(label = sprintf('%0.2f', y)), hjust = -0.5) +
  ylim(0, 17) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(color = "Educational Attainment") +
  xlab("Months since end of formal education") + 
  ylab("Count of number of first births in specific month since end of education") +
  ggtitle("Number of Months from End of Education to First birth", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) +
  ggsave("time_endedu_fb.png")

#Number of months since end of education until first birth (Bar)
ageedu %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women",)) %>%
  ggplot(aes(x = t2, fill = edu)) +
  geom_bar() +
  # geom_label(group_by(age) %>% filter(x == max(x)), aes(label = sprintf('%0.2f', y)), hjust = -0.5) +
  ylim(0, 17) +
  # scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Educational Attainment") +
  xlab("Months since end of formal education") + 
  ylab("Count of number of first births in specific month since end of education") +
  ggtitle("Number of Months from End of Education to First birth", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) 

#Number of months since end of education until first birth (Histogram)
#Changed from months to years to better see the differences
ageedu %>% 
  mutate(endeduyear = t2/12) %>% 
  arrange(endeduyear) %>% 
  mutate(endeduyear = round(endeduyear)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women",)) %>%
  ggplot(aes(x = endeduyear, fill = edu)) +
  geom_histogram(binwidth = 1) +
  # geom_label(group_by(age) %>% filter(x == max(x)), aes(label = sprintf('%0.2f', y)), hjust = -0.5) +
  # ylim(0, 17) +
  # scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Educational Attainment") +
  xlab("Years since end of formal education") + 
  ylab("Count of number of first births in years (rounded)") +
  ggtitle("Years from End of Education to First Birth", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) +
  ggsave("time_endedu_fb_hist.png")

#Education and Time Model Fit
surv5 %>%
  group_by(t2, edu) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  # filter(event ==1) %>% 
  ggplot(aes(x = t2,
             y = log(-log(1-hazard)),
             col = edu)) +
  geom_point() +
  geom_smooth() +
  ylab("log(-log(1 - hazard)))") +
  xlab("Months since end of formal education") +
  ggsave("flm_hazard_edu_31-08-2021.png")

# ------------------------------------------------------------------------------
#Kaplan-Meier non-parametric analysis ------------------------------------------
# ------------------------------------------------------------------------------

km <- surv6 %>% 
  mutate(edu = fct_relevel(edu, c("high", "medium", "low")))

# This plot shows the survival curves stratified by education
kmsurv <- survfit(Surv(t1, t2, event) ~ strata(edu), data = km, cluster = pidp)
summary(kmsurv)
plot(kmsurv, xlab = "Time in months since End of Education", ylab = "First Birth Probability by Education")
ggsurvplot(kmsurv, size = 1, 
           xlim = c(50, 510),
           # change line size
           ylim = c(0.25,1),
           palette = c("#264653", "#2a9d8f", "#e9c46a"),# custom color palettes
           conf.int = TRUE,            # Add confidence interval
           # pval = TRUE,              # Add p-value
           risk.table = TRUE,          # Add risk table
           # risk.table.col = "strata",  # Risk table color by groups
           legend.labs = c("High", "Medium", "Low"),    # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           title = "Kaplan-Meier non-parametric analysis",
           legend.title = "Education") 


 # + labs(caption = "Survival probability cut at 0.3")


# This plot shows the survival curves stratified by education and sex
kmsurv2 <- survfit(Surv(t1, t2, event) ~ strata(edu, sex), data = km, cluster = pidp)
summary(kmsurv2)
plot(kmsurv2, xlab = "Time in months since End of Education", ylab = "First Birth Probability by Education")
ggpar(ggsurvplot, font.legend = size (14))
km_edusex<- ggsurvplot(kmsurv2, size = 1, #you must save the plot as a variable for ggsave to work
           surv.scale = "percent",
           # linetype = c("strata"),
           # linetype = "strata",
           xlim = c(58, 540),
           xscale = 12,
           break.time.by = 36,
           # change line size
           ylim = c(0.25,1),
           palette = c("#000000", "#707070", "#1E7167", "#6ED8CC", "#B67102", "#F0C57F"),# custom color palettes (It goes HM, HF, MM, MW, LM, LW)
           # conf.int = TRUE,            # Add confidence interval
           # pval = TRUE,              # Add p-value
           # risk.table = TRUE,          # Add risk table
           # # risk.table.col = "strata",  # Risk table color by groups
           legend.labs = c("High Men", "High Women", "Medium Men", "Medium Women", "Low Men", "Low Women"),
           font.legend = c(14),
           # Change legend labels
           # risk.table.height = 0.25, # Useful to change when you have multiple groups
           # ggtheme = theme_bw(),      # Change ggplot2 theme
           # legend.title = "Education",
           title = "Kaplan-Meier non-parametric analysis",
           font.title = c(20),
           xlab = "Time in years since end of education",
           ylab = "Remain childless probability")

print(km_edusex)
ggsave(file = "km_edusex_08-11-21.png", km_edusex, dpi = 1000)


