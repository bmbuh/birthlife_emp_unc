#Coded by: Brian Buh
#Started on: 23.08.2021
#Last Updated: 25.08.2021


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


# Load DF -----------------------------------------------------------------
# From script 3-3
# Already cuts off for the first three years only
pji5 <- file.choose()
pji5 <- readRDS(pji5)



pji6 <- pji5 %>% 
  mutate(empstat = ifelse(status <= 2 | status == 100, "Employed", 
                          ifelse(status == 3, "Unemployed",
                                 ifelse(status == 7, "Full-Time Student",
                                        ifelse(status == 9 | status == 11, "Gov't Training Scheme/Appretinceship",
                                               ifelse(status == 103, "National Service", "Inactive"))))))

pji6 %>% count(empstat)

#Attempt to make a figure that shows the variation in status for the first three years post-education
#Does not actually display much variation and is relatively uninteresting
pji6 %>% 
  ggplot(aes(x = num, fill = empstat)) +
  geom_bar(position = 'fill')


###########################################################################
# sample Description ------------------------------------------------------
###########################################################################

# Full Sample -------------------------------------------------------------

statsurv <- surv6 %>% 
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

statsurv %>% count(sex)

#Mean of PJI for Section 3.4
statsurv %>% summarise(pji3 = mean(pji3))
statsurv %>% summarise(se_ee = mean(se_ee))
statsurv %>% group_by(sex) %>% summarise(pji3 = mean(pji3))
statsurv %>% group_by(event) %>% summarise(pji3 = mean(pji3))

#Ever report difficult finacial situation
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

# Table using Arsenal
mycontrols <- tableby.control(test = FALSE)
fullstats <-arsenal::tableby(fb ~ t2 + sex + pji3 + finnow3cat + finfut.imp + employed + edu, data = statsurv, control = mycontrols)
labels(fullstats) <-  c(t2 = "Time since end of education (months)", sex = "Sex", pji3 = "PJI", employed = "Employed",
                        finnow3cat = "Present Finacial", finfut.imp = "Future Finacial", edu = "Educational Attainment")
summary(fullstats)
write2word(fullstats , "fullstats_surv6_21-07-2021.docx") 


#Kaplan-Meier non-parametric analysis
kmsurv_sex <- survfit(Surv(t1, t2, event) ~ strata(sex), data = surv6, cluster = pidp)
summary(kmsurv_sex)
plot(kmsurv_sex, xlab = "Months since end of education", ylab = "First Birth Probability by Sex")
ggsurvplot(kmsurv_sex, size = 1,   # change line size
           # ylim = c(0.69,1),
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

# Employed Sample -------------------------------------------------------------

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
  ylab("Count") +
  ggsave("paper_pji_figure_26-08-21.png")



