#Coded by: Brian Buh
#Started on: 03.03.2021
#Last Updated: 11.03.2021

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
  geom_bar(position = "fill")+
  theme(aspect.ratio = 1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) +
  labs(fill = "finnow") +
  ylab("Percentage")+
  xlab("Wave*")+ 
  # xlab("Persistent Joblessness Index: 1 = Continuously Jobless") + 
  ggtitle("Present Financial Situation", subtitle =  "Sample - waves 1 to 10") +
  # facet_wrap(~sex) +
  labs(caption = "Conducted annually; Wave 1: 2009-2011") +
  ggsave("finnow_surv.png")

surv %>% 
  ggplot(aes(wave, fill = finfut.imp)) +
  geom_bar(position = "fill")+
  theme(aspect.ratio = 1) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) +
  labs(fill = "finfut") +
  ylab("Percentage")+
  xlab("Wave*")+ 
  # xlab("Persistent Joblessness Index: 1 = Continuously Jobless") + 
  ggtitle("Future Financial Situation", subtitle =  "Sample - waves 1 to 10") +
  # facet_wrap(~sex) +
  labs(caption = "Conducted annually; Wave 1: 2009-2011") +
  ggsave("finfut_surv.png")

spsurv %>% 
  filter(wave == 2 | wave == 4 | wave == 6 | wave == 8 | wave == 10) %>% 
  mutate(jbsec = recode(jbsec,
                        "3 non-employed" = "Non-employed",
                        "1 likely" = "Likely",
                        "2 unlikely" = "Unlikely")) %>%
  mutate(jbsec = fct_relevel(jbsec, c( "Likely", "Unlikely", "Non-employed"))) %>% 
  ggplot(aes(wave, fill = jbsec)) +
  geom_bar(position = "fill")+
  # geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2) +
  theme(aspect.ratio = 1) +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) +
  labs(fill = "jbsec") +
  ylab("Percentage")+
  xlab("Wave")+ 
  # xlab("Persistent Joblessness Index: 1 = Continuously Jobless") + 
  ggtitle("Perceived Job Security", subtitle =  "Sample - waves 1 to 10") +
  # facet_wrap(~sex) +
  ggsave("jbsec_surv.png")


# Descriptive stats -------------------------------------------------------

#This is done to look at ending wave stats
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

#Looking at the level of uncertainty experienced
#Categorical variables are changed to numeric

#I start by reversing the scale of finnow
substat <- surv %>% 
  mutate(finnow.imp = fct_relevel(finnow.imp, c("5 Finding it very difficult", "4 Finding it quite difficult",
                                                "3 Just getting by", "2 Doing alright", "1 Living comfortably"))) %>%
  mutate(finnow.num = as.numeric(finnow.imp)) %>% 
  #I change the scale of finfut to be centered at 0
  mutate(finfut.imp = fct_relevel(finfut.imp, c( "Worse off", "About the same", "Better off"))) %>% 
  mutate(finfut.num = as.numeric(finfut.imp)) %>% 
  mutate(finfut.num = recode(finfut.num,
                             "2" = "0",
                             "1" = "-1",
                             "3" = "1")) %>% 
  mutate(finfut.num = as.integer(finfut.num))

str(substat)

sumsubstat <- substat %>% 
  group_by(pidp) %>% 
  summarize(meanfinnow = mean(finnow.num), meanfinfut = mean(finfut.num)) %>% 
  left_join(., statsurv, by = "pidp") %>% 
  mutate(gor_dv = as.character(gor_dv)) %>% 
  #Create categories for ethnicity based on Kulu&Hannemann2016
  mutate(ethnic = ifelse(racel_dv == 1, 1, #english, scottish, welsh, ni
                         ifelse(racel_dv == 2 | racel_dv == 3 | racel_dv == 4, 2, #other white
                                ifelse(racel_dv == 9, 3, #indian
                                       ifelse(racel_dv == 10, 4, #pakistani
                                              ifelse(racel_dv == 11, 5, #bangladeshi
                                                     ifelse(racel_dv == 14, 7, #carribean
                                                            ifelse(racel_dv == 12 | racel_dv == 13, 6, #other asian
                                                                   ifelse(racel_dv == 15, 8, 9))))))))) %>% #african or other
  mutate(ethnic = as.character(ethnic)) %>%  
  mutate(ethnic = recode(ethnic,
                        "1" = "UK",
                        "2" = "Other White",
                        "3" = "Indian",
                        "4" = "Pakistani",
                        "5" = "Bangladeshi",
                        "6" = "Other Asian",
                        "7" = "Caribbean",
                        "8" = "African",
                        "9" = "Mixed/Other")) %>%
  mutate(ethnic = fct_relevel(ethnic, c( "UK",
                                         "Other White",
                                         "Indian",
                                         "Pakistani",
                                         "Bangladeshi",
                                         "Other Asian",
                                         "Caribbean",
                                         "African",
                                         "Mixed/Other"))) %>% 
  mutate(gen = ifelse(generation == 1, 1, 2)) %>% #Creates a binary of immigrants versus born UK
  mutate(gen = recode(gen,
                         "1" = "First Generation",
                         "2" = "UK Born")) %>% 
  mutate(gen = as.character(gen)) %>% 
  unite(genethnic, ethnic, gen,  sep = "-", remove = FALSE) %>% 
  unite(sexethnic, sex, ethnic, sep = "-", remove = FALSE) %>% 
  unite(ethnicsex, ethnic, sex, sep = "-", remove = FALSE) %>% 
  mutate(gor_dv = recode(gor_dv,
                         "1" = "North East",
                         "2" = "North West",
                         "3" = "Yorkshire and Humberside",
                         "4" = "East Midlands",
                         "5" = "West Midlands",
                         "6" = "East of England",
                         "7" = "London",
                         "8" = "South East",
                         "9" = "South West",
                         "10" = "Wales",
                         "11" = "Scotland",
                         "12" = "Northern Ireland",
                         "-9" = "missing"))


str(sumsubstat)
sumsubstat %>% 
  count(genethnic)

sumsubstat %>%   
  mutate(fbyes = ifelse(is.na(kdob), 0, 1)) %>%
  mutate(fbyes = recode(fbyes,
                        "0" = "No birth",
                        "1" = "First birth")) %>%
  mutate(fbyes = as.character(fbyes)) %>% 
  ggplot(aes(meanfinnow, fill = fbyes)) +
  geom_histogram(binwidth = 0.25) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(caption = "5 = Continuously 'Living comfortably'") +
  xlab("Present Financial Situation") + 
  ggtitle("Mean of Present Financial Sitaution", subtitle =  "UKHLS Waves 1-10") +
  facet_wrap(~sex) +
  ggsave("meanfinnow_hist.png")

sumsubstat %>%   
  mutate(fbyes = ifelse(is.na(kdob), 0, 1)) %>%
  mutate(fbyes = recode(fbyes,
                        "0" = "No birth",
                        "1" = "First birth")) %>%
  mutate(fbyes = as.character(fbyes)) %>% 
  ggplot(aes(meanfinfut, fill = fbyes)) +
  geom_histogram(binwidth = 0.25) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(caption = "Centered around 0 = 'About the same'") +
  xlab("Future Financial Situation") + 
  ggtitle("Mean of Future Financial Sitaution", subtitle =  "UKHLS Waves 1-10") +
  facet_wrap(~sex) +
  ggsave("meanfinfut_hist.png")
  
mycontrols <- tableby.control(test = FALSE)
ethnic <-arsenal::tableby(ethnic ~ fb + se_ee + meanfinnow + meanfinfut, data = sumsubstat, control = mycontrols)
labels(ethnic) <-  c(fb = "Entered Parenthood", se_ee = "PJI", meanfinnow = "Present Financial Outlook", meanfinfut = "Future Financial Outlook")
                        # jbsec = "Job Security", edu_cat = "Educational Attainment", combo = "Partnership, Partner's Job Status")
summary(ethnic, text = TRUE)
# as.data.frame(meanstats)
# write2word(meanstats , "meanstats.doc")
write2html(ethnic, "ethnic.html")

genethnic <-arsenal::tableby(genethnic ~ sex +fb + se_ee + meanfinnow + meanfinfut, data = sumsubstat, control = mycontrols)
labels(genethnic) <-  c(sex = "Sex", fb = "Entered Parenthood", se_ee = "PJI", meanfinnow = "Present Financial Outlook", meanfinfut = "Future Financial Outlook")
# jbsec = "Job Security", edu_cat = "Educational Attainment", combo = "Partnership, Partner's Job Status")
summary(genethnic, text = TRUE)
# as.data.frame(meanstats)
# write2word(meanstats , "meanstats.doc")
write2html(genethnic, "genethnic.html")

sexethnic <-arsenal::tableby(ethnicsex ~ gen + fb + se_ee + meanfinnow + meanfinfut, data = sumsubstat, control = mycontrols)
labels(sexethnic) <-  c(gen = "Generation", fb = "Entered Parenthood", se_ee = "PJI", meanfinnow = "Present Financial Outlook", meanfinfut = "Future Financial Outlook")
# jbsec = "Job Security", edu_cat = "Educational Attainment", combo = "Partnership, Partner's Job Status")
summary(sexethnic, text = TRUE)
# as.data.frame(meanstats)
# write2word(meanstats , "meanstats.doc")
write2html(sexethnic, "sexethnic.html")
write2pdf(sexethnic, "sexethnic.pdf")

sumsubstat %>% 
  count(generation)
  
###########################################################################
# Look at relationships by subpopulation ----------------------------------
###########################################################################


subpop <- sumsubstat %>% 
  group_by(ethnic, sex) %>% 
  summarise(meanfinnow = mean(meanfinnow), meanfinfut = mean(meanfinfut), meanpji = mean(se_ee), meanfb = mean(fb))

subpop %>% 
  ggplot(aes(meanpji, meanfinfut)) +
  geom_point(aes(color = factor(ethnic), shape = factor(sex), size = 4)) +
  geom_smooth(method = "lm") +
  # geom_point(colour = "grey90", size = 1.5) +
  scale_color_brewer(palette = "Set1") +
  xlab("Persistent Joblessness Index") +
  ylab("Future Financial Outlook")+
  labs(shape = "Sex", color = "Ethnicity", caption = "PJI scaled from 0-1, 1 represents continuously jobless") +
  ggtitle("Perceived Job Security and Future Financial Outlook", subtitle =  "Childless Respodents Finished with Education under 45 (Women) or 50 (Men)")+ 
  ggsave("pji_finfut_ethnic.png")

subpop %>% 
  ggplot(aes(meanpji, meanfb)) +
  geom_point(aes(color = factor(ethnic), shape = factor(sex)), size = 6) +
  geom_smooth(method = "lm", se = TRUE) +
  # geom_point(colour = "grey90", size = 1.5) +
  scale_color_brewer(palette = "Set1") +
  xlab("Persistent Joblessness Index") +
  ylab("Average Observed Rate of Entering Parenthood")+
  labs(shape = "Sex", color = "Ethnicity", caption = "PJI scaled from 0-1, 1 represents continuously jobless") +
  ggtitle("Persistent Joblessness and First Birth by Ethnicity and Sex", subtitle =  "Childless Respondents Finished with Education under 45 (Women) or 50 (Men)") + 
  ggsave("pji_fb_ethnic.png")

gensubpop <- sumsubstat %>% 
  group_by(ethnic, gen) %>% 
  summarise(meanfinnow = mean(meanfinnow), meanfinfut = mean(meanfinfut), meanpji = mean(se_ee), meanfb = mean(fb)) %>% 
  mutate(gen = recode(gen,
                        "1" = "First Generation",
                        "2" = "UK Born")) 

gorsubpop <- sumsubstat %>% 
  group_by(gor_dv, sex) %>% 
  summarise(meanfinnow = mean(meanfinnow), meanfinfut = mean(meanfinfut), meanpji = mean(se_ee), meanfb = mean(fb)) %>% 
  mutate(gor_dv = recode(gor_dv,
                         "1" = "North East",
                         "2" = "North West",
                         "3" = "Yorkshire and Humberside",
                         "4" = "East Midlands",
                         "5" = "West Midlands",
                         "6" = "East of England",
                         "7" = "London",
                         "8" = "South East",
                         "9" = "South West",
                         "10" = "Wales",
                         "11" = "Scotland",
                         "12" = "Northern Ireland",
                         "-9" = "missing"))

geostat <-arsenal::tableby(gor_dv ~ sex + fb + se_ee + meanfinnow + meanfinfut, data = sumsubstat, control = mycontrols)
labels(geostat) <-  c(sex = "Sex", fb = "Entered Parenthood", se_ee = "PJI", meanfinnow = "Present Financial Outlook", meanfinfut = "Future Financial Outlook")
# jbsec = "Job Security", edu_cat = "Educational Attainment", combo = "Partnership, Partner's Job Status")
summary(geostat, text = TRUE)
# as.data.frame(meanstats)
# write2word(meanstats , "meanstats.doc")
write2html(geostat, "geostat.html")
  
  
gensubpop %>% 
  ggplot(aes(meanpji, meanfb)) +
  geom_point(aes(color = factor(ethnic), shape = factor(gen)), size = 6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values=c(15, 18)) +
  # theme_bw() +
  xlab("Persistent Joblessness Index") +
  ylab("Average Observed Rate of Entering Parenthood")+
  labs(shape = "Generation", color = "Ethnicity", caption = "PJI scaled from 0-1, 1 represents continuously jobless") +
  ggtitle("Persistent Joblessness and First Birth by Ethnicity and Generation", subtitle =  "Childless Respondents Finished with Education under 45 (Women) or 50 (Men)") + 
  ggsave("pji_fb_gen.png")

#Attempt to look at histogram of different ethnic groups PJI
# sumsubstat %>% 
#   filter(ethnic != "UK") %>% 
#   ggplot(aes(se_ee, fill = fb)) +
#   geom_histogram(binwidth = 0.05) +
#   scale_fill_brewer(palette = "Dark2") +
#   theme(aspect.ratio = 1) +
#   labs(fill = "Sex", caption = "Not shown, 0 = continuously employed") +
#   xlab("Persistent Joblessness Index: 1 = Continuously Jobless") + 
#   ggtitle("Truncated Persistent Joblessness Index", subtitle =  "UKHLS = Measured 9 months before first birth") +
#   facet_wrap(~ethnic)

surv %>% 
  count(imp)

sumsubstat %>%
  group_by(gor_dv) %>%
  summarise(count = n() / nrow(.) )


sumsubstat %>% 
ggplot(aes(wave, fill = finfut.imp)) +
  geom_bar(position = "fill")+
  # geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2) +
  theme(aspect.ratio = 1) +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) +
  labs(fill = "jbsec") +
  ylab("Percentage")+
  xlab("Wave")+ 
  # xlab("Persistent Joblessness Index: 1 = Continuously Jobless") + 
  ggtitle("Perceived Job Security", subtitle =  "Sample - waves 1 to 10") +
  facet_wrap(~ethnic) 

sumsubstat %>% 
    mutate(age = agemn/12) %>%
    ggplot(aes(age)) +
    geom_histogram(binwidth = 0.5) +
  facet_wrap(~sex)
