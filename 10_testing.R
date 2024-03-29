#Coded by: Brian Buh
#Started on: 15.04.2021
#Last Updated: 25.05.2021


library(tidyverse)
library(haven)
library(effects)


#surv2 has the fixed clock issues plus objective measures
surv2 <- file.choose()
surv2 <- readRDS(surv2)

surv2 %>% 
  count(edu_cat)

###########################################################################
# Testing for clock starting point ----------------------------------------
###########################################################################

ageedu <- surv2 %>% 
  dplyr::select(sex, agemn, event, edu_cat, t2, se_ee) %>% 
  mutate(edu_cat = fct_relevel(edu_cat, c("high", "medium", "low", "other"))) %>% 
  mutate(age = agemn/12) %>% 
  filter(event == 1) %>% 
  arrange(age) %>% 
  group_by(age) %>% 
  add_tally() %>% 
  mutate(sex = as.factor(sex)) %>% 
  ungroup()

ageedu %>% 
  count(edu_cat)


###Months since end of education
#Number of months since end of education until first birth (Smooth)
ageedu %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women",)) %>%
  ggplot(aes(x = t2, y= n, group = edu_cat, color = edu_cat)) +
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
  ggplot(aes(x = t2, fill = edu_cat)) +
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
  ggplot(aes(x = endeduyear, fill = edu_cat)) +
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

###Age
#Age at first birth by education and sex (Smooth)
ageedu %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women",)) %>%
  ggplot(aes(x = age, y= n, group = edu_cat, color = edu_cat)) +
  geom_smooth() +
  # geom_label(group_by(age) %>% filter(x == max(x)), aes(label = sprintf('%0.2f', y)), hjust = -0.5) +
  ylim(0, 17) +
  # scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(color = "Educational Attainment") +
  xlab("Age") + 
  ylab("Count of number of women experiencing first births in age by month") +
  ggtitle("Age at First birth by Educational Attainment", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) +
  ggsave("age_fb_edu_smooth.png")

#Age at first birth by education and sex (Bar)
ageedu %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women",)) %>%
  ggplot(aes(x = age, fill = edu_cat)) +
  geom_bar() +
  # geom_label(group_by(age) %>% filter(x == max(x)), aes(label = sprintf('%0.2f', y)), hjust = -0.5) +
  ylim(0, 17) +
  # scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Educational Attainment") +
  xlab("Age in Months") + 
  ylab("Count of number of women experiencing first births in age by month") +
  ggtitle("Age at First birth by Educational Attainment", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) 

#creating a distribution with age in years
#The histogram does a better job
#the y-limit needs to be changed
ageedu %>% 
  mutate(age = round(age)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women",)) %>%
  ggplot(aes(x = age, fill = edu_cat)) +
  geom_histogram(binwidth = 1) + 
  # geom_label(group_by(age) %>% filter(x == max(x)), aes(label = sprintf('%0.2f', y)), hjust = -0.5) +
  # ylim(0, 17) +
  # scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Educational Attainment") +
  xlab("Age") + 
  ylab("Count of number of women experiencing first births by year (rounded)") +
  ggtitle("Age at First birth by Educational Attainment", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) +
  ggsave("age_fb_edu_hist.png")

#Age at first birth by education and sex (Histogram)
ageedu %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women",)) %>%
  ggplot(aes(x = age, fill = edu_cat)) +
  geom_histogram(binwidth = 0.25) +
  # geom_label(group_by(age) %>% filter(x == max(x)), aes(label = sprintf('%0.2f', y)), hjust = -0.5) +
  ylim(0, 17) +
  # scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Educational Attainment", caption = "histogram with binwidth of 0.25 years") +
  xlab("Age") + 
  ylab("Count of number of women experiencing first births by quarter year") +
  ggtitle("Age at First birth by Educational Attainment", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) 



###########################################################################
# Testing for interaction employment and finnow---------------------------
###########################################################################

#There is an issue here where I need to find if there is an interaction between
#employment and the subjective measure
# Important note: surv3 is current located on script 9
surv3 %>% 
  mutate(employed = recode(employed,
                                    "0" = "non-emp",
                                    "1" = "emp")) %>% 
  with(tapply(event, list(comf, employed), mean))

surv3 %>% 
  count(employed, comf)

#I am testing for my response variable. My response variable should be the hazard
surv4 <- surv3 %>% 
  mutate(fb = ifelse(is.na(kdob), 0, 1)) %>% 
  mutate(unemp = ifelse(jbstat == "Unemployed", 1, 0)) %>% #Create a variable to just test for individuals reporting unemployed (0 = employed, inactive and other)
  mutate(unemp = ifelse(is.na(jbstat), 0, unemp)) %>% 
  mutate(uncomf = ifelse(finnow.num <= 2, 1, 0)) %>%  #Creates a binary for positive versus negative current financial stability
  mutate(finnow3cat = ifelse(finnow.num <= 2, 1, ifelse(finnow.num == 3, 2, 3))) %>% 
  mutate(finnow3cat = as.character(finnow3cat)) %>% 
  mutate(finnow3cat = recode(finnow3cat,
                         "1" = "Finding it difficult",
                         "2" = "Getting by",
                         "3" = "Doing fine")) %>% 
  group_by(t2) %>%
  mutate(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>% #I took this from my script 9
  mutate(worse = as.character(worse)) %>% 
  ungroup()


surv4 %>% count(finnow3cat)

surv4m <- surv4 %>% filter(sex == 1)
surv4f <- surv4 %>% filter(sex == 2)

#Interaction Test 1 - comf employed
#
interaction.plot(x.factor = surv4$employed,
                 trace.factor = surv4$comf,
                 response = surv4$hazard)

boxplot(hazard ~ comf * employed, data=surv4)


aov1 <- aov(hazard ~ comf*employed, data=surv4)
summary(aov1)
model.tables(aov1, type = "means")
lm1 <- lm(hazard ~ comf*employed, data=surv4)
summary(lm1)

plot(allEffects(aov1), multiline=TRUE, ci.style="bars")

#It is clear that there is a slight interaction between "comf" and "employed"
#This was also tested seperately for each sex and holds true

###
#Interaction Test 2 - comf unemp
#
interaction.plot(x.factor = surv4$unemp,
                 trace.factor = surv4$comf,
                 response = surv4$hazard)

boxplot(hazard ~ comf * unemp, data=surv4)


aov2 <- aov(hazard ~ comf*unemp, data=surv4)
summary(aov2)
model.tables(aov2, type = "means")
lm2 <- lm(hazard ~ comf*employed, data=surv4)
summary(lm2)

plot(allEffects(aov2), multiline=TRUE, ci.style="bars")

#There is no interaction effect here.
#Quick test for checking the other side (uncomf)

aov3 <- aov(hazard ~ uncomf*unemp, data=surv4)
summary(aov3)
#Confirms the above findings

###
#Interaction Test 3 - finnow.imp employed
#
interaction.plot(x.factor = surv4$employed,
                 trace.factor = surv4$finnow.imp,
                 response = surv4$hazard)

boxplot(hazard ~ finnow.imp * employed, data=surv4)


aov4 <- aov(hazard ~ finnow.imp*employed, data=surv4)
summary(aov4)
model.tables(aov4, type = "means")
lm4 <- lm(hazard ~ comf*employed, data=surv4)
summary(lm4)

plot(allEffects(aov4), multiline=TRUE, ci.style="bars")

# There is a very clear interaction effect. This confirms test 1 and also suggests there is room to manipulate the variable
#There seems to be a particularly strong relationship to the middle category "Just getting by"

###
#Interaction Test 4 - finnow.imp unemp
#
interaction.plot(x.factor = surv4$unemp,
                 trace.factor = surv4$finnow.imp,
                 response = surv4$hazard)

boxplot(hazard ~ finnow.imp * unemp, data=surv4)


aov5 <- aov(hazard ~ finnow.imp*unemp, data=surv4)
summary(aov5)
model.tables(aov5, type = "means")
lm5 <- lm(hazard ~ comf*employed, data=surv4)
summary(lm5)

plot(allEffects(aov5), multiline=TRUE, ci.style="bars")

#There is only a very weak interaction effect. This confirms the findings from test 2

###
#Interaction Test 5 - finnow3cat employed
#
interaction.plot(x.factor = surv4$employed,
                 trace.factor = surv4$finnow3cat,
                 response = surv4$hazard)

boxplot(hazard ~ finnow3cat*employed, data=surv4)


aov6 <- aov(hazard ~ finnow3cat*employed, data=surv4)
summary(aov6)
model.tables(aov6, type = "means")
lm6 <- lm(hazard ~ comf*employed, data=surv4)
summary(lm6)

plot(allEffects(aov6), multiline=TRUE, ci.style="bars")

#There is a strong interaction here. This seems to be the strongest categorization of finnow

###
#Interaction Test 5 - finnow3cat unemp
#
interaction.plot(x.factor = surv4$unemp,
                 trace.factor = surv4$finnow3cat,
                 response = surv4$hazard)

boxplot(hazard ~ finnow3cat*unemp, data=surv4)


aov7 <- aov(hazard ~ finnow3cat*unemp, data=surv4)
summary(aov7)
model.tables(aov7, type = "means")
lm7 <- lm(hazard ~ finnow3cat*unemp, data=surv4)
summary(lm7)

plot(allEffects(aov7), multiline=TRUE, ci.style="bars")

#confirming previous tests, unemployment does not interact


###########################################################################
# Testing interaction employment and finfut -------------------------------
###########################################################################

#Interaction Test 1 - finfut.imp employed
#
interaction.plot(x.factor = surv4$employed,
                 trace.factor = surv4$finfut.imp,
                 response = surv4$hazard)

boxplot(hazard ~ finfut.imp * employed, data=surv4)


aov8 <- aov(hazard ~ finfut.imp*employed, data=surv4)
summary(aov8)
model.tables(aov8, type = "means")
lm8 <- lm(hazard ~ finfut.imp*employed, data=surv4)
summary(lm8)

plot(allEffects(aov8), multiline=TRUE, ci.style="bars")

#There seems to be a strong effect of hazard on both finfut and employed but they do not interact

#Interaction Test 2 - finfut.imp unemp
#
interaction.plot(x.factor = surv4$unemp,
                 trace.factor = surv4$finfut.imp,
                 response = surv4$hazard)

boxplot(hazard ~ finfut.imp * unemp, data=surv4)


aov9 <- aov(hazard ~ finfut.imp*unemp, data=surv4)
summary(aov9)
model.tables(aov9, type = "means")
lm9 <- lm(hazard ~ finfut.imp*unemp, data=surv4)
summary(lm9)

plot(allEffects(aov9), multiline=TRUE, ci.style="bars")

#On the other hand, there is a clear interaction with being unemployed and finfut. 
#The effect is strong for worse off and better off but not about the same

#Interaction Test 3 - worse employed
#
interaction.plot(x.factor = surv4$employed,
                 trace.factor = surv4$worse,
                 response = surv4$hazard)

boxplot(hazard ~ worse * employed, data=surv4)


aov10 <- aov(hazard ~ worse*employed, data=surv4)
summary(aov10)
model.tables(aov10, type = "means")
lm10 <- lm(hazard ~ worse*employed, data=surv4)
summary(lm10)

plot(allEffects(aov10), multiline=TRUE, ci.style="bars")

#There seems to be a strong effect of hazard on both finfut and employed but they do not interact
#There is no interaction effect

#Interaction Test 4 worse unemp
#
interaction.plot(x.factor = surv4$unemp,
                 trace.factor = surv4$worse,
                 response = surv4$hazard)

boxplot(hazard ~ worse * unemp, data=surv4)


aov11 <- aov(hazard ~ worse*unemp, data=surv4)
summary(aov11)
model.tables(aov11, type = "means")
lm11 <- lm(hazard ~ worse*unemp, data=surv4)
summary(lm11)

plot(allEffects(aov11), multiline=TRUE, ci.style="bars")

# There is clearly no interaction but the trend shows that unemployment makes future perceptions worse



###########################################################################
# Covariate Testing -------------------------------------------------------
###########################################################################

surv3 %>% count(hazard)

surv5 <- surv4 %>% 
  select(pidp, wave, hazard, finnow3cat) %>% 
  left_join(., surv3, by = c("pidp", "wave")) %>% 
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
  mutate(immigrant = ifelse(generation == 1, 1, 0)) %>% #Creates a binary of immigrants versus born UK
  # mutate(gen = recode(gen,
  #                     "1" = "First Generation",
  #                     "0" = "UK Born")) %>%
  mutate(immigrant = as.character(immigrant)) %>%
  # unite(genethnic, ethnic, gen,  sep = "-", remove = FALSE) %>%
  # unite(sexethnic, sex, ethnic, sep = "-", remove = FALSE) %>%
  # unite(ethnicsex, ethnic, sex, sep = "-", remove = FALSE) %>%
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
                         "-9" = "missing")) %>% 
  ungroup() %>% 
  mutate(finnow3cat = ifelse(finnow.num <= 2, 1, ifelse(finnow.num == 3, 2, 3))) %>% 
  mutate(finnow3cat = as.character(finnow3cat)) %>% 
  mutate(finnow3cat = recode(finnow3cat,
                             "1" = "finddifficult",
                             "2" = "getby",
                             "3" = "doingfine")) %>% 
  mutate(finfut.imp = fct_relevel(finfut.imp, c("About the same", "Worse off","Better off"))) %>% 
  mutate(combo = fct_relevel(combo, c("single-unknown", "cohab-employed", "cohab-non-employed", "cohab-unknown", 
                                      "married-employed", "married-non-employed", "married-unknown")))
  
surv5 %>% count(finnow3cat)
surv5 %>% count(finfut.imp)

#Saved surv5 RDS for quicker usage on model running
saveRDS(surv5, "surv5.rds")


# Immigrant
aov12 <- aov(hazard ~ immigrant, data=surv5)
summary(aov12)
TukeyHSD(aov12)
# Ethnicity
aov13 <- aov(hazard ~ ethnic, data=surv5)
summary(aov13)
TukeyHSD(aov13)

testglm <- glm(formula = event ~ t2 + agemn + agesq + se_ee + finnow3cat*employed + finfut.imp*employed+ edu_cat,
               family = binomial(link = "cloglog"),
               data = surv5)
summary(testglm)
summ(testglm, exp = TRUE, scale = TRUE) #exp = TRUE means that we want exponentiated estimates


interaction.plot(x.factor = surv5$immigrant,
                 trace.factor = surv5$ethnic,
                 response = surv5$hazard)

boxplot(hazard ~ ethnic*immigrant, data=surv5)


aov14 <- aov(hazard ~ ethnic*immigrant, data=surv5)
summary(aov14)
model.tables(aov14, type = "means")
lm14 <- lm(hazard ~ ethnic*immigrant, data=surv5)
summary(lm14)

plot(allEffects(aov14), multiline=TRUE, ci.style="bars")



