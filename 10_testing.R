#Coded by: Brian Buh
#Started on: 15.04.2021
#Last Updated: 


library(tidyverse)
library(haven)


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
  mutate(sex = as.factor(sex))


###Months since end of education
#Number of months since end of education until first birth (Smooth)
ageedu %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Wormen",)) %>%
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
  facet_wrap(~sex) 

#Number of months since end of education until first birth (Bar)
ageedu %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Wormen",)) %>%
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
ageedu %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Wormen",)) %>%
  ggplot(aes(x = t2, fill = edu_cat)) +
  geom_histogram(binwidth = 3) +
  # geom_label(group_by(age) %>% filter(x == max(x)), aes(label = sprintf('%0.2f', y)), hjust = -0.5) +
  ylim(0, 17) +
  # scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Educational Attainment", caption = "histogram with binwidth of 3 months") +
  xlab("Months since end of formal education") + 
  ylab("Count of number of first births in specific month since end of education") +
  ggtitle("Number of Months from End of Education to First birth", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) 

###Age
#Age at first birth by education and sex (Smooth)
ageedu %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Wormen",)) %>%
  ggplot(aes(x = age, y= n, group = edu_cat, color = edu_cat)) +
  geom_smooth() +
  # geom_label(group_by(age) %>% filter(x == max(x)), aes(label = sprintf('%0.2f', y)), hjust = -0.5) +
  ylim(0, 17) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(color = "Educational Attainment") +
  xlab("Age in Months") + 
  ylab("Count of number of women experiencing first births in age by month") +
  ggtitle("Age at First birth by Educational Attainment", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) 

#Age at first birth by education and sex (Bar)
ageedu %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Wormen",)) %>%
  ggplot(aes(x = age, fill = edu_cat)) +
  geom_bar() +
  # geom_label(group_by(age) %>% filter(x == max(x)), aes(label = sprintf('%0.2f', y)), hjust = -0.5) +
  ylim(0, 17) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Educational Attainment") +
  xlab("Age in Months") + 
  ylab("Count of number of women experiencing first births in age by month") +
  ggtitle("Age at First birth by Educational Attainment", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) 

#Age at first birth by education and sex (Histogram)
ageedu %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Wormen",)) %>%
  ggplot(aes(x = age, fill = edu_cat)) +
  geom_histogram(binwidth = 0.25) +
  # geom_label(group_by(age) %>% filter(x == max(x)), aes(label = sprintf('%0.2f', y)), hjust = -0.5) +
  ylim(0, 17) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Educational Attainment", caption = "histogram with binwidth of 0.25 years") +
  xlab("Age in Months") + 
  ylab("Count of number of women experiencing first births in age by month") +
  ggtitle("Age at First birth by Educational Attainment", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) 

