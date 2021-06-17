#Coded by: Brian Buh
#Started on: 17.06.2021
#Last Updated: 


library(tidyverse)

###Figures Young Demographers Conference

surv5 <- file.choose()
surv5 <- readRDS(surv5)

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
  dplyr::filter(cutsamp == TRUE, se_ee > 0) %>% 
  ggplot(aes(se_ee, fill = fbyes)) +
  geom_histogram(binwidth = 0.05) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  scale_x_continuous(breaks = c(0.01, 0.25, 0.5, 0.75, 1.0)) +
  labs(fill = "Sex", caption = "Not shown, 0 = continuously employed") +
  xlab("Persistent Joblessness Index: 1 = Continuously Jobless") +
  ggtitle("Truncated Persistent Joblessness Index", subtitle =  "UKHLS = Measured 9 months before first birth") +
  facet_wrap(~sex) +
  ggsave("yd_pji_truncated.png")





#Histogram of PJI of sample (NOT truncated)
surv5 %>% 
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
  annotate("text", x=.25, y=4500, size = 6, label= "0 = No Jobless Spells") +
  annotate("text", x=.8, y=1200, size = 6, label= "1 = Completely Jobless") +
  scale_fill_manual(values = c("#2a9d8f", "#e76f51")) +
  theme_minimal()+
  theme(legend.position = c(.8,.8), plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15, vjust=-1), axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(fill = "Births") +
  xlab("Persistent Joblessness Index") +
  ylab("Count")+
  ggsave("yd_pji.png")
