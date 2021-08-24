#Coded by: Brian Buh
#Started on: 23.08.2021
#Last Updated: 


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

pji6 %>% 
  ggplot(aes(x = num, fill = empstat)) +
  geom_bar(position = 'fill')
