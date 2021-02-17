#Coded by: Brian Buh
#Started on: 28.01.2021
#Last Updated: 




#removing data respondents outside of the UKHLS Wave 1
#hhorig: 1= UKHLS GB, 2= UKHLS NI, 3= UKHLS emboost
#status: 1= marriage, 2= civil part, 10= cohabitation
#mrgend: 0= ongoing, 1= separation, 2= divorce, 3= widowhood, 4= unknown 
#cohend: 0= ongoing, 1= breakup, 2= marriage
part_his_edit <- part_his %>% 
  filter(hhorig <= 2 | hhorig == 7) %>% 
  dplyr:: select(-pid, -startdate, -enddate, -divorcedate, -lastintdate) %>% 
  mutate_all(~replace(.,.<0, NA)) %>%  #Changes all negative codes to NA
  unite(startdate, c(startm, starty), sep = "-")%>% 
  mutate(startdate = parse_date_time(startdate, "my"))%>% 
  unite(enddate, c(endm, endy), sep = "-") %>% 
  mutate(enddate = parse_date_time(enddate, "my")) %>% 
  unite(divorcedate, c(divorcem, divorcey), sep = "-") %>% 
  mutate(divorcedate = parse_date_time(divorcedate, "my"))

part_his_edit %>% 
  count(is.na(startdate))

part_his %>% 
  count(ever_married)



###########################################################################
# Saving partnership RDS --------------------------------------------------
###########################################################################

saveRDS(part_his_edit, file = "part_his_edit.rds")
part_his_edit <- file.choose()
part_his_edit <- readRDS(part_his_edit)


