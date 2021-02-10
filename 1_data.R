#Coded by: Brian Buh
#Started on: 25.01.2020
#Last Updated: 27.01.2021

library(tidyverse)
library(haven)
library(lubridate)
library(usethis)

# Load Files for Project --------------------------------------------------



##########################################################################
# UKHLS -------------------------------------------------------------------

#wave 1
a_indall <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w1/a_indall_protect.dta")
a_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w1/a_child_protect.dta")
a_natchild <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w1/a_natchild_protect.dta")
a_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w1/a_indresp_protect.dta")


#wave 2
b_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w2/b_child_protect.dta")
b_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w2/b_indresp_protect.dta")


#wave 3
c_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w3/c_child_protect.dta")
c_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w3/c_indresp_protect.dta")


#wave 4
d_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w4/d_child_protect.dta")
d_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w4/d_indresp_protect.dta")


#wave 5
e_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w5/e_child_protect.dta")
e_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w5/e_indresp_protect.dta")


#wave6
f_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w6/f_child_protect.dta")
f_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w6/f_indresp_protect.dta")


#wave 7
g_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w7/g_child_protect.dta")
g_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w7/g_indresp_protect.dta")


#wave 8
h_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w8/h_child_protect.dta")
h_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w8/h_indresp_protect.dta")


#wave 9
i_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w9/i_child_protect.dta")
i_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w9/i_indresp_protect.dta")


#wave 10
j_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w10/j_child_protect.dta")
j_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w10/j_indresp_protect.dta")

#xwavedata
xwave <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_wx/xwavedat_protect.dta")

###########################################################################
# Parent data (Alita Nandi) -----------------------------------------------
###########################################################################

#Retrospective Fertility History
a_parent <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w1/a_parent_protect.dta")


###########################################################################
# Employment and Education History -----------------------------------------
###########################################################################
#File stata_work_life_histories produced from the Stata code provided by
#Liam Wright on the UK Data service

#!!! There is an issue with the transformation fo dates from the Stata file to the R
# file that needs to be fixed. The simplest answer would be to separate the month
# and years to separate variables in State and then bring the files into R

#UKHLS
annual_his <- read_dta("S:/Questionnaires/UKHLS/stata_work_life_histories/UKHLS Annual History.dta")
life_his <- read_dta("S:/Questionnaires/UKHLS/stata_work_life_histories/UKHLS Life History.dta")
edu_his <- read_dta("S:/Questionnaires/UKHLS/stata_work_life_histories/UKHLS Education History.dta")
# edu_his_var <- read_dta("S:/Questionnaires/UKHLS/stata_work_life_histories/Education Variables - Cleaned.dta")



###########################################################################
# Partnership Histories ---------------------------------------------------
###########################################################################

part_his <- read_dta("S:/Questionnaires/UKHLS/partnership-histories/stata/stata13/phistory_long.dta")


