#Coded by: Brian Buh
#Started on: 11.10.2021
#Last Updated: 




# Test for interactions

# -------------------------------------------------------------------------
# Interaction 1 - employed finnow -----------------------------------------
# -------------------------------------------------------------------------

####Model for men
baseline_mglm <- glm(formula = event ~ t2,
                     family = binomial(link = "cloglog"),
                     data = surv6m)
summ(baseline_mglm, exp = TRUE, scale = TRUE)

mglm1 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant,
             family = binomial(link = "cloglog"),
             data = surv6m)
summary(mglm1)
summ(mglm1, exp = TRUE)

mglm2 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant + fihhmnnet4_dv,
             family = binomial(link = "cloglog"),
             data = surv6m)
summary(mglm2)
summ(mglm2, exp = TRUE)
+ fihhmnnet4_dv

mglm3 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant + combo,
             family = binomial(link = "cloglog"),
             data = surv6m)
summary(mglm3)
summ(mglm3, exp = TRUE)

mglm4 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant + combo + fihhmnnet4_dv,
             family = binomial(link = "cloglog"),
             data = surv6m)
summary(mglm4)
summ(mglm4, exp = TRUE)

####Model for women
baseline_fglm <- glm(formula = event ~ t2,
                     family = binomial(link = "cloglog"),
                     data = surv6f)
summ(baseline_fglm, exp = TRUE)
fglm1 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant,
             family = binomial(link = "cloglog"),
             data = surv6f)
summary(fglm1)
summ(fglm1, exp = TRUE)

fglm2 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant + fihhmnnet4_dv,
             family = binomial(link = "cloglog"),
             data = surv6f)
summary(fglm2)
summ(fglm2, exp = TRUE)

fglm3 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant + combo,
             family = binomial(link = "cloglog"),
             data = surv6f)
summary(fglm3)
summ(fglm3, exp = TRUE)

fglm4 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant + combo + fihhmnnet4_dv,
             family = binomial(link = "cloglog"),
             data = surv6f)
summary(fglm4)
summ(fglm4, exp = TRUE)

# -------------------------------------------------------------------------
# Interaction 2 - employed finnow  + employed finfut-----------------------
# -------------------------------------------------------------------------

export_summs(mglm1, mglm2, mglm3, mglm4, fglm1, fglm2, fglm3, fglm4,
             # model.names = c("Men 1", "Employed Men 1", "Men 2", "Employed Men 2", "Women 1", "Employed Women 1", "Women 2", "Employed Women 2"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t2",
             #           "PJI" = "pji3",
             #           "Employed" = "employed",
             #           "Finding it difficult" = "finnow3catfinddifficult",
             #           "Getting by" = "finnow3catgetby",
             #           "Worse off" = "finfut.impWorse off",
             #           "Better off" = "finfut.impBetter off",
             #           "CCI" = "cci",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "< = 1975" = "cohort2<=1975",
             #           ">= 1990" = "cohort2>=1990",
             #           "Education Low" = "edulow",
             #           "Education Medium" = "edumedium",
             #           "Immigrant" = "immigrant1",
             #           "Time 2" = "t2_3",
             #           "Likely lose job next 12 months" = "jbsec.dummy1",
             #           "Permanent Contract" = "permcon",
             #           "Part-Time" = "parttime",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "Household Income, benefits and deductions adjusted" = "fihhmnnet4_dv"),
             exp = TRUE,
             to.file = "html",
             file.name = "full_interaction1_11-10-21.html")

# -------------------------------------------------------------------------
# Full Sample Model Run ---------------------------------------------------
# -------------------------------------------------------------------------

####Model for men
baseline_mglm <- glm(formula = event ~ t2,
                     family = binomial(link = "cloglog"),
                     data = surv6m)
summ(baseline_mglm, exp = TRUE, scale = TRUE)

mglm1 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat  + employed * finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant,
             family = binomial(link = "cloglog"),
             data = surv6m)
summary(mglm1)
summ(mglm1, exp = TRUE)

mglm2 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat  + employed * finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant + fihhmnnet4_dv,
             family = binomial(link = "cloglog"),
             data = surv6m)
summary(mglm2)
summ(mglm2, exp = TRUE)
+ fihhmnnet4_dv

mglm3 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat  + employed * finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant + combo,
             family = binomial(link = "cloglog"),
             data = surv6m)
summary(mglm3)
summ(mglm3, exp = TRUE)

mglm4 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat  + employed * finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant + combo + fihhmnnet4_dv,
             family = binomial(link = "cloglog"),
             data = surv6m)
summary(mglm4)
summ(mglm4, exp = TRUE)

####Model for women
baseline_fglm <- glm(formula = event ~ t2,
                     family = binomial(link = "cloglog"),
                     data = surv6f)
summ(baseline_fglm, exp = TRUE)
fglm1 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat + employed * finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant,
             family = binomial(link = "cloglog"),
             data = surv6f)
summary(fglm1)
summ(fglm1, exp = TRUE)

fglm2 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat  + employed * finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant + fihhmnnet4_dv,
             family = binomial(link = "cloglog"),
             data = surv6f)
summary(fglm2)
summ(fglm2, exp = TRUE)

fglm3 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat  + employed * finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant + combo,
             family = binomial(link = "cloglog"),
             data = surv6f)
summary(fglm3)
summ(fglm3, exp = TRUE)

fglm4 <- glm(formula = event ~ t2 + pji3 + employed * finnow3cat  + employed * finfut.imp + cci + agemn + agesq + cohort2 + edu + immigrant + combo + fihhmnnet4_dv,
             family = binomial(link = "cloglog"),
             data = surv6f)
summary(fglm4)
summ(fglm4, exp = TRUE)

# -------------------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# -------------------------------------------------------------------------

export_summs(mglm1, mglm2, mglm3, mglm4, fglm1, fglm2, fglm3, fglm4,
             # model.names = c("Men 1", "Employed Men 1", "Men 2", "Employed Men 2", "Women 1", "Employed Women 1", "Women 2", "Employed Women 2"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t2",
             #           "PJI" = "pji3",
             #           "Employed" = "employed",
             #           "Finding it difficult" = "finnow3catfinddifficult",
             #           "Getting by" = "finnow3catgetby",
             #           "Worse off" = "finfut.impWorse off",
             #           "Better off" = "finfut.impBetter off",
             #           "CCI" = "cci",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "< = 1975" = "cohort2<=1975",
             #           ">= 1990" = "cohort2>=1990",
             #           "Education Low" = "edulow",
             #           "Education Medium" = "edumedium",
             #           "Immigrant" = "immigrant1",
             #           "Time 2" = "t2_3",
             #           "Likely lose job next 12 months" = "jbsec.dummy1",
             #           "Permanent Contract" = "permcon",
             #           "Part-Time" = "parttime",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "Household Income, benefits and deductions adjusted" = "fihhmnnet4_dv"),
             exp = TRUE,
             to.file = "html",
             file.name = "full_interaction2_11-10-21.html")
