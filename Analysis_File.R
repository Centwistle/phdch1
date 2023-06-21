## MAIN REGRESSION AND ANALYSIS FILE

# Package checks

library(tidyverse)
library(haven)
library(data.table)
library(lmtest)
library(MatchIt)
library(plm)

# Set WD and read in data 

setwd("C:/Users/40344509/Documents/Ch1 Rep")
usoc_final = read_csv("usoc_final.csv")

# Merging various childcare-specific responses into 2 variables: formal and informal.
# This then drops the individual ccare measure variables

usoc_final = usoc_final %>% mutate(ccareformal = case_when(wrkch2a1_child == 1 | wrkch2a2_child == 1 | wrkch2a3_child == 1 | wrkch2a4_child == 1 | wrkch2a5_child == 1 | wrkch2a6_child == 1 | wrkch2a7_child == 1 | wrkch2a8_child == 1 | wrkch2a9_child == 1 | wrkch2a10_child == 1 ~ 1),
                                   ccareinformal = case_when(wrkch2a11_child == 1 | wrkch2a12_child == 1 | wrkch2a13_child == 1 | wrkch2a14_child == 1 | wrkch2a15_child == 1 ~ 1)) %>%
  mutate(ccareformal = replace_na(ccareformal, 0),
         ccareinformal = replace_na(ccareinformal, 0))

usoc_final = usoc_final %>% select(-wrkch2a1_child, -wrkch2a2_child, -wrkch2a3_child, -wrkch2a4_child, -wrkch2a5_child, -wrkch2a6_child, -wrkch2a7_child, -wrkch2a8_child, -wrkch2a9_child, -wrkch2a10_child, -wrkch2a11_child, -wrkch2a12_child, -wrkch2a13_child, -wrkch2a14_child, -wrkch2a15_child)

# Coding in age bands. Age bands are: 15-24, 25-34, 35-44, 45+

usoc_final = usoc_final %>% mutate(AgeB1 = case_when(age_dv >= 15 & age_dv <= 24 ~ 1),
                                   AgeB2 = case_when(age_dv >= 25 & age_dv <= 34 ~ 1),
                                   AgeB3 = case_when(age_dv >= 35 & age_dv <= 44 ~ 1),
                                   AgeB4 = case_when(age_dv > 45 ~ 1)) %>%
                            mutate(AgeB1 = replace_na(AgeB1, 0),
                                   AgeB2 = replace_na(AgeB2, 0),
                                   AgeB3 = replace_na(AgeB3, 0),
                                   AgeB4 = replace_na(AgeB4, 0))

# Generate Treatment Period Variable for when treatment/policy was enacted (1st Sep 2017)

usoc_final = usoc_final %>% mutate(treat_period = if_else(int_dat < as.Date("2017-09-01"), 0, 1))

# Generation of Treatment Groups
# This first set of variables generates the month rolling blocks for treatment (i.e. children born between January and March will become eligible from April on)...
# ...this corresponds to 'elig_group_1' below, and so on. I.e. '1' corresponds to January.
# Src: https://www.gov.uk/30-hours-free-childcare

elig_group_1 = c(1, 2, 3)
elig_group_2 = c(4, 5, 6, 7, 8)
elig_group_3 = c(9, 10, 11, 12)

## Setting region variables

england = c(1:9)
england_wales = c(1:10)

# This generates the potential eligibility variables. In plain English, children of age, as/after the policy was enacted.
# This requires the policy to have been enacted ('treat_period == 1') and the children to be within the required ages.
# Those children old enough and at the right time have a 1, those outside the time period have a 0.
# Also generates an eligbility period (i.e. 1 if they become eligible in April, 2 in September and 3 in January)

usoc_final = usoc_final %>% mutate(elig = case_when(treat_period == 1 & age_dv_child >= 3 & age_dv_child < 5 ~ 1)) %>% mutate(elig = if_else(is.na(elig), 0, elig))
usoc_final = usoc_final %>% mutate(elig_period = case_when(birthm_child %in% elig_group_1 ~ 1, birthm_child %in% elig_group_2 ~ 2, birthm_child %in% elig_group_3 ~ 3))

# Finally this generates the treatment variables themselves. 1 if a parent of a child is eligible for free hours of childcare, and 0 if not.
# Conditions are that the policy be enacted, the child be between 3 and 5, and that it correspond to the month/cohort eligibility as described above.

usoc_final = usoc_final %>%
  mutate(treatedintreatperiod = case_when(elig == 1 & age_dv_child >= 3 & age_dv_child < 5 & intdatm_dv_child >= 4 & birthm %in% elig_group_1 ~ 1,
                                           elig == 1 & age_dv_child >= 3 & age_dv_child < 5 & intdatm_dv_child >= 8 & birthm %in% elig_group_2 ~ 1,
                                           elig == 1 & age_dv_child >= 3 & age_dv_child < 5 & intdatm_dv_child >= 1 & birthm %in% elig_group_3 ~ 1)) %>%
  mutate(treatedintreatperiod = replace_na(treatedintreatperiod, 0))

usoc_final = usoc_final %>%
  mutate(treated = case_when(age_dv_child >= 3 & age_dv_child < 5 & intdatm_dv_child >= 4 & birthm %in% elig_group_1 ~ 1,
                             age_dv_child >= 3 & age_dv_child < 5 & intdatm_dv_child >= 8 & birthm %in% elig_group_2 ~ 1,
                             age_dv_child >= 3 & age_dv_child < 5 & intdatm_dv_child >= 1 & birthm %in% elig_group_3 ~ 1)) %>%
  mutate(treated = replace_na(treated, 0))

# Filtering for England Regions & freeing memory - writes a separate usoc file for analysis.

usoc = usoc_final %>% filter(gor_dv %in% england)
rm(usoc_final)
write_csv(usoc, "usoc_analysis_ready.csv")

# Generating some frequency tables for eligibility vs. treatment

elig_freq_tab = table(usoc$treated, usoc$elig)
elig_freq_tab
  
# Checking treated n over time - table and chart

usoc %>% filter(intdaty_dv > 0) %>%
  group_by(intdaty_dv, treated) %>%
  summarise(n = sum(treated))

usoc %>% filter(intdaty_dv > 0) %>%
  group_by(intdaty_dv) %>%
  summarise(n = sum(treated)) %>%
  ggplot(aes(x = intdaty_dv, y = n)) +
  geom_line() + 
  geom_point() +
  labs(title = "Number of treated units over time", subtitle = "Policy enacted in 2017")

# Given the incredibly low number of treated in 2022 (n,2022 = 14) I filter this out.
# Only results in a very small number of observations lost on total dataset (-571 or a -0.11% reduction).

nrow(usoc) - nrow(usoc %>% filter(intdaty_dv < 2022))
(nrow(usoc %>% filter(intdaty_dv < 2022)) / nrow(usoc) - 1) * 100

# Filtering for non-applicable observations (Don't knows, missing values etc.)
# Note, some minus variables are acceptable to include, for example -8 (inadmissable) for hours worked includes the unemployed. 

usocfilt = usoc %>%
  filter(sex_dv != 0) %>%
  filter(age_dv != -9) %>%
  filter(birthm != -1 | birthm != -2) %>%
  filter(gor_dv != -9) %>%
  filter(mastat_dv > 0 ) %>%
  filter(health > 0) %>%
  filter(hiqual_dv > 0) %>%
  filter(jbstat > 0) %>%
  filter(jbft_dv > 0 | jbft_dv == -8) %>%
  filter(jbhrs >= 0 | jbhrs == -8) %>%
  filter(jbsic07_cc > 0 | jbsic07_cc == -8) %>%
  filter(fimnnet_dv > 0)
  
freq2 = table(usocfilt$treated, usocfilt$elig)
freq2

# Recoding some variables into binary dummies (i.e. Sex goes from 1 == Man 2 == Woman to 0 == Man and 1 == Woman)

usocfilt = usocfilt %>%
  mutate(sex_dv = case_when(sex_dv == 1 ~ 0,
                            sex_dv == 2 ~ 1),
         relationship = case_when(mastat_dv == 2 | mastat_dv == 3 | mastat_dv  == 10 ~ 1,
                                  mastat_dv == 1 | mastat_dv == 4 | mastat_dv == 5 | mastat_dv == 6 | mastat_dv == 7 | mastat_dv == 8 | mastat_dv == 9 ~ 0),
         ltsick = case_when(health == 2 ~ 0,
                            health == 1 ~ 1),
         educat = case_when(hiqual_dv == 1 | hiqual_dv == 2 | hiqual_dv == 3 ~ 1,
                            hiqual_dv == 4 | hiqual_dv == 5 | hiqual_dv == 9 ~ 0),
         inedorwork = case_when(jbstat == 1 | jbstat == 2 | jbstat == 5 | jbstat == 7 | jbstat == 9 |  jbstat == 11 | jbstat == 12 ~ 1,
                                jbstat == 3 | jbstat == 4 | jbstat == 6 | jbstat == 8 | jbstat == 10 | jbstat == 13 | jbstat == 97 ~ 0),
         ftemp = case_when(jbft_dv == 1 ~ 1,
                           jbft_dv == 2 | jbft_dv < 0 ~ 0),
         ptemp = case_when(jbft_dv == 2 ~ 1,
                           jbft_dv == 1 | jbft_dv < 0 ~ 0),
         usecccare = case_when(ccare == 2 | ccare < 0 ~ 0,
                               ccare == 1 ~ 1))


# BASELINE PROBITS AND LOGITS
# 1. Base probits and logits with personal characteristics - estimating probabilities/odds of being treated dependent on characteristics

options(scipen = 999)
base_probit = glm(treated ~ sex_dv + age_dv + relationship + ltsick + educat + inedorwork + ftemp + ptemp + fimnnet_dv,
                  family = binomial(link = "probit"),
                  data = usocfilt)

base_logit = glm(treated ~ sex_dv + age_dv + relationship + ltsick + educat + inedorwork + ftemp + ptemp + fimnnet_dv,
                  family = binomial(link = "logit"),
                  data = usocfilt)

summary(base_probit)
summary(base_logit)

# 2. Base Probits and logits with personal characteristics - adding in Age squared variables to detect non-linear impacts with age

base_probit_age_sqaured = glm(treated ~ sex_dv + age_dv + I(age_dv^2) + relationship + ltsick + educat + inedorwork + ftemp + ptemp + fimnnet_dv,
                  family = binomial(link = "probit"),
                  data = usocfilt)

base_logit_age_squared = glm(treated ~ sex_dv + age_dv + I(age_dv^2) + relationship + ltsick + educat + inedorwork + ftemp + ptemp + fimnnet_dv,
                 family = binomial(link = "logit"),
                 data = usocfilt)

summary(base_probit_age_sqaured)
summary(base_logit_age_squared)

# 3. Base Probits and logits with personal characteristics - rerunning with Age grouped into banding.

base_probit_bands = glm(treated ~ sex_dv + AgeB1 + AgeB2 + AgeB3 + AgeB4 + relationship + ltsick + educat + inedorwork + ftemp + ptemp + fimnnet_dv,
                        family = binomial(link = "probit"),
                        data=usocfilt)
base_logit_bands = glm(treated ~ sex_dv + AgeB1 + AgeB2 + AgeB3 + AgeB4 + relationship + ltsick + educat + inedorwork + ftemp + ptemp + fimnnet_dv,
                        family = binomial(link = "logit"),
                        data=usocfilt)

summary(base_probit_bands)
summary(base_logit_bands)

# outputting comparisons

list(base_probit, base_probit_age_sqaured) %>% outreg()
list(base_logit, base_logit_age_squared) %>% outreg()

# 4. Test baseline linear pooled regression on the panel data.

lm1 = lm(jbhrs ~ treatedintreatperiod + sex_dv + age_dv + gor_dv + relationship + ltsick + inedorwork, data = df_match, method = "lm", weights = weights)
summary(lm1)

# adding in robust standard errors

lm1robust = coeftest(lm1, 
                     vcov. = vcovHC,
                     type = "HC1")
options(scipen = 999)
tidy(lm1)
tidy(lm1robust)

# MATCHING - playing with matchit package

usocmatch = usocfilt %>% filter(agechy_dv_hhresp >= 0)

m.out = matchit(treated ~ sex_dv + age_dv + gor_dv + relationship + racel + ltsick + inedorwork + nkids_dv_hhresp + agechy_dv_hhresp + fimnnet_dv,
                         method = "nearest", data = usocmatch)

usocmatch = match.data(m.out)
dim(m.out)

summary(m.out, un = FALSE)

# some plots
# 1. jitter

plot(m.out, type = "density", interactive = FALSE,
     which.xs = ~age_dv + sex_dv + relationship)

plot(m.out, type = "density", interactive = FALSE,
     which.xs = ~racel + ltsick + inedorwork)

plot(m.out, type = "density", interactive = FALSE,
     which.xs = ~nkids_dv_hhresp + agechy_dv_hhresp + fimnnet_dv)

# Test DiD

did_1 = lm(jbhrs ~ treat_period + treated + treat_period:treated, data = usocmatch)
summary(did_1)

# Parallel trends chart
# 1. Parallel Trends of Hours Worked
# 2. Parallel Trends of Employment 
# 3. Parallel Trends of Full Time Employment as a Proportion of Sample

pt_hrs_worked = usocmatch %>%
  filter(intdaty_dv <= 2021 & intdaty_dv > 2009 & jbhrs >= 0) %>%
  group_by(intdaty_dv, treated) %>% 
  mutate(treated = as.factor(treated)) %>%
  summarise(avg_hrs = mean(jbhrs))

ggplot_para_trend_hrs = ggplot(pt_hrs_worked, aes(x = intdaty_dv, y = avg_hrs, color = treated)) + 
  geom_line(size = 1) +
  geom_vline(xintercept = 2017.75, linetype = "dashed", size = 1) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40)) +
  labs(title = "Average Weekly Hours Worked: Testing Parallel Trends Assumption", subtitle = "Dashed Vertical Line = Policy Enacted")

pt_emp_rate = usocmatch %>% 
  mutate(employed = case_when(jbstat == 1 | jbstat == 2 ~ 1),
                    employed = replace_na(employed, 0)) %>%
  filter(intdaty_dv <= 2021 & intdaty_dv > 2009) %>%
  group_by(intdaty_dv, treated) %>%
  mutate(emp_rate = mean(employed) * 100, treated = as.factor(treated)) %>%
  summarise(avg_emp = mean(emp_rate)) 

ggplot_para_trend_emp = ggplot(pt_emp_rate, aes(x = intdaty_dv, y = avg_emp, color = treated)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2017.75, linetype = "dashed", size = 1) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + 
  labs(title = "Employment Rate: Testing Parallel Trends Assumption", subtitle = "Dashed Vertical Line = Policy Enacted")

pt_ft_wrk = usocmatch %>%
  filter(intdaty_dv <= 2021 & intdaty_dv > 2009) %>%
  group_by(intdaty_dv, treated) %>% 
  mutate(treated = as.factor(treated), ftratio = mean(ftemp) * 100) %>%
  summarise(ftemp = mean(ftratio))

ggplot_para_trend_ft = ggplot(pt_ft_wrk, aes(x = intdaty_dv, y = ftemp, color = treated)) +
  geom_line(size = 1) + 
  geom_vline(xintercept = 2017.75, linetype = "dashed", size = 1) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + 
  labs(title = "Proportion of FT Workers: Testing Parallel Trends Assumption", subtitle = "Dashed Vertical Line = Policy Enacted")

# DiDs with time fixed effects
# 1. Define panel object

reg_hrs = usocmatch %>% filter(intdaty_dv > 2009 & intdaty_dv <= 2021 & jbhrs >= 0)

did_hrs = lm(jbhrs ~ treat_period + treated + treat_period:treated, data = reg_hrs)
summary(did_hrs)

reg_emp = usocmatch %>% 
  filter(intdaty_dv > 2009 & intdaty_dv <= 2021) %>%
  mutate(employed = case_when(jbstat == 1 | jbstat == 2 ~ 1),
         employed = replace_na(employed, 0)) %>%
  group_by(intdaty_dv) %>%
  mutate(emp_rate = mean(employed) * 100)

did_emp = lm(emp_rate ~ treat_period + treated + treat_period:treated, data = reg_emp)
summary(did_emp)

reg_ft = usocmatch %>%
  filter(intdaty_dv > 2009 & intdaty_dv <= 2021) %>%
  group_by(intdaty_dv, treated)
  mutate(ftratio = mean(ftemp) * 100)

did_ft = lm(ftemp ~ treat_period + treated + treat_period:treated, data = usocmatch)
summary(did_ft)

stargazer(did_hrs, did_emp, did_ft, type = "html")

# OUTPUTS #
# 1. Summary Stats
# 2. Probits and Logits
# 3. DiD Regression Tables
# 4. Parallel Trend Charts
# 5. Matched Density Plots

sum_stat_all = usocfilt %>% filter(jbhrs >= 0) %>% select(sex_dv, intdaty_dv, age_dv, birthy, relationship, ltsick, educat, inedorwork, ftemp, ptemp, jbhrs, usecccare) %>% st(file = "sum_stat_all.html")
sum_stat_match = usocmatch %>% filter(jbhrs >= 0) %>% select(sex_dv, intdaty_dv, age_dv, birthy, relationship, ltsick, educat, inedorwork, ftemp, ptemp, jbhrs, usecccare) %>% st(file = "sum_stat_matched.html")

stargazer(base_logit, base_logit_age_squared, base_logit_bands, title = "Baseline Logit Results", type = "html")
stargazer(base_probit, base_probit_age_sqaured, base_probit_bands, title = "Baseline Probit Results", type = "html")
stargazer(did_hrs, type = "html")
stargazer(did_emp, type = "html")
stargazer(did_ft, type = "html")

ggplot_para_trend_hrs
ggplot_para_trend_emp
ggplot_para_trend_ft

plot(m.out, type = "density", interactive = FALSE,
     which.xs = ~age_dv + sex_dv + relationship)

plot(m.out, type = "density", interactive = FALSE,
     which.xs = ~racel + ltsick + inedorwork)

plot(m.out, type = "density", interactive = FALSE,
     which.xs = ~nkids_dv_hhresp + agechy_dv_hhresp + fimnnet_dv)