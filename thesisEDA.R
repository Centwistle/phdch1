library(tidyverse)
library(ggplot2)
library(Cairo)

CairoWin()

# Exploratory Data Analysis for USOC individual responses from 2009 - 2022
# Produces some summary tables, some group averages and some charting for headline evolutions

# Summary table of values

# Adding in Country Region Variable based on NUTS2 codes

usoc = usoc %>%
        mutate(Region = case_when(gor_dv < 10 & gor_dv >0 ~ "England", 
                                  gor_dv == 10 ~ "Wales",
                                  gor_dv == 11 ~ "Scotland",
                                  gor_dv == 12 ~ "NI"))

# Summary table of overall variables

summary(usoc)

# Analysing overall trends of childcare utilisation in the UK over time #
# Finds proportion of positive respondents who answer using childcare #

usoc %>%
  filter(intdaty_dv > 0) %>%
  filter(ccare == 1 | ccare == 2) %>%
  group_by(intdaty_dv) %>%
  mutate(prop = 1 - (mean(ccare) - 1)) %>%
  summarise(avg_prop = mean(prop)) %>%
  ggplot(aes(x = intdaty_dv, y = avg_prop)) + geom_line() + scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(title = "Rates of Childcare Utilisation in the UK", subtitle = "% of Positive Respondents (2009 - 2022)")

# Comparing childcare utilisation across NUTS2 regions of UK. Note this includes subregions of England (i.e. policy homogenous) compared to DA regions #
# Filters for NA responses in interview year, region and actual responses for childcare response
# Also filters for most recent responses in 2022 (massive inrregularities in data)
  
usoc %>%
  filter(intdaty_dv > 0 & intdaty_dv < 2021) %>%
  filter(gor_dv >= 0) %>%
  filter(ccare == 1 | ccare == 2) %>%
  group_by(intdaty_dv, gor_dv) %>%
  mutate(prop = 1 - (mean(ccare) - 1)) %>%
  summarise(avg_prop = mean(prop)) %>%
  ggplot(aes(x = intdaty_dv, y = avg_prop, group = gor_dv)) + geom_line(aes(color = gor_dv)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(title = "Rates of Childcare Utilisation Across NUTS2 Region", subtitle = "% of Positive Respondents (2009 - 2020")


# Repeating exercise across countries within the UK
# Note: Weird Seesawing effect between years for all non-English countries

usoc %>%
  filter(intdaty_dv > 0 & intdaty_dv < 2021) %>%
  filter(gor_dv >= 0) %>%
  filter(ccare == 1 | ccare == 2) %>%
  group_by(intdaty_dv, Region) %>%
  mutate(prop = 1 - (mean(ccare) - 1)) %>%
  summarise(avg_prop = mean(prop)) %>%
  ggplot(aes(x = intdaty_dv, y = avg_prop, group = Region)) + geom_line(aes(color = Region)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) + 
  labs(title = "Rates of Childcare Utilisation Across UK Home Nations", subtitle = "% of Positive Respondents (2009 - 2020")
  

# Investigating seesawing

usoc_test = usoc %>%
  filter(intdaty_dv > 0 & intdaty_dv < 2021) %>%
  filter(Region == "NI") %>%
  filter(ccare == 1 | ccare == 2) %>%
  group_by(intdaty_dv) %>%
  mutate(prop = 1 - (mean(ccare) - 1)) %>%
  summarise(avg_prop = mean(prop), n = n_distinct(pidp))

usoc_test %>% 
  mutate(prop_diff = avg_prop - lag(avg_prop), n_diff = n - lag(n)) %>%
  ggplot(aes(x = prop_diff, y = n_diff)) +
  geom_point() + 
  geom_smooth(method = "lm")

# Looking at hours worked for those answering no for childcare vs. those answering yes
# Filters out for the inadmissable / non-acceptable answers (i.e. the > 0s)

usoc %>% 
  filter(intdaty_dv > 0 & intdaty_dv < 2021 & jbhrs > 0) %>%
  filter(ccare == 1 | ccare == 2) %>%
  group_by(ccare) %>%
  summarise(mean_hrs_worked = mean(jbhrs)) 

# Boxplot chart for those not using childcare and those using

usoc %>%
  filter(intdaty_dv > 0 & intdaty_dv < 2021) %>%
  filter(jbhrs > 0) %>%
  filter(ccare == 1 | ccare == 2) %>%
  ggplot(aes(ccare, jbhrs, group = ccare)) + geom_boxplot() 

# Histograms of hours worked for those using childcare versus those not

usoc %>%
  filter(intdaty_dv > 0 & intdaty_dv < 2021) %>%
  filter(jbhrs > 0) %>%
  filter(ccare == 1 | ccare == 2) %>%
  mutate(ccare = as.factor(ccare)) %>%
  ggplot(aes(x = jbhrs, group = ccare, color = ccare)) + geom_histogram(fill = "azure")
