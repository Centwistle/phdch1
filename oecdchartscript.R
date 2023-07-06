oecd = oecdchildcaredat

oecd = oecd %>%
  mutate(country = as.factor(LOCATION), subject = as.factor(SUBJECT)) %>%

oecd %>%
  mutate(Country = fct_reorder(country, Value, .fun = 'mean')) %>%
  ggplot(aes(x = Country, y = Value, shape = factor(subject))) +
  geom_point(aes(color = factor(subject)), size = 4) + 
  geom_point(colour = "grey90", size = 1.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 1)) +
  scale_x_discrete(labels = c("GBR" = expression(bold(GBR)), parse=TRUE)) +
  labs(title = "Net Childcare Costs as a % of Household Income", 
       subtitle = "Couples and Single Parents Earning 67% of Avg. Wage")

oecdtime = OECDchildcareovertime

oecdtime = oecdtime %>%
  mutate(country = as.factor(LOCATION))

colval = c("UK" = "#F8766D", "OECD" = "#00BFC4")

oecdtime %>%
  ggplot(aes(x = TIME, y = Value, group = country)) + 
  geom_line(alpha = 0.1) + 
  geom_line(data = subset(oecdtime, country == "GBR"), color = "#F8766D", linewidth = 1.5) +
  geom_line(data = subset(oecdtime, country == "OECD"), color = "#00BFC4", linewidth = 1.5) +
  labs(title = "Net Childcare Costs as a % of Household Income Over Time", subtitle = "Couples Earning 67% of Average Wage", color = "Legend") +
  scale_color_manual(values = colval)