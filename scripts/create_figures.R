

# figure: e0, long term -----------------------------------------------------------


dif_mnvars_e0  %>% 
  arrange(sex, year)  %>% 
  select(year, sex, ex=mean_death_overall, vx=var_death_overall)  %>% 
  distinct %>%
  ggplot(data=.) + 
  geom_line(aes(y=ex, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="e0 in years", x="Year") + ylim(c(0,90)) + 
  theme(legend.justification = c(0, 1), legend.position=c(0,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/period_life_expectancy_1750_present.png", 
       units = "cm", dpi = 300, width=8, height=8)


# figure: e5, long term -----------------------------------------------------------


dif_mnvars_e5  %>% 
  arrange(sex, year)  %>% 
  select(year, sex, ex=mean_death_overall, vx=var_death_overall)  %>% 
  distinct %>%
  ggplot(data=.) + 
  geom_line(aes(y=ex, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="e5 in years", x="Year") + ylim(c(0,90)) + 
  theme(legend.justification = c(0, 1), legend.position=c(0,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/e5_1750_present.png", 
       units = "cm", dpi = 300, width=8, height=8)

# figure: e65, long term -----------------------------------------------------------


dif_mnvars_e65  %>% 
  arrange(sex, year)  %>% 
  select(year, sex, ex=mean_death_overall, vx=var_death_overall)  %>% 
  distinct %>%
  ggplot(data=.) + 
  geom_line(aes(y=ex, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="e65 in years", x="Year") + ylim(c(65,90)) + 
  theme(legend.justification = c(0, 1), legend.position=c(0,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/e65_1750_present.png", 
       units = "cm", dpi = 300, width=8, height=8)


# figure: var(e0), long-term ------------------------------------------------------

dif_mnvars_e0  %>% 
  arrange(sex, year)  %>% 
  select(year, sex, ex=mean_death_overall, vx=var_death_overall)  %>% 
  distinct %>%
  ggplot(data=.) +
  geom_line(aes(y=vx, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="Variance in e0 (Years squared)", x="Year") +
  theme(legend.justification = c(0, 0), legend.position=c(0,0), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/var_e0_1750_present.png", 
       units = "cm", dpi = 300, width=8, height=8)




# figure: e5, long term -----------------------------------------------------------

dif_mnvars_e5  %>% 
  arrange(sex, year)  %>% 
  select(year, sex, ex=mean_death_overall, vx=var_death_overall)  %>% 
  distinct %>%
  ggplot(data=.) + 
  geom_line(aes(y=ex, x=year, group=sex, linetype=sex, col=sex)) + 
  labs(y="e5 (years)", x="Year") +
  theme(legend.justification = c(0, 1), legend.position=c(0,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/mean_e5_1750_present.png", 
       units = "cm", dpi = 300, width=8, height=8)




# figure: e65, long term -----------------------------------------------------------


dif_mnvars_e65  %>% 
  arrange(sex, year)  %>% 
  select(year, sex, ex=mean_death_overall, vx=var_death_overall)  %>% 
  distinct %>%
  ggplot(data=.) + 
  geom_line(aes(y=ex, x=year, group=sex, linetype=sex, col=sex)) + 
  labs(y="e65 (years)", x="Year") +
  theme(legend.justification = c(0, 1), legend.position=c(0,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/mean_e65_1750_present.png", 
       units = "cm", dpi = 300, width=8, height=8)




# figure: var(e5), long term ------------------------------------------------------


dif_mnvars_e5  %>% 
  arrange(sex, year)  %>% 
  select(year, sex, ex=mean_death_overall, vx=var_death_overall)  %>% 
  distinct %>%
  ggplot(data=.) +
  geom_line(aes(y=vx, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="Variance in e5 (Years squared)", x="Year") +
  theme(legend.justification = c(0, 0), legend.position=c(0,0), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/var_e5_1750_present.png", 
       units = "cm", dpi = 300, width=8, height=8)



# figure: var(e65), long term ------------------------------------------------------


dif_mnvars_e65  %>% 
  arrange(sex, year)  %>% 
  select(year, sex, vx=var_death_overall)  %>% 
  distinct %>%
  ggplot(data=.) +
  geom_line(aes(y=vx, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="Variance in e65 (Years squared)", x="Year") +
  theme(legend.justification = c(1, 0), legend.position=c(1,0), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/var_e65_1750_present.png", 
       units = "cm", dpi = 300, width=8, height=8)



###

# Correlations : mean and var, e0 and e5 ------------------------------------------------------------

# correlations between means and variances for e0 and e5

dif_mnvars_e0 %>%
  arrange(sex, year) %>%
  select(year, sex, ex=mean_death_overall, vx=var_death_overall) %>%
  distinct %>%
  ddply(., .(sex), function(x) round(cor(x=x$ex, y= x$vx), 2))

dif_mnvars_e5 %>%
  arrange(sex, year) %>%
  select(year, sex, ex=mean_death_overall, vx=var_death_overall) %>%
  distinct %>%
  ddply(., .(sex), function(x) round(cor(x=x$ex, y= x$vx), 2))

dif_mnvars_e65 %>%
  arrange(sex, year) %>%
  select(year, sex, ex=mean_death_overall, vx=var_death_overall) %>%
  distinct %>%
  ddply(., .(sex), function(x) round(cor(x=x$ex, y= x$vx), 2))

# Now for 1950 onwards

dif_mnvars_e0 %>%
  arrange(sex, year) %>%
  select(year, sex, ex=mean_death_overall, vx=var_death_overall) %>%
  distinct %>%
  filter(year >=1950) %>%
  ddply(., .(sex), function(x) round(cor(x=x$ex, y= x$vx), 2))

dif_mnvars_e5 %>%
  arrange(sex, year) %>%
  select(year, sex, ex=mean_death_overall, vx=var_death_overall) %>%
  distinct %>%
  filter(year >=1950) %>%
  ddply(., .(sex), function(x) round(cor(x=x$ex, y= x$vx), 2))

dif_mnvars_e65 %>%
  arrange(sex, year) %>%
  select(year, sex, ex=mean_death_overall, vx=var_death_overall) %>%
  distinct %>%
  filter(year >=1950) %>%
  ddply(., .(sex), function(x) round(cor(x=x$ex, y= x$vx), 2))



# figure: e0, short-term ----------------------------------------------------------

dif_mnvars_e0 %>%
  arrange(sex, year) %>%
  select(year, sex, ex=mean_death_overall, vx=var_death_overall) %>%
  distinct %>%
  filter(year>=1950) %>%
  ggplot(data=.) + 
  geom_line(aes(y=ex, x=year, group=sex, col=sex, linetype=sex)) + 
  labs(y="e0 (Years)", x="Year") + 
  theme(legend.justification = c(1,0), legend.position=c(1,0), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed") 
ggsave(filename = "figures/mean_e0_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)




# Figure: var(e5), short term----------------------------------------------------

vardeath_e0 %>%
  filter(year>=1950) %>%
  ggplot(data=.) + 
  geom_line(aes(y=var_death, x=year, group=sex, col=sex, linetype=sex)) + 
  labs(y="Variance in e0 (Years squared)", x="Year") + 
  theme(legend.justification = c(1,1), legend.position=c(1,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed") 
ggsave(filename = "figures/var_e0_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)





# Figure: e5, short-term  ------------------------------------------------------


vardeath_e5  %>% 
  filter(year >=1950) %>%
  ggplot(data=.) +
  geom_line(aes(y=mean_death, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="e5 in years", x="Year") +
  theme(legend.justification = c(0, 1), legend.position=c(0,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/mean_e5_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)




# Figure: var(e5), short term ---------------------------------------------

vardeath_e5  %>% 
  filter(year >=1950) %>%
  ggplot(data=.) +
  geom_line(aes(y=var_death, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="Variance in e5 (Years squared)", x="Year") +
  theme(legend.justification = c(1, 1), legend.position=c(1,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/var_e5_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)




# Figure: e65, short-term  ------------------------------------------------------


vardeath_e65  %>% 
  filter(year >=1950) %>%
  ggplot(data=.) +
  geom_line(aes(y=mean_death, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="e65 in years", x="Year") +
  theme(legend.justification = c(0, 1), legend.position=c(0,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/mean_e65_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)





# Figure: var(e65), short term ---------------------------------------------

vardeath_e65  %>% 
  filter(year >=1950) %>%
  ggplot(data=.) +
  geom_line(aes(y=var_death, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="Variance in e65 (Years squared)", x="Year") +
  theme(legend.justification = c(1, 0), legend.position=c(1,0), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/var_e65_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)





# Figure: country differences in e0, males ------------------------------------------


#figure 
dif_mnvars_e0 %>%
  filter(sex=="male" & year >=1950) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_mean < 0, dif_mean, 0),
                  ymax=ifelse(dif_mean > 0, dif_mean, 0)
  )) + 
  facet_wrap(~country) + ylim(c(-10, 10)) + 
  labs(title="Males", x="Year", y="Difference in e0 from European average")
ggsave(filename="figures/dif_males_1950.png", width=20, height=20, dpi=300, unit="cm")

#table




# figure: country differences in e0, female ---------------------------------------------

dif_mnvars_e0 %>%
  filter(sex=="female" & year >=1950) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_mean < 0, dif_mean, 0),
                  ymax=ifelse(dif_mean > 0, dif_mean, 0)
  )) + 
  facet_wrap(~country) + ylim(c(-10, 10)) +
  labs(title="Females", x="Year", y="Difference in e0 from European average")
ggsave(filename="figures/dif_females_1950.png", width=10, height=10, dpi=300)




# figure: rms of country differences in e0 -----------------------------------------------------


rms_e0 %>% filter(year <=2010) %>%
  ggplot(.) +
  geom_line(aes(x=year, y=rms_e0, group=sex, col=sex, linetype=sex)) +
  labs(y="RMS of country differences in e0", x="Year") + 
  theme(legend.justification = c(1,1), legend.position=c(1,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) +
  ylim(c(0, 8))
ggsave(filename = "figures/rms_e0_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)


# Figure, differences in e5, males ----------------------------------------


dif_mnvars_e5 %>%
  filter(sex=="male" & year >=1950) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_mean < 0, dif_mean, 0),
                  ymax=ifelse(dif_mean > 0, dif_mean, 0)
  )) + 
  facet_wrap(~country) + ylim(c(-10, 10)) + 
  labs(title="Males", x="Year", y="Difference in e5 from European average")
ggsave(filename="figures/dif_males_1950_e5.png", width=20, height=20, dpi=300, unit="cm")


# Figure - RMS plot, e5 ------------------------------------------------------


rms_e5 %>% filter(year <=2010) %>%
  ggplot(.) +
  geom_line(aes(x=year, y=rms_e5, group=sex, col=sex, linetype=sex)) +
  labs(y="RMS of country differences in e5", x="Year") + 
  theme(legend.justification = c(1,0), legend.position=c(1,0), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) +
  ylim(c(0, 4))
ggsave(filename = "figures/rms_e5_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)







# Figure: country differences in e5, short term, females ------------------


dif_mnvars %>%
  filter(sex=="female" & year >=1950) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_mean < 0, dif_mean, 0),
                  ymax=ifelse(dif_mean > 0, dif_mean, 0)
  )) + 
  facet_wrap(~country) + ylim(c(-10, 10)) +
  labs(title="Females", x="Year", y="Difference in e5 from European average")
ggsave(filename="figures/dif_females_1950_e5.png", width=10, height=10, dpi=300)



# Figure, differences in e65, males ----------------------------------------


dif_mnvars_e65 %>%
  filter(sex=="male" & year >=1950) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_mean < 0, dif_mean, 0),
                  ymax=ifelse(dif_mean > 0, dif_mean, 0)
  )) + 
  facet_wrap(~country) + ylim(c(-25, 25)) + 
  labs(title="Males", x="Year", y="Difference in e65 from European average")
ggsave(filename="figures/dif_males_1950_e5.png", width=20, height=20, dpi=300, unit="cm")


# Figure - RMS plot, e5 ------------------------------------------------------


rms_e5 %>% filter(year <=2010) %>%
  ggplot(.) +
  geom_line(aes(x=year, y=rms_e5, group=sex, col=sex, linetype=sex)) +
  labs(y="RMS of country differences in e5", x="Year") + 
  theme(legend.justification = c(1,0), legend.position=c(1,0), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) +
  ylim(c(0, 4))
ggsave(filename = "figures/rms_e5_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)








# Figure: country differences in e5, short term, females ------------------


dif_mnvars %>%
  filter(sex=="female" & year >=1950) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_mean < 0, dif_mean, 0),
                  ymax=ifelse(dif_mean > 0, dif_mean, 0)
  )) + 
  facet_wrap(~country) + ylim(c(-10, 10)) +
  labs(title="Females", x="Year", y="Difference in e5 from European average")
ggsave(filename="figures/dif_females_1950_e5.png", width=10, height=10, dpi=300)



# figure: rms e0 ----------------------------------------------------------

dif_mnvars_e0 %>%
  filter(year >= 1950 & year <= 2010) %>%
  select(country, sex, year, dif_mean) %>%
  group_by(sex, year) %>%
  summarise(rms = (mean(dif_mean^2, na.rm=T)^0.5)) %>%
  ggplot +
  geom_line(aes(x=year, y=rms, group=sex, col=sex, linetype=sex)) +
  labs(y="RMS of country differences in e0", x="Year") + 
  theme(legend.justification = c(1,1), legend.position=c(1,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) 
ggsave(filename = "figures/rms_e0_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)


# figure: rms e5 ----------------------------------------------------------

dif_mnvars_e5 %>%
  filter(year >= 1950 & year <= 2010) %>%
  select(country, sex, year, dif_mean) %>%
  group_by(sex, year) %>%
  summarise(rms = (mean(dif_mean^2, na.rm=T)^0.5)) %>%
  ggplot +
  geom_line(aes(x=year, y=rms, group=sex, col=sex, linetype=sex)) +
  labs(y="RMS of country differences in e5", x="Year") + 
  theme(legend.justification = c(1,0), legend.position=c(1,0), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) 
ggsave(filename = "figures/rms_e5_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)


# figure: rms e65 ----------------------------------------------------------

dif_mnvars_e65 %>%
  filter(year >= 1950 & year <= 2010) %>%
  select(country, sex, year, dif_mean) %>%
  group_by(sex, year) %>%
  summarise(rms = (mean(dif_mean^2, na.rm=T)^0.5)) %>%
  ggplot +
  geom_line(aes(x=year, y=rms, group=sex, col=sex, linetype=sex)) +
  labs(y="RMS of country differences in e65", x="Year") + 
  ylim(c(0, 14)) +
  theme(legend.justification = c(1,1), legend.position=c(1,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) 
ggsave(filename = "figures/rms_e5_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)

# relationship between var(e5) and rms(e5) --------------------------------

rms_meanvar %>%
  filter(year <=2010) %>%
  ggplot(data=.)  +
  geom_line(aes(x=year, y=rms_e5, group=sex, colour=sex, linetype=sex))

rms_meanvar %>%
  filter(year <=2010) %>%
  ggplot(data=.)  +
  geom_line(aes(x=year, y=var_e5, group=sex, colour=sex, linetype=sex))

rms_meanvar %>%
  filter(year <= 2010) %>%
  ggplot(data = .) + 
  geom_point(aes(colour=year, y=var_e5, x=rms_e5)) + facet_wrap(~sex) +
  labs(x="Variance in Europe-wide e5", y="RMS of difference in e5 between nations")

ggsave(filename="figures/e5_rms_var_relationship.png",
       units= "cm", dpi=300, width=15, height=15
)

# So, it appears the relationship between rms_e5, a country difference, and 
# var_e5, an overall variation, used to be stronger until the 1970s/1980s

rms_meanvar %>%
  filter(year <= 2010) %>%
  ggplot(data = .) + 
  geom_point(aes(colour=year, y=var_e0, x=rms_e0)) + facet_wrap(~sex) +
  labs(x="Variance in Europe-wide e0", y="RMS of difference in e0 between nations")
ggsave(filename="figures/e0_rms_var_relationship.png",
       units= "cm", dpi=300, width=15, height=15
)


# A much clearer trend: historically, it appears that most of the difference
# in e0 was to do with differences in infant mortality between countries

