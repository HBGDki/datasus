library(tidyverse)
library(ggthemes)
library(purr)
library(fs)
library(osfr)
library(sp)
library(geosphere)
library(parallel)
#https://www.r-bloggers.com/speed-up-your-code-parallel-processing-with-multidplyr/
library(multidplyr)
library(geofacet)


#(cl <- detectCores()-1)
#cluster <- create_cluster(cores = cl)


if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

snsc <- snsc %>% as.tibble()

load("data/artifacts/muni_census_ga.Rdata") # brthwt_inc and brthwt_inc_ga in the Rdata file


########################################################################
#  Verify the relationship between prenatal visits and gestational age at birth categories.
########################################################################

visit_gest <- snsc %>%
  group_by(n_prenatal_visit, gest_weeks) %>%
  summarise(n = n(), birth_mean = mean(brthwt_g, na.rm = 2)) %>%
  filter(!is.na(n_prenatal_visit), !is.na(gest_weeks))


visit_gest_state <- snsc %>%
  group_by(n_prenatal_visit, gest_weeks, m_state_code) %>%
  summarise(n = n(), birth_mean = mean(brthwt_g, na.rm = 2)) %>%
  filter(!is.na(n_prenatal_visit), !is.na(gest_weeks))

save(visit_gest, visit_gest_state, file = "data/artifacts/ga_visits_brthwt.Rdata")

load(file = "data/artifacts/ga_visits_brthwt.Rdata")

# Looks like number of visits doesn't explain much for any group but the 'less than 22 weeks'
# Take aways from this. 
#     1) More or less visits doesn't improve birth size. 
#     2) We could use the number of visits for the smallest gestational periods to say what week they are in.
#     3) Patterns are consistent accross states

visit_gest_state %>%
  ggplot() +
  geom_rect(data = visit_gest, aes(fill = n/1000), xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf,  inherit.aes = FALSE) +
  geom_point(aes(x = m_state_code, y = birth_mean), fill = NA, color = "white") +
  facet_grid(gest_weeks~n_prenatal_visit, as.table = FALSE) +
  scale_fill_continuous(trans = "log10") +
  geom_hline(data = visit_gest, aes(yintercept = birth_mean), color = "lightgrey") +
  geom_text(data = visit_gest, x = 15, y = 2750, aes(label = paste0(round(n/1000,0),"K")), color = "white") +
  labs(fill = "Number Births\n1000s", x = "Municipality", y = "Average birth weight (g)", title = "Number of visits and gestational age at birth relationship to birth weight") +
  theme_bw() +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))

ggsave("hathaway/results/ga_visits_brthwt.png", width = 18, height = 8)


########################################################################
########################################################################


########################################################################
#  Is the birth weight relationship of the middle 4 gestational age groups similar across municipalities?
# Use income by municipality as well.
########################################################################

gest_state_muni <- snsc %>%
  filter(!gest_weeks %in% c("Less than 22 weeks", "42 weeks and more"), !is.na(gest_weeks)) %>%
  group_by(gest_weeks, m_state_code, m_muni_code) %>%
  summarise(n = n(), birth_mean = mean(brthwt_g, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gest_weeks_number = case_when(gest_weeks == "22-27 weeks" ~ 24,
                                       gest_weeks == "28 to 31 weeks" ~ 29,
                                       gest_weeks == "32-36 weeks" ~ 34,
                                       gest_weeks == "37-41 weeks" ~ 40))


muni_nlarge <- gest_state_muni %>%
  group_by(m_muni_code) %>%
  summarise(min_n = min(n)) %>%
  filter(min_n > 25) %>%
  .$m_muni_code


muni_slopes <- gest_state_muni %>%
  split(.$m_muni_code) %>%
#  .[1:2] %>%
  map(~ lm(birth_mean ~ gest_weeks_number, data = .)) %>%
  map("coefficients") %>%
map("gest_weeks_number") %>%
  do.call("rbind", .) 

# https://www.babycenter.com/average-fetal-length-weight-chart
muni_slopes <- muni_slopes %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  as.tibble() %>%
  rename(muni_code = rowname, slope = V1) %>%
  mutate(muni_code = as.integer(muni_code))

muni_summary <- brthwt_inc %>%
  filter(year == 2010) %>%
  left_join(muni_slopes) 

save(muni_summary, file = "data/artifacts/muni_census_ga_slope.Rdata")
load(file = "data/artifacts/muni_census_ga_slope.Rdata")


muni_summary %>%
  filter(!state_code == "NA", muni_code %in% muni_nlarge) %>%
  ggplot(aes(x = fct_reorder(state_name, slope, median), y = slope)) +
  geom_boxplot() +
  geom_jitter(width = .25, alpha = .5) +
  facet_grid(~region_name, scales = "free_x", space = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "State", y = "Slope: grams per week growth\nusing middle four gestational groups (24, 29, 34, 40)")

ggsave("hathaway/results/slope_state_region.png", width = 12, height = 6)

muni_summary %>%
  filter(!state_code == "NA", muni_code %in% muni_nlarge) %>%
  ggplot(aes(x = mean_inc, y = slope)) +
  geom_point(aes(color = region_name), alpha = .5) +
  facet_wrap(~region_name, nrow = 1) +
  theme_bw() +
  geom_smooth(se = FALSE) + 
  labs(x = "Municipality-level average monthly household income (R$)", y = "Slope: grams per week growth\nusing middle four gestational groups (24, 29, 34, 40)") +
  theme(legend.position = "none") +
  scale_color_tableau()
  
ggsave("hathaway/results/slope_income_region.png", width = 12, height = 6)


muni_summary %>%
  filter(!state_code == "NA", muni_code %in% muni_nlarge) %>%
  ggplot(aes(x = mean_inc, y = slope)) +
  geom_point(aes(color = region_name), alpha = .5) +
  theme_bw() +
  geom_smooth(se = FALSE) + 
  labs(x = "Municipality-level average monthly household income (R$)", y = "Slope: grams per week growth\nusing middle four gestational groups (24, 29, 34, 40)",
       color = "Region") +
  scale_color_tableau()

ggsave("hathaway/results/slope_income.png", width = 12, height = 6)


muni_summary %>%
  filter(!state_code == "NA", muni_code %in% muni_nlarge) %>%
  ggplot(aes(x = prop_4mw, y = slope)) +
  geom_point(aes(color = region_name), alpha = .5) +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~region_name, nrow = 1) +
  labs(x = "Percentage with household income less than 1/4 minimum wage", y = "Slope: grams per week growth\nusing middle four gestational groups (24, 29, 34, 40)",
       color = "Region") +
  scale_color_tableau()

ggsave("hathaway/results/slope_prop4mw.png", width = 12, height = 6)


muni_summary %>%
  filter(!state_code == "NA", muni_code %in% muni_nlarge) %>%
  ggplot(aes(x = prop_low_bwt, y = slope)) +
  geom_point(aes(color = region_name), alpha = .5) +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~region_name, nrow = 1) +
  labs(x = "Percentage of children born with low birthweight", y = "Slope: grams per week growth\nusing middle four gestational groups (24, 29, 34, 40)",
       color = "Region") +
  scale_color_tableau()

ggsave("hathaway/results/slope_proplowbw.png", width = 12, height = 6)


### Need Ryan's Grid
muni_summary %>%
  filter(!state_code == "NA", muni_code %in% muni_nlarge) %>%
  ggplot(aes(x = mean_inc, y = slope)) +
  geom_point(aes(color = region_name), alpha = .5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(x = "Municipality-level average monthly household income (R$)", y = "Slope: grams per week growth\nusing middle four gestational groups (24, 29, 34, 40)",
       color = "Region") +
  theme(legend.position = "none") +
  scale_color_tableau() +
  facet_geo(~ state_code, grid = "br_grid1", label = "name") 
  

########################################################################
########################################################################


########################################################################
#  How does race and delivery type interact with birth weight or gestational age?
########################################################################

gest_deliv_year <- snsc %>%
  mutate(gest_weeks = fct_collapse(gest_weeks,
                                   `Less than 27 weeks` = c("22-27 weeks", "Less than 22 weeks"),
                                   `More than 37 weeks` = c("37-41 weeks", "42 weeks and more"),
                                   `28-31 weeks` = "28 to 31 weeks",
                                   `32-36 weeks` = "32-36 weeks")) %>%
  group_by(gest_weeks, deliv_type, birth_year) %>%
  summarise(n = n(), birth_mean = mean(brthwt_g, na.rm = 2)) %>%
  filter(!is.na(gest_weeks), !is.na(deliv_type)) %>%
  ungroup() %>% group_by(birth_year) %>%
  mutate(perc = n/sum(n, na.rm = T), pse = sqrt(perc * (1- perc) / n))

#Picture Description: The percentages sum to one for each year. So if we sum the columns in the table below each column will sum to 1. The plot shows the data in the table.
#Picture Discussion: The 32-36 week period is interesting. Officially, these births are early. From previous work we can see that gestational age is the primary driver of birth weight. If people are electing to deliver early then the weights will be small.
# - It does look like the percent preterm is growing year by year for all groups. However, there are big percentage point jumps in the 32-36 weeks.


gest_deliv_year %>%
  select(gest_weeks, deliv_type, birth_year, perc) %>%
  spread(key = birth_year, value = perc) %>%
  knitr::kable(digits = 3)

gest_deliv_year %>%
  ggplot(aes(x = birth_year, y = perc)) +
  geom_point(aes( color = deliv_type), show.legend = FALSE) + geom_line(aes( color = deliv_type), show.legend = FALSE) +
  geom_ribbon(aes(ymin = perc - 1.96*pse, ymax = perc + 1.96*pse, fill = deliv_type), alpha = .4) +
  facet_wrap(~gest_weeks, nrow = 1, scales = "free_y") +
  labs(x = "Birth Year", y = "Percent in group (delivery type and gestantional age) by year", fill = "Delivery type") +
  theme_bw() +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  scale_fill_tableau("trafficlight") + scale_color_tableau("trafficlight")

ggsave("hathaway/results/propbirth_byDelivetypeGestweeks.png", width = 12, height = 6)



gest_deliv_race_year <- snsc %>%
  mutate(gest_weeks = fct_collapse(gest_weeks,
                                   `Less than 27 weeks` = c("22-27 weeks", "Less than 22 weeks"),
                                   `More than 37 weeks` = c("37-41 weeks", "42 weeks and more"),
                                   `28-31 weeks` = "28 to 31 weeks",
                                   `32-36 weeks` = "32-36 weeks")) %>%
  group_by(gest_weeks, birth_year, race, deliv_type) %>%
  summarise(n = n(), birth_mean = mean(brthwt_g, na.rm = 2)) %>%
  filter(!is.na(race), !is.na(gest_weeks), !is.na(deliv_type)) %>%
  ungroup() %>% group_by(race, birth_year) %>%
  mutate(perc = n/sum(n, na.rm = T), pse = sqrt(perc * (1- perc) / n))

#Does race come into play with these proportions?
# - Can we use race as a proxy for wealth?

#Picture Description: The percentages sum to one for each year within each race.
#Picture Discussion: Do to the low counts it is hard to see any differences between races for the less than 31 weeks groups. 
#However, the two upper groups do show a race effect for allocation of births between cesarean/vaginal and race.

gest_deliv_race_year %>%
  ggplot(aes(x = birth_year, y = perc)) +
  geom_point(aes(color = race), show.legend = FALSE) + geom_line(aes(color = race), show.legend = FALSE) +
  geom_ribbon(aes(ymin = perc - 1.96*pse, ymax = perc + 1.96*pse, fill = race), alpha = .4) +
  facet_grid(gest_weeks~deliv_type, scales = "free_y") +
  labs(x = "Birth Year", y = "Percent in group (delivery type and gestantional age) by year and race", fill = "Delivery type") +
  theme_bw() +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  scale_fill_tableau() + scale_color_tableau()

ggsave("hathaway/results/propbirth_race_byDelivetypeGestweeks.png", width = 12, height = 6)

