<<<<<<< HEAD
source("_fns.R")
library(ggplot2)
library(ggthemes)
library(forcats)
library(geofacet)

if (!"census" %in% ls())
  load("data/census.Rdata")

load("data/geo/state_muni_codes.Rdata")

d <- filter(census$RENDABR, !is.na(state_code))

##
##---------------------------------------------------------

# roll up across state
state_inc <- d %>%
  group_by(state_code, year) %>%
  summarise(
    mean_inc = sum(house_inc) / sum(pop),
    prop_4mw = sum(pop_4mw) / sum(pop),
    pop = sum(pop)) %>%
  left_join(state_codes) %>%
  ungroup() %>%
  mutate(state_name = fct_reorder(state_name, mean_inc))

state_inc10 <- state_inc %>%
  filter(year == 2010) %>%
  mutate(
    state_name = fct_reorder(state_name, mean_inc),
    region_name = fct_reorder(region_name, mean_inc, .desc = TRUE))

state_inc10p <- state_inc %>%
  filter(year == 2010) %>%
  mutate(
    state_name = fct_reorder(state_name, prop_4mw, .desc = TRUE),
    region_name = fct_reorder(region_name, prop_4mw))

ggplot(state_inc10, aes(mean_inc, state_name, color = region_name)) +
  geom_point()

ggplot(state_inc10, aes(mean_inc, state_name, color = region_name)) +
  geom_point(size = 2) +
  facet_grid(region_name ~ ., scales = "free_y", space = "free_y") +
  theme_bw() +
  scale_color_tableau(guide = FALSE) +
  labs(y = NULL, x = "Mean household income")

ggplot(state_inc10p, aes(prop_4mw * 100, state_name, color = region_name)) +
  geom_point(size = 2) +
  facet_grid(region_name ~ ., scales = "free_y", space = "free_y") +
  theme_bw() +
  scale_color_tableau(guide = FALSE) +
  labs(
    y = NULL,
    x = "Percentage of population with average household income less than 1/4 minimum wage")

## over time by state
##---------------------------------------------------------

ggplot(state_inc, aes(year, prop_4mw * 100)) +
  # geom_rect(data = birth_col, aes(fill = val),
  #   xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf, alpha = 0.5,
  #   inherit.aes = FALSE) +
  geom_area(aes(fill = region_name), alpha = 0.5) +
  geom_point() +
  scale_fill_tableau(guide = FALSE) +
  # geom_abline(slope = 0, intercept = 0, alpha = 0.25) +
  theme_bw() +
  # scale_fill_gradient2("% Change\n2001 - 2015") +
  # scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  # scale_fill_viridis_c("% Change 2015") +
  facet_geo(~ state_code, grid = "br_states_grid2", label = "name") +
  theme(strip.text.x = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 7)) +
  labs(
    x = "Year",
    y = "Percentage of population with average household income less than 1/4 minimum wage")



# Average income domic. per capita: Average household income per capita of persons resident in a given geographical area, in the year in question. The per capita household income was the sum of the monthly income of the residents of the household, in reais, divided by the number of their residents. The minimum wage for the last year for which the series is being calculated becomes the benchmark for the whole series. This amount is corrected for all based on the INPC of July 2010, changing the value of the poverty line and consequently the proportion of poor. The reference value, the 2010 minimum wage, is R $ 510.00.


# country segmentation:
# - once on outcomes
# - once on covariates
# - send
=======
source("_fns.R")
library(ggplot2)
library(ggthemes)
library(forcats)
library(geofacet)

if (!"census" %in% ls())
  load("data/census.Rdata")

load("data/geo/state_muni_codes.Rdata")

d <- filter(census$RENDABR, !is.na(state_code))

##
##---------------------------------------------------------

# roll up across state
state_inc <- d %>%
  group_by(state_code, year) %>%
  summarise(
    mean_inc = sum(house_inc) / sum(pop),
    prop_4mw = sum(pop_4mw) / sum(pop),
    pop = sum(pop)) %>%
  left_join(state_codes) %>%
  ungroup() %>%
  mutate(state_name = fct_reorder(state_name, mean_inc))

state_inc10 <- state_inc %>%
  filter(year == 2010) %>%
  mutate(
    state_name = fct_reorder(state_name, mean_inc),
    region_name = fct_reorder(region_name, mean_inc, .desc = TRUE))

state_inc10p <- state_inc %>%
  filter(year == 2010) %>%
  mutate(
    state_name = fct_reorder(state_name, prop_4mw, .desc = TRUE),
    region_name = fct_reorder(region_name, prop_4mw))

ggplot(state_inc10, aes(mean_inc, state_name, color = region_name)) +
  geom_point()

ggplot(state_inc10, aes(mean_inc, state_name, color = region_name)) +
  geom_point(size = 2) +
  facet_grid(region_name ~ ., scales = "free_y", space = "free_y") +
  theme_bw() +
  scale_color_tableau(guide = FALSE) +
  expand_limits(x = 0) +
  labs(y = NULL, x = "Average monthly household income (R$)")

ggplot(state_inc10p, aes(prop_4mw * 100, state_name, color = region_name)) +
  geom_point(size = 2) +
  facet_grid(region_name ~ ., scales = "free_y", space = "free_y") +
  theme_bw() +
  scale_color_tableau(guide = FALSE) +
  expand_limits(x = 0) +
  labs(
    y = NULL,
    x = "Percentage with household income less than 1/4 minimum wage")

## over time by state
##---------------------------------------------------------

ggplot(state_inc, aes(year, prop_4mw * 100)) +
  geom_area(aes(fill = region_name), alpha = 0.5) +
  geom_point() +
  scale_fill_tableau("tableau10", "Region") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  theme_bw() +
  facet_geo(~ state_code, grid = "br_states_grid2", label = "name") +
  theme(strip.text.x = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 7)) +
  labs(
    x = "Year",
    y = "Percentage with household income less than 1/4 minimum wage")

## see how race income has changed over time
##---------------------------------------------------------

state_race_inc <- d %>%
  group_by(state_code, race, year) %>%
  summarise(
    mean_inc = sum(house_inc) / sum(pop),
    prop_4mw = sum(pop_4mw) / sum(pop),
    pop = sum(pop)) %>%
  left_join(state_codes) %>%
  ungroup() %>%
  mutate(state_name = fct_reorder(state_name, mean_inc))

ggplot(state_race_inc, aes(year, mean_inc, color = race, group = race)) +
  geom_line(color = "black", alpha = 0.2) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_tableau("tableau10", "Region") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  theme_bw() +
  facet_geo(~ state_code, grid = "br_states_grid2", label = "name", scales = "free_y") +
  theme(strip.text.x = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 7)) +
  labs(
    x = "Year",
    y = "Average household income (R$)")



# Average income domic. per capita: Average household income per capita of persons resident in a given geographical area, in the year in question. The per capita household income was the sum of the monthly income of the residents of the household, in reais, divided by the number of their residents. The minimum wage for the last year for which the series is being calculated becomes the benchmark for the whole series. This amount is corrected for all based on the INPC of July 2010, changing the value of the poverty line and consequently the proportion of poor. The reference value, the 2010 minimum wage, is R $ 510.00.


# country segmentation:
# - once on outcomes
# - once on covariates
# - send
>>>>>>> 6c92d402697e720d314a407e31acf5ed03631f33
