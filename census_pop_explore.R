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

by_race <- d %>%
  group_by(race) %>%
  summarise(
    mean_inc = sum(house_inc) / sum(pop),
    prop_4mw = sum(pop_4mw) / sum(pop),
    pop = sum(pop)) %>%
  ungroup() %>%
  mutate(
    race_p = fct_reorder(race, prop_4mw),
    race_mi = fct_reorder(race, mean_inc))

ggplot(by_race, aes(race, prop_4mw * 100, fill = race)) +
  geom_col() +
  theme_bw() +
  scale_fill_tableau(guide = FALSE)

ggplot(by_race, aes(race, mean_inc, fill = race)) +
  geom_col() +
  theme_bw() +
  scale_fill_tableau(guide = FALSE)
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

# rendabr <- d
# save(rendabr, file = "data/artifacts/census_rendabr.Rdata")

by_race <- d %>%
  filter(year == 2010) %>%
  group_by(race) %>%
  summarise(
    mean_inc = sum(house_inc) / sum(pop),
    prop_4mw = sum(pop_4mw) / sum(pop),
    pop = sum(pop)) %>%
  ungroup() %>%
  mutate(
    race_p = fct_reorder(race, prop_4mw),
    race_mi = fct_reorder(race, mean_inc))

ggplot(by_race, aes(race, prop_4mw * 100, fill = race)) +
  geom_col() +
  theme_bw() +
  scale_fill_tableau(guide = FALSE) +
  labs(
    y = "Proportion with household income less than 1/4 minimum wage",
    x = "Race")

ggplot(by_race, aes(race, mean_inc, fill = race)) +
  geom_col() +
  theme_bw() +
  scale_fill_tableau(guide = FALSE) +
  labs(y = "Average household income", x = "Race")

## by race and year
##---------------------------------------------------------

by_race_year <- d %>%
  group_by(race, year) %>%
  summarise(
    mean_inc = sum(house_inc) / sum(pop),
    prop_4mw = sum(pop_4mw) / sum(pop),
    pop = sum(pop)) %>%
  ungroup() %>%
  mutate(
    race = fct_reorder(race, mean_inc))

ggplot(by_race_year, aes(year, mean_inc, color = race)) +
  geom_line(color = "black", alpha = 0.5) +
  geom_point() +
  theme_bw() +
  scale_color_tableau(guide = FALSE) +
  facet_wrap(~ race, nrow = 1)

>>>>>>> 6c92d402697e720d314a407e31acf5ed03631f33
