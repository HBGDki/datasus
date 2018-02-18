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
