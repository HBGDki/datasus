
########################################################################
#  How does race and delivery type interact with gestational age period?
########################################################################

library(tidyverse)

#(cl <- detectCores()-1)
#cluster <- create_cluster(cores = cl)


if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

snsc <- snsc %>% as.tibble()

load("data/artifacts/muni_census_ga.Rdata") # brthwt_inc and brthwt_inc_ga in the Rdata file



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


save(gest_deliv_year, gest_deliv_race_year, file = "data/artifacts/gest_deliv_props.Rdata")
