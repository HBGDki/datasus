source("_fns.R")
library(dplyr)
library(forcats)

if (!"census" %in% ls())
  load("data/census.Rdata")

load("data/geo/state_muni_codes.Rdata")
load("data/artifacts/muni_summaries.Rdata")

rendabr <- filter(census$RENDABR, !is.na(state_code))

# roll up across municipality
muni_inc <- rendabr %>%
  group_by(muni_code, state_code, year) %>%
  summarise(
    mean_inc = sum(house_inc) / sum(pop),
    prop_4mw = sum(pop_4mw) / sum(pop),
    pop = sum(pop)) %>%
  left_join(state_codes) %>%
  ungroup() %>%
  mutate(state_name = fct_reorder(state_name, mean_inc))

muni_inc2 <- filter(muni_inc, year %in% c(2000, 2010))

## merge 2000/2010 municipality birth weight data with census
##---------------------------------------------------------

brthwt_muni_year2 <- brthwt_muni_year %>%
  filter(birth_year %in% c(2001, 2011)) %>%
  mutate(year = birth_year - 1)

brthwt_inc <- left_join(muni_inc2, brthwt_muni_year2,
  by = c(muni_code = "m_muni_code", year = "year"))

# filter(brthwt_inc, n >= 100)
# plot(sort(log10(brthwt_inc$n)))
# nrow(filter(brthwt_inc, n >= 20)) / nrow(brthwt_inc)

## merge 2000/2010 municipality and ga birth weight data with census
##---------------------------------------------------------

tmp <- brthwt_muni_ga_year %>%
  filter(birth_year %in% c(2001, 2011)) %>%
  mutate(year = birth_year - 1) %>%
  filter(!is.na(gest_weeks) & !is.na(mean_bwt))
brthwt_muni_ga_year2 <- filter(tmp, n >= 2)
# nrow(brthwt_muni_ga_year2) / nrow(tmp)

brthwt_inc_ga <- left_join(
  brthwt_muni_ga_year2,
  muni_inc2,
  by = c(m_muni_code = "muni_code", year = "year"))

##
##---------------------------------------------------------

save(brthwt_inc, brthwt_inc_ga, file = "data/artifacts/muni_census_ga.Rdata")
