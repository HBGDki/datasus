<<<<<<< HEAD
source("_fns.R")
library(ggplot2)
library(ggthemes)

if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

load("data/geo/state_muni_codes.Rdata")
load("data/geo/state_muni_shapefiles.Rdata")

brthwt_muni <- snsc %>%
  group_by(m_muni_code) %>%
  summarise(
    mean_bwt = mean(brthwt_g, na.rm = TRUE),
    n = length(which(!is.na(brthwt_g))),
    q1 = quantile(brthwt_g, 0.25, na.rm = TRUE),
    q3 = quantile(brthwt_g, 0.75, na.rm = TRUE)) %>%
  filter(!is.na(m_muni_code) & !is.nan(mean_bwt)) %>%
  arrange(mean_bwt)

muni_codes$code2 <- as.integer(substr(muni_codes$muni_code, 1, 6))

brthwt_muni_region <- brthwt_muni %>%
  left_join(muni_codes, by = c(m_muni_code = "code2")) %>%
  left_join(state_codes) %>%
  filter(!is.na(state_code)) %>%
  group_by(region_code) %>%
  mutate(rank = order(mean_bwt), region_mnbwt = mean(mean_bwt)) %>%
  arrange(state_code, rank) %>%
  ungroup() %>%
  arrange(mean_bwt)

brthwt_muni_region$rank2 <- seq_along(brthwt_muni_region$mean_bwt)
brthwt_muni_region$region_code <- forcats::fct_reorder(brthwt_muni_region$region_code, brthwt_muni_region$region_mnbwt, mean)
brthwt_muni_region$region_name <- forcats::fct_reorder(brthwt_muni_region$region_name, brthwt_muni_region$region_mnbwt, mean)

brthwt_muni_year <- snsc %>%
  group_by(m_muni_code, birth_year) %>%
  summarise(
    mean_bwt = mean(brthwt_g, na.rm = TRUE),
    n = length(which(!is.na(brthwt_g))),
    prop_low_bwt = length(which(brthwt_g < 2500)) / n,
    q1 = quantile(brthwt_g, 0.25, na.rm = TRUE),
    q3 = quantile(brthwt_g, 0.75, na.rm = TRUE)) %>%
  filter(!is.na(m_muni_code) & !is.nan(mean_bwt)) %>%
  arrange(mean_bwt)

brthwt_muni_deliv_year <- snsc %>%
  group_by(m_muni_code, birth_year, deliv_type) %>%
  summarise(
    mean_bwt = mean(brthwt_g, na.rm = TRUE),
    n = length(which(!is.na(brthwt_g))),
    prop_low_bwt = length(which(brthwt_g < 2500)) / n,
    q1 = quantile(brthwt_g, 0.25, na.rm = TRUE),
    q3 = quantile(brthwt_g, 0.75, na.rm = TRUE)) %>%
  filter(!is.na(m_muni_code) & !is.nan(mean_bwt))

muni_shps <- do.call(rbind, muni_shp)
muni_shps$code <- as.integer(substr(muni_shps$GEOCODIGO, 1, 6))
muni_shps@data <- left_join(muni_shps@data, brthwt_muni, by = c(code = "m_muni_code"))
muni_shps@data <- left_join(muni_shps@data, state_codes, by = c(UF = "state_code"))

save(brthwt_muni, brthwt_muni_region, brthwt_muni_year, muni_shps,
  file = "data/artifacts/muni_summaries.Rdata")
=======
source("_fns.R")
library(ggplot2)
library(ggthemes)

if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

load("data/geo/state_muni_codes.Rdata")
load("data/geo/state_muni_shapefiles.Rdata")

brthwt_muni <- snsc %>%
  group_by(m_muni_code) %>%
  summarise(
    mean_bwt = mean(brthwt_g, na.rm = TRUE),
    n = length(which(!is.na(brthwt_g))),
    q1 = quantile(brthwt_g, 0.25, na.rm = TRUE),
    q3 = quantile(brthwt_g, 0.75, na.rm = TRUE)) %>%
  filter(!is.na(m_muni_code) & !is.nan(mean_bwt)) %>%
  arrange(mean_bwt)

muni_codes$code2 <- as.integer(substr(muni_codes$muni_code, 1, 6))

brthwt_muni_region <- brthwt_muni %>%
  left_join(muni_codes, by = c(m_muni_code = "code2")) %>%
  left_join(state_codes) %>%
  filter(!is.na(state_code)) %>%
  group_by(region_code) %>%
  mutate(rank = order(mean_bwt), region_mnbwt = mean(mean_bwt)) %>%
  arrange(state_code, rank) %>%
  ungroup() %>%
  arrange(mean_bwt)

brthwt_muni_region$rank2 <- seq_along(brthwt_muni_region$mean_bwt)
brthwt_muni_region$region_code <- forcats::fct_reorder(brthwt_muni_region$region_code, brthwt_muni_region$region_mnbwt, mean)
brthwt_muni_region$region_name <- forcats::fct_reorder(brthwt_muni_region$region_name, brthwt_muni_region$region_mnbwt, mean)

brthwt_muni_year <- snsc %>%
  group_by(m_muni_code, birth_year) %>%
  summarise(
    mean_bwt = mean(brthwt_g, na.rm = TRUE),
    n = length(which(!is.na(brthwt_g))),
    prop_low_bwt = length(which(brthwt_g < 2500)) / n,
    q1 = quantile(brthwt_g, 0.25, na.rm = TRUE),
    q3 = quantile(brthwt_g, 0.75, na.rm = TRUE)) %>%
  filter(!is.na(m_muni_code) & !is.nan(mean_bwt)) %>%
  arrange(mean_bwt)

brthwt_muni_deliv_year <- snsc %>%
  group_by(m_muni_code, birth_year, deliv_type) %>%
  summarise(
    mean_bwt = mean(brthwt_g, na.rm = TRUE),
    n = length(which(!is.na(brthwt_g))),
    prop_low_bwt = length(which(brthwt_g < 2500)) / n,
    q1 = quantile(brthwt_g, 0.25, na.rm = TRUE),
    q3 = quantile(brthwt_g, 0.75, na.rm = TRUE)) %>%
  filter(!is.na(m_muni_code) & !is.nan(mean_bwt))

brthwt_muni_ga_year <- snsc %>%
  group_by(m_muni_code, birth_year, gest_weeks) %>%
  summarise(
    mean_bwt = mean(brthwt_g, na.rm = TRUE),
    sd_bwt = sd(brthwt_g, na.rm = TRUE),
    n = length(which(!is.na(brthwt_g))),
    prop_low_bwt = length(which(brthwt_g < 2500)) / n,
    q1 = quantile(brthwt_g, 0.25, na.rm = TRUE),
    q3 = quantile(brthwt_g, 0.75, na.rm = TRUE)) %>%
  filter(!is.na(m_muni_code) & !is.nan(mean_bwt))

muni_shps <- do.call(rbind, muni_shp)
muni_shps$code <- as.integer(substr(muni_shps$GEOCODIGO, 1, 6))
muni_shps@data <- left_join(muni_shps@data, brthwt_muni, by = c(code = "m_muni_code"))
muni_shps@data <- left_join(muni_shps@data, state_codes, by = c(UF = "state_code"))

# load("data/artifacts/muni_summaries.Rdata")
save(brthwt_muni, brthwt_muni_region, brthwt_muni_year,
  brthwt_muni_ga_year, brthwt_muni_deliv_year, muni_shps,
  file = "data/artifacts/muni_summaries.Rdata")

>>>>>>> 6c92d402697e720d314a407e31acf5ed03631f33
