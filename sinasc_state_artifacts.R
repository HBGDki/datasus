<<<<<<< HEAD
source("_fns.R")
library(ggplot2)
library(ggthemes)
library(geofacet)

if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

births_st_dlv_y <- summarize_st_yr(snsc, deliv_type)
births_st_edu_y <- summarize_st_yr(snsc, m_educ)
births_st_gest_y <- summarize_st_yr(snsc, gest_weeks)
births_st_race_y <- summarize_st_yr(snsc, race)
births_st_mar_y <- summarize_st_yr(snsc, marital_status)
births_st_vst_y <- summarize_st_yr(snsc, n_prenatal_visit)

birth_year_state <- snsc %>%
  group_by(m_state_code, birth_year) %>%
  tally()

brthwt_year_state <- snsc %>%
  group_by(m_state_code, birth_year) %>%
  summarise(
    mean_bwt = mean(brthwt_g, na.rm = TRUE),
    pct_low = 100 * length(which(brthwt_g < 2500)) / length(which(!is.na(brthwt_g)))) %>%
  filter(!is.na(m_state_code))

save(births_st_dlv_y, births_st_edu_y, births_st_gest_y, births_st_race_y,
  births_st_mar_y, births_st_vst_y, birth_year_state, brthwt_year_state,
  file = "data/artifacts/state_summaries.Rdata")
=======
source("_fns.R")
library(ggplot2)
library(ggthemes)
library(geofacet)

if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

births_st_dlv_y <- summarize_st_yr(snsc, deliv_type)
births_st_edu_y <- summarize_st_yr(snsc, m_educ)
births_st_gest_y <- summarize_st_yr(snsc, gest_weeks)
births_st_race_y <- summarize_st_yr(snsc, race)
births_st_mar_y <- summarize_st_yr(snsc, marital_status)
births_st_vst_y <- summarize_st_yr(snsc, n_prenatal_visit)

birth_year_state <- snsc %>%
  group_by(m_state_code, birth_year) %>%
  tally()

brthwt_year_state <- snsc %>%
  group_by(m_state_code, birth_year) %>%
  summarise(
    mean_bwt = mean(brthwt_g, na.rm = TRUE),
    pct_low = 100 * length(which(brthwt_g < 2500)) / length(which(!is.na(brthwt_g)))) %>%
  filter(!is.na(m_state_code))


## mother's age
##---------------------------------------------------------

# table(floor(snsc$m_age_yrs / 10))

snsc$m_age_bin <- cut(snsc$m_age_yrs,
  breaks = c(0, 10, 20, 30, 40, 100),
  labels = c("<10", "10-19", "20-29", "30-39", "40+"),
  right = FALSE)
snsc$m_age_bin[snsc$m_age_bin == "<10"] <- NA

births_st_mage_y <- summarize_st_yr(snsc, m_age_bin)

## now with numeric summaries

births_st_mage_y2 <- snsc %>%
  filter(!is.na(m_age_yrs) & !is.na(m_state_code)) %>%
  group_by(birth_year, m_state_code) %>%
  summarise(
    med = median(m_age_yrs),
    q1 = quantile(m_age_yrs, 0.25),
    q3 = quantile(m_age_yrs, 0.75))

# load("data/artifacts/state_summaries.Rdata")
save(births_st_dlv_y, births_st_edu_y, births_st_gest_y, births_st_race_y,
  births_st_mar_y, births_st_vst_y, birth_year_state, brthwt_year_state,
  births_st_mage_y, births_st_mage_y2,
  file = "data/artifacts/state_summaries.Rdata")
>>>>>>> 6c92d402697e720d314a407e31acf5ed03631f33
