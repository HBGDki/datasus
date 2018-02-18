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
