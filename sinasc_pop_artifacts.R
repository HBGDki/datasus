source("_fns.R")
library(ggplot2)
library(ggthemes)
# library(plotly)

if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

daily_births <- snsc %>%
  group_by(birth_date, birth_year) %>%
  tally()

daily_births_deliv <- snsc %>%
  group_by(birth_date, birth_year, deliv_type) %>%
  tally()

brthwt_yearly <- summarize_var_by(snsc, brthwt_g, birth_year)
brthwt_gest_weeks <- summarize_var_by(snsc, brthwt_g, gest_weeks)
brthwt_preg_type <- summarize_var_by(snsc, brthwt_g, preg_type)
brthwt_deliv_type <- summarize_var_by(snsc, brthwt_g, deliv_type)
brthwt_sex <- summarize_var_by(snsc, brthwt_g, sex)
brthwt_race <- summarize_var_by(snsc, brthwt_g, race)
brthwt_m_educ <- summarize_var_by(snsc, brthwt_g, m_educ)
brthwt_marital_status <- summarize_var_by(snsc, brthwt_g, marital_status)
brthwt_m_age_yrs <- summarize_var_by(snsc, brthwt_g, m_age_yrs)
brthwt_n_prenatal_visit <- summarize_var_by(snsc, brthwt_g, n_prenatal_visit)

save(daily_births, daily_births_deliv, brthwt_yearly, brthwt_gest_weeks,
  brthwt_preg_type, brthwt_deliv_type, brthwt_sex, brthwt_race, brthwt_m_educ,
  brthwt_marital_status, brthwt_m_age_yrs, brthwt_n_prenatal_visit,
  file = "data/artifacts/brthwt_summaries.Rdata")
