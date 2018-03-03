source("_fns.R")
library(dplyr)
library(ggplot2)
library(ggthemes)

load("data/artifacts/muni_census_ga.Rdata")

##
##---------------------------------------------------------

ggplot(filter(brthwt_inc, n >= 10 & !is.na(mean_bwt)), aes(mean_inc, mean_bwt)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  geom_abline(slope = 0, intercept = log10(2500), linetype = 2, alpha = 0.7) +
  scale_x_continuous(trans = "log10", breaks = c(100, 300, 800, 2000)) +
  facet_wrap(~ year) +
  theme_bw() +
  labs(
    title = "Mean birthweight vs. mean income for each municipality by census year",
    x = "Mean monthly income (R$)",
    y = "Mean birth weight (g)")

# ggplot(filter(brthwt_inc, n >= 100), aes(prop_4mw, prop_low_bwt)) +
#   geom_point(alpha = 0.5) +
#   geom_smooth() +
#   facet_wrap(~ year) +
#   theme_bw()

##
##---------------------------------------------------------

ggplot(brthwt_inc_ga, aes(mean_inc, mean_bwt)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "loess", span = 1, se = FALSE) +
  geom_abline(slope = 0, intercept = log10(2500), linetype = 2, alpha = 0.7) +
  scale_x_continuous(trans = "log10", breaks = c(100, 300, 800, 2000)) +
  scale_y_continuous(trans = "log10", breaks = c(500, 1000, 1500, 2500, 4000)) +
  facet_grid(year ~ gest_weeks) +
  theme_bw() +
  labs(
    title = "Mean birthweight vs. mean income for each municipality by gestational age at birth and census year",
    x = "Mean monthly income (R$)",
    y = "Mean birth weight (g)")

# ggplot(brthwt_inc_ga, aes(prop_4mw, prop_low_bwt)) +
#   geom_point(alpha = 0.5) +
#   geom_smooth(method = "loess", se = FALSE) +
#   facet_grid(year ~ gest_weeks) +
#   theme_bw() +
#   labs(
#     title = "Poverty vs. low birth weight for every municipality / gestational age at birth combination",
#     x = "Proportion with household income < 1/4 minimum wage",
#     y = "Proportion with low birth weight (g)")


# filter(brthwt_inc_ga, prop_low_bwt > 0.95 & prop_4mw < 0.05 & gest_weeks == "22-27 weeks")

# tmp <- filter(snsc, gest_weeks == "22-27 weeks" & birth_year == 2011 & m_muni_code == 410690)
# table(tmp$brthwt_g < 1800)



##
##---------------------------------------------------------

# brthwt_muni_deliv_year2 <- brthwt_muni_deliv_year %>%
#   filter(birth_year %in% c(2001, 2011)) %>%
#   mutate(year = birth_year - 1) %>%
#   filter(!is.na(deliv_type) & n >= 100)

# brthwt_inc2 <- left_join(brthwt_muni_deliv_year2, muni_inc2, by = c(m_muni_code = "muni_code", year = "year"))

# ggplot(brthwt_inc2, aes(mean_inc, mean_bwt)) +
#   geom_point(alpha = 0.5) +
#   geom_smooth() +
#   scale_x_log10() +
#   facet_wrap(~ year) +
#   theme_bw()

# ggplot(brthwt_inc2, aes(prop_4mw, prop_low_bwt)) +
#   geom_point(alpha = 0.5) +
#   geom_smooth() +
#   facet_grid(deliv_type ~ year) +
#   theme_bw()
