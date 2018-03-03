<<<<<<< HEAD
source("_fns.R")
library(ggplot2)
library(ggthemes)
library(geofacet)

load("data/artifacts/brthwt_summaries.Rdata")

## how many mother's and birth state codes differ?
##---------------------------------------------------------

st_code_match <- table(snsc$m_state_code != snsc$birth_state_code)
prop.table(st_code_match)
#      FALSE       TRUE
# 0.98820667 0.01179333

# should look at how far apart they are...

##
##---------------------------------------------------------

length(which(is.na(snsc$m_state_code)))
# 7056
length(which(is.na(snsc$birth_state_code)))
# 111

## deliv_type by state
##---------------------------------------------------------

# birth_state_deliv <- snsc %>%
#   group_by(m_state_code, deliv_type) %>%
#   tally()

# birth_state_deliv2 <- birth_state_deliv %>%
#   filter(!is.na(deliv_type) & !is.na(m_state_code)) %>%
#   group_by(m_state_code) %>%
#   mutate(pct = n / sum(n) * 100)

# ggplot(birth_state_deliv2, aes(deliv_type, pct, fill = deliv_type)) +
#   geom_col() +
#   coord_flip() +
#   facet_geo(~ m_state_code, grid = "br_states_grid2", label = "name") +
#   theme_bw() +
#   scale_fill_tableau()


## percent change in number of births by state
##---------------------------------------------------------

birth_year_state2 <- birth_year_state %>%
  arrange(m_state_code, birth_year) %>%
  group_by(m_state_code) %>%
  mutate(pct_chg = 100 * (n - n[1]) / n[1]) %>%
  filter(!is.na(m_state_code))

birth_col <- birth_year_state2 %>%
  group_by(m_state_code) %>%
  summarise(val = tail(pct_chg, 1))

ggplot(birth_year_state2, aes(birth_year, pct_chg)) +
  geom_rect(data = birth_col, aes(fill = val),
    xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf, alpha = 0.5,
    inherit.aes = FALSE) +
  geom_point() +
  geom_abline(slope = 0, intercept = 0, alpha = 0.25) +
  theme_bw() +
  scale_fill_gradient2("% Change\n2001 - 2015") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  # scale_fill_viridis_c("% Change 2015") +
  facet_geo(~ m_state_code, grid = "br_states_grid2", label = "name") +
  theme(strip.text.x = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 7)) +
  labs(x = "Birth Year", y = "Percentage Change in Number of Births")

## percent low birth weight over time by state
##---------------------------------------------------------

brthwt_col <- brthwt_year_state %>%
  group_by(m_state_code) %>%
  summarise(val = tail(pct_low, 1))

ggplot(brthwt_year_state, aes(birth_year, pct_low)) +
  geom_rect(data = brthwt_col, aes(fill = val),
    xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf, alpha = 0.5, inherit.aes = FALSE) +
  geom_point() +
  theme_bw() +
  scale_fill_viridis_c("% Low\nBirthweight\n(2015)") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  facet_geo(~ m_state_code, grid = "br_states_grid2", label = "name") +
  theme(strip.text.x = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 7)) +
  labs(x = "Birth Year", y = "Percentage of Children Born with Low Birthweight")

brthwt_year_state %>%
  filter(birth_year %in% c(2001, 2015)) %>%
  group_by(m_state_code) %>%
  summarise(diff = diff(pct_low)) %>%
  arrange(-diff)


##
##---------------------------------------------------------

# births_st_preg_y <- summarize_st_yr(snsc, preg_type)
# plot_st_yr(births_st_preg_y, "preg_type")
# # almost all singletons - not much to see

# births_st_sex_y <- summarize_st_yr(snsc, sex)
# plot_st_yr(births_st_sex_y, "sex")
# # all 50/50 as you'd expect

plot_st_yr(births_st_dlv_y, "deliv_type", llab = "Delivery Type")
plot_st_yr(births_st_edu_y, "m_educ", llab = "Mother's\nEducation")
# plot_st_yr(births_st_gest_y, "gest_weeks", llab = "Gestational Age\nat Birth")
plot_st_yr(births_st_race_y, "race", llab = "Race")
plot_st_yr(births_st_mar_y, "marital_status", llab = "Marital Status")
plot_st_yr(births_st_vst_y, "n_prenatal_visit", llab = "# Prenatal\nVisits")

# brthwt_m_age_yrs <- summarize_var_by(snsc, brthwt_g, m_age_yrs)


##
##---------------------------------------------------------

=======
source("_fns.R")
library(ggplot2)
library(ggthemes)
library(geofacet)

load("data/artifacts/brthwt_summaries.Rdata")
load("data/artifacts/state_summaries.Rdata")

## how many mother's and birth state codes differ?
##---------------------------------------------------------

st_code_match <- table(snsc$m_state_code != snsc$birth_state_code)
prop.table(st_code_match)
#      FALSE       TRUE
# 0.98820667 0.01179333

# should look at how far apart they are...

##
##---------------------------------------------------------

length(which(is.na(snsc$m_state_code)))
# 7056
length(which(is.na(snsc$birth_state_code)))
# 111

## deliv_type by state
##---------------------------------------------------------

# birth_state_deliv <- snsc %>%
#   group_by(m_state_code, deliv_type) %>%
#   tally()

# birth_state_deliv2 <- birth_state_deliv %>%
#   filter(!is.na(deliv_type) & !is.na(m_state_code)) %>%
#   group_by(m_state_code) %>%
#   mutate(pct = n / sum(n) * 100)

# ggplot(birth_state_deliv2, aes(deliv_type, pct, fill = deliv_type)) +
#   geom_col() +
#   coord_flip() +
#   facet_geo(~ m_state_code, grid = "br_states_grid2", label = "name") +
#   theme_bw() +
#   scale_fill_tableau()


## percent change in number of births by state
##---------------------------------------------------------

birth_year_state2 <- birth_year_state %>%
  arrange(m_state_code, birth_year) %>%
  group_by(m_state_code) %>%
  mutate(pct_chg = 100 * (n - n[1]) / n[1]) %>%
  filter(!is.na(m_state_code))

birth_col <- birth_year_state2 %>%
  group_by(m_state_code) %>%
  summarise(val = tail(pct_chg, 1))

ggplot(birth_year_state2, aes(birth_year, pct_chg)) +
  geom_rect(data = birth_col, aes(fill = val),
    xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf, alpha = 0.5,
    inherit.aes = FALSE) +
  geom_point() +
  geom_abline(slope = 0, intercept = 0, alpha = 0.25) +
  theme_bw() +
  scale_fill_gradient2("% Change\n2001 - 2015") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  # scale_fill_viridis_c("% Change 2015") +
  facet_geo(~ m_state_code, grid = "br_states_grid2", label = "name") +
  theme(strip.text.x = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 7)) +
  labs(x = "Birth Year", y = "Percentage Change in Number of Births")

## percent low birth weight over time by state
##---------------------------------------------------------

brthwt_col <- brthwt_year_state %>%
  group_by(m_state_code) %>%
  summarise(val = tail(pct_low, 1))

ggplot(brthwt_year_state, aes(birth_year, pct_low)) +
  geom_rect(data = brthwt_col, aes(fill = val),
    xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf, alpha = 0.5, inherit.aes = FALSE) +
  geom_point() +
  theme_bw() +
  # scale_fill_viridis_c("% Low\nBirthweight\n(2015)") +
  viridis::scale_fill_viridis("% Low\nBirthweight\n(2015)") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  facet_geo(~ m_state_code, grid = "br_states_grid2", label = "name") +
  theme(strip.text.x = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 7)) +
  labs(x = "Birth Year", y = "Percentage of Children Born with Low Birthweight")

brthwt_year_state %>%
  filter(birth_year %in% c(2001, 2015)) %>%
  group_by(m_state_code) %>%
  summarise(diff = diff(pct_low)) %>%
  arrange(-diff)


## mother's age
##---------------------------------------------------------

ggplot(births_st_mage_y2, aes(birth_year, med)) + # , ymin = q1, ymax = q3)) +
  geom_point(size = 2) +
  theme_bw() +
  # geom_errorbar(width = 0.2) +
  facet_geo(~ m_state_code, grid = "br_states_grid2", label = "name") +
  theme(strip.text.x = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 7)) +
  labs(x = "Birth Year", y = "Median Mother's Age")



##
##---------------------------------------------------------

# births_st_preg_y <- summarize_st_yr(snsc, preg_type)
# plot_st_yr(births_st_preg_y, "preg_type")
# # almost all singletons - not much to see

# births_st_sex_y <- summarize_st_yr(snsc, sex)
# plot_st_yr(births_st_sex_y, "sex")
# # all 50/50 as you'd expect

plot_st_yr(births_st_dlv_y, "deliv_type", llab = "Delivery Type")
plot_st_yr(births_st_edu_y, "m_educ", llab = "Mother's\nEducation")
# plot_st_yr(births_st_gest_y, "gest_weeks", llab = "Gestational Age\nat Birth")
plot_st_yr(births_st_race_y, "race", llab = "Race")
plot_st_yr(births_st_mar_y, "marital_status", llab = "Marital Status")
plot_st_yr(births_st_vst_y, "n_prenatal_visit", llab = "# Prenatal\nVisits")

# brthwt_m_age_yrs <- summarize_var_by(snsc, brthwt_g, m_age_yrs)


##
##---------------------------------------------------------

>>>>>>> 6c92d402697e720d314a407e31acf5ed03631f33
