source("_fns.R")
library(ggplot2)
library(ggthemes)
# library(plotly)

load("data/artifacts/brthwt_summaries.Rdata")

## yearly
##---------------------------------------------------------

yearly_births <- daily_births %>%
  group_by(birth_year) %>%
  summarise(n = sum(n))

# ***
ggplot(yearly_births, aes(birth_year, n / 1e6)) +
  geom_point() +
  theme_bw() +
  labs(x = "Birth Year", y = "Number of Births (millions)")

## daily
##---------------------------------------------------------

daily_births <- daily_births %>%
  mutate(
    birth_month = as.integer(format(birth_date, "%m")),
    birth_month2 = factor(month.abb[birth_month], levels = month.abb),
    birth_wkday = format(birth_date, "%a"),
    day_type = ifelse(birth_wkday %in% c("Sat", "Sun"), "weekend", "weekday"))

ggplot(daily_births, aes(birth_date, n)) +
  geom_point() +
  theme_bw()

ggplot(daily_births, aes(birth_date, n)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ birth_year, scales = "free_x")

# ***
ggplot(daily_births, aes(birth_date, n, color = day_type)) +
  geom_point(alpha = 0.7) +
  theme_bw() +
  scale_color_tableau("tableau10", NULL) +
  scale_x_date(labels = function(x) paste0("'", substr(x, 3, 4))) +
  facet_wrap(~ birth_month2, nrow = 1) +
  labs(x = "Birth Date", y = "Number of Daily Births")

## weekly
##---------------------------------------------------------

weekly_births <- daily_births %>%
  mutate(birth_week = lubridate::floor_date(birth_date, unit = "week")) %>%
  group_by(birth_week) %>%
  summarise(n = sum(n)) %>%
  mutate(birth_month = as.integer(format(birth_week, "%m"))) %>%
  filter(n > 45000)

ggplot(weekly_births, aes(birth_week, n)) +
  geom_point() +
  theme_bw()

ggplot(weekly_births, aes(birth_week, n)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ birth_month, nrow = 1) +
  labs(x = "Birth Week", y = "Number of Births")

## monthly
##---------------------------------------------------------

monthly_births <- daily_births %>%
  mutate(birth_month_dt = lubridate::floor_date(birth_date, unit = "month")) %>%
  group_by(birth_month_dt) %>%
  summarise(n = sum(n)) %>%
  mutate(
    birth_month = as.integer(format(birth_month_dt, "%m")),
    birth_month2 = factor(month.abb[birth_month], levels = month.abb))

ggplot(monthly_births, aes(birth_month_dt, n)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ birth_month2, nrow = 1)

## number of births by time delivery type
##---------------------------------------------------------

yearly_births_deliv <- daily_births_deliv %>%
  group_by(birth_year, deliv_type) %>%
  summarise(n = sum(n)) %>%
  group_by(birth_year)

sum(yearly_births_deliv$n[is.na(yearly_births_deliv$deliv_type)])
# 100342 are not recorded (0.225 %)

yearly_births_deliv <- yearly_births_deliv %>%
  filter(!is.na(deliv_type)) %>%
  group_by(birth_year) %>%
  mutate(
    n_year = sum(n),
    pct = n / n_year * 100)

# ***
ggplot(yearly_births_deliv, aes(birth_year, n / 1000000, color = deliv_type)) +
  geom_point(size = 2.5, alpha = 0.75) +
  theme_bw() +
  scale_color_tableau(name = "Delivery") +
  labs(x = "Year", y = "Number of Births (millions)")

# ***
ggplot(yearly_births_deliv, aes(birth_year, pct, fill = deliv_type)) +
  geom_col(position = position_stack()) +
  geom_abline(slope = 0, intercept = 50, alpha = 0.5) +
  theme_bw() +
  scale_fill_tableau("tableau10", "Delivery Type") +
  labs(x = "Year", y = "Percentage of Births")

## look at distribution of birth weight
##---------------------------------------------------------

# d <- data_frame(
#   x = ppoints(1000),
#   y = quantile(snsc$brthwt_g, ppoints(1000), na.rm = TRUE))

# d2 <- data_frame(
#   x = ppoints(1000),
#   y = quantile(log10(snsc$brthwt_g), ppoints(1000), na.rm = TRUE))

# d3 <- data_frame(y = quantile(rnorm(100000), ppoints(1000)))

tmp <- sample(snsc$brthwt_g, 100000)

qqnorm(tmp)
qqline(tmp)

qqnorm(log(tmp))
qqline(log(tmp))

qqnorm(sqrt(tmp))
qqline(sqrt(tmp))

hist(tmp, breaks = 100)

plot(qnorm(ppoints(1000)), d$y)
plot(qnorm(ppoints(1000)), sqrt(d$y))
plot(qnorm(ppoints(1000)), log10(d$y))
plot(qnorm(ppoints(1000)), d2$y)

# ggplot(snsc, aes(brthwt_g)) +
#   geom_histogram()

## look at individual effect of various variables on birthwt
##---------------------------------------------------------

## birth weight by year
##---------------------------------------------------------

## *** yearly
p <- plot_var_by(brthwt_yearly, "Birth Weight (g)", "birth_year", xlab = "Birth Year")
p + geom_abline(slope = 0, intercept = 2500, linetype = 2, alpha = 0.7)
# has not changed over time

## *** gest_weeks
p <- plot_var_by(brthwt_gest_weeks, "Birth Weight (g)", "gest_weeks",
  xlab = "Gestational Age at Birth")
p + geom_abline(slope = 0, intercept = 2500, linetype = 2, alpha = 0.7)
# preterm birth obviously has an effect

## *** preg_type
p <- plot_var_by(brthwt_preg_type, "Birth Weight (g)", "preg_type",
  xlab = "Pregnancy Type")
p + geom_abline(slope = 0, intercept = 2500, linetype = 2, alpha = 0.7)

## *** deliv_type
p <- plot_var_by(brthwt_deliv_type, "Birth Weight (g)", "deliv_type",
  xlab = "Delivery Type")
p + geom_abline(slope = 0, intercept = 2500, linetype = 2, alpha = 0.7)

## *** sex
p <- plot_var_by(brthwt_sex, "Birth Weight (g)", "sex")
p + geom_abline(slope = 0, intercept = 2500, linetype = 2, alpha = 0.7)

## *** race
p <- plot_var_by(brthwt_race, "Birth Weight (g)", "race")
p + geom_abline(slope = 0, intercept = 2500, linetype = 2, alpha = 0.7)

## *** m_educ
p <- plot_var_by(brthwt_m_educ, "Birth Weight (g)", "m_educ",
  xlab = "Mother's Education")
p + geom_abline(slope = 0, intercept = 2500, linetype = 2, alpha = 0.7)

## *** marital status
p <- plot_var_by(brthwt_marital_status, "Birth Weight (g)", "marital_status",
  xlab = "Marital Status")
p + geom_abline(slope = 0, intercept = 2500, linetype = 2, alpha = 0.7)

## *** mother's age
p <- plot_var_by(brthwt_m_age_yrs, "Birth Weight (g)", "m_age_yrs",
  xlab = "Mother's Age")
p + geom_abline(slope = 0, intercept = 2500, linetype = 2, alpha = 0.7)

## *** number of prenatal visits
p <- plot_var_by(brthwt_n_prenatal_visit, "Birth Weight (g)", "n_prenatal_visit",
  xlab = "Number of Prenatal Visits")
p + geom_abline(slope = 0, intercept = 2500, linetype = 2, alpha = 0.7)


## n_prenatal_visit
##---------------------------------------------------------


# gest_weeks
# preg_type
# deliv_type
# sex
# race
# birth_place
# marital_status
# m_educ
# n_prenatal_visit
# m_age_yrs
# cong_anom
# n_live_child
# n_dead_child
# birth_date
