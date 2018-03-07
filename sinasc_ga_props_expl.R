library(tidyverse)
library(ggthemes)
library(purr)
library(fs)
library(osfr)
library(sp)
library(geosphere)
library(parallel)
#https://www.r-bloggers.com/speed-up-your-code-parallel-processing-with-multidplyr/
library(multidplyr)
library(geofacet)



########################################################################
#  How does race and delivery type interact with birth weight or gestational age?
########################################################################

load(file = "data/artifacts/gest_deliv_props.Rdata")



#Picture Description: The percentages sum to one for each year. So if we sum the columns in the table below each column will sum to 1. The plot shows the data in the table.
#Picture Discussion: The 32-36 week period is interesting. Officially, these births are early. From previous work we can see that gestational age is the primary driver of birth weight. If people are electing to deliver early then the weights will be small.
# - It does look like the percent preterm is growing year by year for all groups. However, there are big percentage point jumps in the 32-36 weeks.


gest_deliv_year %>%
  select(gest_weeks, deliv_type, birth_year, perc) %>%
  spread(key = birth_year, value = perc) %>%
  knitr::kable(digits = 3)

gest_deliv_year %>%
  ggplot(aes(x = birth_year, y = perc)) +
  geom_point(aes( color = deliv_type), show.legend = FALSE) + geom_line(aes( color = deliv_type), show.legend = FALSE) +
  geom_ribbon(aes(ymin = perc - 1.96*pse, ymax = perc + 1.96*pse, fill = deliv_type), alpha = .4) +
  facet_wrap(~gest_weeks, nrow = 1, scales = "free_y") +
  labs(x = "Birth Year", y = "Percent in group (delivery type and gestantional age) by year", fill = "Delivery type") +
  theme_bw() +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  scale_fill_tableau("trafficlight") + scale_color_tableau("trafficlight")

ggsave("hathaway/results/propbirth_byDelivetypeGestweeks.png", width = 12, height = 6)

#Does race come into play with these proportions?
# - Can we use race as a proxy for wealth?

#Picture Description: The percentages sum to one for each year within each race.
#Picture Discussion: Do to the low counts it is hard to see any differences between races for the less than 31 weeks groups. 
#However, the two upper groups do show a race effect for allocation of births between cesarean/vaginal and race.

gest_deliv_race_year %>%
  ggplot(aes(x = birth_year, y = perc)) +
  geom_point(aes(color = race), show.legend = FALSE) + geom_line(aes(color = race), show.legend = FALSE) +
  geom_ribbon(aes(ymin = perc - 1.96*pse, ymax = perc + 1.96*pse, fill = race), alpha = .4) +
  facet_grid(gest_weeks~deliv_type, scales = "free_y") +
  labs(x = "Birth Year", y = "Percent in group (delivery type and gestantional age) by year and race", fill = "Race") +
  theme_bw() +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  scale_fill_tableau() + scale_color_tableau() +
  theme(legend.position = "bottom")

ggsave("hathaway/results/propbirth_race_byDelivetypeGestweeks.png", width = 6, height = 8)
