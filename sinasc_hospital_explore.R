library(tidyverse)
library(ggthemes)
library(geofacet)
library(trelliscopejs)


load("data/artifacts/hospital.Rdata")


hosp %>%
  filter(n > 50) %>%
  mutate(state_name = fct_reorder(state_name, Cesarean, median, na.rm = TRUE)) %>%
  ggplot(aes(x = Cesarean, fill = region_name)) +
  facet_wrap(~state_name, scales = "free_y") +
  geom_histogram( color = "white", bins = 10) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_tableau() +
  labs(x = "Percentage of cesarean births (per hospital)", y = "Number of hospitals in bin", fill = "Region",
       title = "Distribution of proportion cesareans at each hospital (n>50)") +
  scale_x_continuous(breaks = c(.1, .3, .5, .7, .9), labels = c(10, 30, 50, 70, 90))

ggsave("hathaway/results/prop_delivtype_hospital.png", width = 12, height = 6)

hosp %>%
  filter(n > 50) %>%
  mutate(state_name = fct_reorder(state_name, Cesarean, median, na.rm = TRUE)) %>%
  ggplot(aes(x = state_name, y = Cesarean)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(aes(color = region_name), width = .25, alpha = .5) +
  scale_color_tableau() +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(x = "State", y = "Percentage of cesarean births (per hospital)", color = "Region",
       title = "Distribution of proportion cesareans at each hospital (n>50)") 

ggsave("hathaway/results/prop_delivtype_hospital_boxplot.png", width = 12, height = 6)




###### by year  ######

n200_hosp <- hosp %>%
  filter(n > 200, !is.na(health_estbl_code)) %>%
  .$health_estbl_code


hosp_year %>%
  filter(health_estbl_code %in% n200_hosp) %>%
  mutate(state_name = fct_reorder(state_name, Cesarean)) %>%
  ggplot(aes(x= birth_year, y = Cesarean, color = region_name)) +
  facet_wrap(~state_name, scales = "free_y") +
  geom_point(aes(size = n)) +
  geom_line(aes(group = health_estbl_code)) +
  theme_bw() 

hosp_year %>%
#filter(health_estbl_code %in% c("2001071", "5701929")) %>%
  filter(health_estbl_code %in% n200_hosp) %>%
  ggplot(aes(x= birth_year, y = Cesarean)) +
    geom_point(aes(y = Vaginal, color = "V")) +
    geom_line(aes(y = Vaginal, color = "V"), show.legend = FALSE) +
    geom_point(aes(y = Cesarean, color = "Cn")) +
    geom_line(aes(y = Cesarean, color = "Cn"), show.legend = FALSE) +
    scale_color_manual(values = c(V = "#75bbfd", Cn = "#a83c09") ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(override.aes = list(linetype = NA, size = 4))) +
    labs(x = "Birth year", y = "Proportion", color = "Del. type") +
#    facet_wrap(~health_estbl_code + state_name)
    facet_trelliscope(~health_estbl_code + state_name + birth_muni_code, as_plotly = TRUE, width = 650, path = "../share/docs/trelliscope", nrow = 3, ncol = 3)








