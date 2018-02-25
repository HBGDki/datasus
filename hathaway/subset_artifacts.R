library(tidyverse)
library(ggthemes)
library(fs)
library(osfr)
library(sp)
library(geosphere)
library(parallel)
#https://www.r-bloggers.com/speed-up-your-code-parallel-processing-with-multidplyr/
library(multidplyr)
library(geofacet)


#(cl <- detectCores()-1)
#cluster <- create_cluster(cores = cl)


if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

snsc <- snsc %>% as.tibble()

##### Make stratified subset  ######

dat <- snsc %>%
  filter(preg_type == "Singleton", gest_weeks %in% c("37-41 weeks", "42 weeks and more"), birth_year %in% c(2001, 2005, 2010, 2015), sex == "Female") %>%
  as.tibble()

write_rds(dat, "data/artifacts/singleton_fullterm_female_2005.rds")


muni_dat <- read_rds("data/geo/state_muni_latlong_elev_pop2017.Rds") 


muni_dat %>%
  group_by(state_name) %>%
  summarise_at( .vars = c("pop2017", "elev_m", "d_45_non", "d_45", "h_45", "d_33_non", "d_33", "h_33", "d_20", "h_20"), .funs = mean, na.rm = TRUE) %>%
  arrange(d_45)

muni_mom <- muni_dat %>%
  select(ibge7_code, ibge6_code, muni_state_name, state_code, region_code, lon, lat, elev_m) %>%
  rename_all(.funs = function(x) paste0(x,"_mom"))  %>%
  rename(m_muni_code = ibge6_code_mom) 

muni_birth <- muni_dat %>%
  select(ibge7_code, ibge6_code, muni_state_name, state_code, region_code, lon, lat, elev_m, pop2017, d_45) %>%
  rename_all(.funs = function(x) paste0(x,"_birth")) %>%
  rename(birth_muni_code = ibge6_code_birth)


dat_m <- dat %>%
  left_join(muni_birth) %>%
  mutate(m_age_group = case_when(m_age_yrs < 18 ~ "<18",
                                 m_age_yrs < 28 ~ "18-27",
                                 m_age_yrs < 38 ~ "28-37",
                                 m_age_yrs < 48 ~ "38-47",
                                 m_age_yrs >= 48 ~ ">48",
                                 TRUE ~ "missing"
                                 ),
         previous_births = rowSums(cbind(n_live_child, n_dead_child), na.rm = TRUE))


pdat <- dat_m %>%
  group_by(m_age_group, deliv_type, birth_year, birth_muni_code, muni_state_name_birth, race) %>%
  summarise(
    n = n(),
    med  = median(brthwt_g, na.rm = TRUE),
    q1 = quantile(brthwt_g, 0.25, na.rm = TRUE),
    q3 = quantile(brthwt_g, 0.75, na.rm = TRUE),
    mad = mad(brthwt_g, na.rm = TRUE),
    mean = mean(brthwt_g, na.rm = TRUE),
    sd = sd(brthwt_g, na.rm = TRUE),
    se = sd / sqrt(n()),
    elev_m_birth = mean(elev_m_birth, na.rm = TRUE),
    pop2017_birth = mean(pop2017_birth, na.rm =TRUE),
    d_45 = mean(d_45_birth, na.rm = TRUE)
    )





pdat %>%
  filter(n > 5, !is.na(race), elev_m_birth > -50, !is.na(deliv_type), m_age_group != "missing") %>%
  ungroup() %>%
  mutate(m_age_group = factor(m_age_group, levels = c("<18", "18-27", "28-37", "38-47", ">48"))) %>%
  ggplot(aes(x = elev_m_birth, y = mean, color = m_age_group)) +
    geom_point(alpha = .4) +
    geom_smooth(method = "lm", se = FALSE) +
#   geom_smooth(span = 250, color = "#FF3300", aes(group = race)) +
    facet_grid(deliv_type~race) +
    theme_bw() +
    geom_hline(linetype = "dashed", color = "black", yintercept = 2500) +
    theme(strip.text = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 12)) +
    scale_color_brewer(type = "qual") +
    labs(x = "Elevation of birth location (m)", y = "Average birth weight in municipality", color = "Mother's age group", title = "Average birth weight by municipality for full term births") +
    guides(col = guide_legend(override.aes = list(alpha = 1, size = 10, linetype = 0, fill = NA)))

ggsave(filename = "hathaway/results/elevation_race_delivtype_fullterm.png", width = 15, height = 8)


#### Make Year Data Sets  ####
for (i in 2008:2015){
  
  path <- paste0("data/byyear/snsc_",i,".Rds")
  dat_year <- snsc %>% 
    as.tibble() %>%
    filter(birth_year == i) %>%
    mutate(m_muni_code = as.character(m_muni_code),
           birth_muni_code = as.character(birth_muni_code))
  write_rds(dat_year, path)
  #  osfr::upload_file(id = "nxh36", path = path, dest = path)
  print(i)
  
}