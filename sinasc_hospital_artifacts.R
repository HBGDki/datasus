

# https://www.theatlantic.com/health/archive/2014/04/why-most-brazilian-women-get-c-sections/360589/ - Very good read on why the c-section rate is so high.
# Doctors and activists here say Borges's experience is fairly common among women who give birth in the country’s private hospitals, where 82 percent of all babies are born by C-section. 
# Even in public hospitals, the C-section rate is roughly half of all births. 

# https://motherboard.vice.com/en_us/article/9a38g8/brazil-c-sections-natural-births - Despite a decreasing mortality rate, Brazil ranks alongside Congo and Nigeria as producing the largest number of premature babies in the world. The number of babies born prematurely in Brazil has nearly doubled over the last decade, to 11 percent of all births, according to a 2012 study by the World Health Organization. Researchers see a link between c-section rate and the rise in premature births.
# Vaginal births cost on average $300 in Brazil, while C-sections can go for as much as $5,000, according to the Brazilian Medical Association.
# Meanwhile, home births and doulas, increasingly popular options, are expensive and not covered by insurance.


# So private hospitals and home births may be markers for wealthier families.  


# Analysis Ideas

# Find the c-section proportion for each state.  
# Find the proportion rate for each hospital and assign a hospital as private or public based on this proportion.
# Only use 2011 through 2015 - See email from Ila Falcao on March 19th - There were some changes in the declaration of live birth (document that "feeds" the SINASC) in 2011,


library(tidyverse)

#(cl <- detectCores()-1)
#cluster <- create_cluster(cores = cl)


if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

snsc <- snsc %>% as.tibble()

muni_detail <- readRDS("data/geo/state_muni_latlong_elev_pop2017.Rds") %>%
  select(ibge7_code, ibge6_code, muni_name, state_code, state_name, region_code, region_name) %>%
  rename(birth_muni_code = ibge6_code ,birth_state_code = state_code )

# keep births after 2011 when the measures changed.  See above
dat <- snsc %>% 
  filter(birth_year >= 2011)


## Need to fix the birth_place variable that is different for the same health_estbl_code

# Now build hospital data for just 2011 and 2015

hosp <- dat %>%
  filter(birth_year %in% c(2011, 2015)) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, birth_place, birth_year) %>%
  summarise(n = n(), birth_mean = mean(brthwt_g, na.rm = TRUE)) %>%
  ungroup() 

hosp_race <- dat %>%
  filter(!is.na(race), birth_year %in% c(2011, 2015)) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, birth_place, birth_year, race) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(birth_state_code, birth_muni_code, birth_year, health_estbl_code) %>%
  mutate(perc = n/sum(n)) %>%
  select(-n) %>%
  spread(race, perc, fill = 0)

hosp_deliv <- dat %>%
  filter(!is.na(deliv_type), birth_year %in% c(2011, 2015)) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code,birth_place, birth_year, deliv_type) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(birth_state_code, birth_muni_code, birth_year, health_estbl_code) %>%
  mutate(perc = n/sum(n)) %>%
  select(-n) %>%
  spread(deliv_type, perc, fill = 0)

hosp_ga <- dat %>%
  filter(!is.na(gest_weeks), birth_year %in% c(2011, 2015)) %>%
  mutate(gest_weeks = fct_collapse(gest_weeks,
                                   `Less than 27 weeks` = c("22-27 weeks", "Less than 22 weeks"),
                                   `More than 37 weeks` = c("37-41 weeks", "42 weeks and more"),
                                   `28-31 weeks` = "28 to 31 weeks",
                                   `32-36 weeks` = "32-36 weeks")) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, birth_place, birth_year, gest_weeks) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(birth_state_code, birth_muni_code, birth_year, health_estbl_code) %>%
  mutate(perc = n/sum(n)) %>%
  select(-n) %>%
  spread(gest_weeks, perc, fill = 0)

hosp_year <- hosp %>%
  left_join(hosp_deliv) %>%
  left_join(hosp_race) %>%
  left_join(hosp_ga) %>%
  left_join(muni_detail)

# Now build hospital data for 2011 - 2015 or don't group by year on the dat object

hosp <- dat %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, birth_place) %>%
  summarise(n = n(), birth_mean = mean(brthwt_g, na.rm = TRUE)) %>%
  ungroup() 

hosp_race <- dat %>%
  filter(!is.na(race)) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, birth_place, race) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code) %>%
  mutate(perc = n/sum(n)) %>%
  select(-n) %>%
  spread(race, perc, fill = 0)

hosp_deliv <- dat %>%
  filter(!is.na(deliv_type)) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code,birth_place, deliv_type) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code) %>%
  mutate(perc = n/sum(n)) %>%
  select(-n) %>%
  spread(deliv_type, perc, fill = 0)

hosp_ga <- dat %>%
  filter(!is.na(gest_weeks)) %>%
  mutate(gest_weeks = fct_collapse(gest_weeks,
                                   `Less than 27 weeks` = c("22-27 weeks", "Less than 22 weeks"),
                                   `More than 37 weeks` = c("37-41 weeks", "42 weeks and more"),
                                   `28-31 weeks` = "28 to 31 weeks",
                                   `32-36 weeks` = "32-36 weeks")) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, birth_place, gest_weeks) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code) %>%
  mutate(perc = n/sum(n)) %>%
  select(-n) %>%
  spread(gest_weeks, perc, fill = 0)

hosp <- hosp %>%
  left_join(hosp_deliv) %>%
  left_join(hosp_race) %>%
  left_join(hosp_ga) %>%
  left_join(muni_detail)

hosp$birth_year <- "2011-2015"

hosp <- hosp_year %>%
  mutate(birth_year = as.character(birth_year)) %>%
  bind_rows(hosp)


state_delivtype <- dat %>%
  filter(!is.na(deliv_type), birth_year %in% c(2011:2015)) %>%
  group_by(birth_state_code, birth_year, deliv_type) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(birth_state_code, birth_year) %>%
  mutate(perc = n/sum(n)) %>%
  select(-n) %>%
  spread(deliv_type, perc, fill = 0)

save(state_delivtype, dat, hosp, file = "data/artifacts/hospital_snsc20112015.Rdata")

