library(tidyverse)
library(ggthemes)
library(fs)
library(osfr)
library(sp)
library(geosphere)

  

# osf login
login(pat = "")
download_files(id = "7buxm",  path = "data/", private = TRUE)

muni_dat <- read_rds("data/geo/state_muni_codes_latlong_elev_pop2017.Rds") 

muni_mom <- muni_dat %>%
  select(muni_code, muni_state_name, state_code, region_code, lon, lat, elev_m) %>%
  rename_all(.funs = function(x) paste0(x,"_mom"))  %>%
  rename(m_muni_code = muni_code_mom) %>%
  mutate(m_muni_code = as.integer(m_muni_code))

muni_birth <- muni_dat %>%
  select(muni_code, muni_state_name, state_code, region_code, lon, lat, elev_m, pop2017) %>%
  rename_all(.funs = function(x) paste0(x,"_birth")) %>%
  rename(birth_muni_code = muni_code_birth) %>%
  mutate(birth_muni_code = as.integer(birth_muni_code))



snsc_full <- snsc_grouped %>%
  left_join(muni_mom) %>%
  left_join(muni_birth)

baby_dist <- geosphere::distm(select(snsc_full,lon_birth, lat_birth), select(snsc_full,lon_mom, lat_mom))

#library(geosphere)
#distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)

muni_dat %>%
  group_by(state_code, state_name, region_code, region_name) %>%
  summarise(elev_m_weighted = weighted.mean(elev_m, pop2017, na.rm = TRUE), 
            elev_m = mean(elev_m, na.rm = TRUE), pop2017 = sum(pop2017, na.rm = TRUE)) %>%
  arrange(region_name, state_name) %>%
  data.frame()

#wget ftp://ftp.unidata.ucar.edu/pub/udunits/udunits-2.2.26.tar.gz
#tar zxf udunits-2.2.26.tar.gz
#cd ./udunits-2.2.26/
#./configure
# make
# sudo make install
# sudo ldconfig
# 


# https://www.sciencedaily.com/releases/2009/05/090518101908.htm
# https://www.ncbi.nlm.nih.gov/pubmed/7068485
# https://www.thebump.com/a/babies-born-at-high-altitudes-weigh-less
# https://www.babycenter.com/404_is-it-true-that-babies-born-at-high-altitudes-weigh-less_10304419.bc
#    * In Colorado, for example, birth weight declines an average of just over 3.5 ounces per 3,300 feet of elevation.
#    
#    3.5/96 = 0.0365 percent

# The normal weight of a baby who reaches full term between 37 and 40 weeks is 2.7-4.1kg (6 - 9 lbs), with an average weight of 3.5kg (7.7 lbs). A baby who weighs less than 2.5kg (5.5 lbs) is considered to have a low birth weight.Dec 12, 2017
# 
# 
# Smoking and altitude is a double whammy http://www.scielo.br/scielo.php?script=sci_arttext&pid=S0034-89102016000200313
# 



#### Push Lot's of Graphs ######

# 
# for (i in 2015:2001){
#   
#   for (state_use in c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
#                       "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", 
#                       "RO", "RR", "SC", "SP", "SE", "TO") ){
#     
#     #state_use <- "SP"
#     #dat_year <- read_rds(paste0("data/byyear/snsc_", i, ".Rds"))
#     dat_year <- snsc %>% 
#       as.tibble() %>%
#       filter(birth_year == i) %>%
#       mutate(m_muni_code = as.character(m_muni_code),
#              birth_muni_code = as.character(birth_muni_code))
#     
#     snsc_yearn <- dat_year %>%
#       filter(!is.na(race), !is.na(sex), !is.na(deliv_type), !is.na(gest_weeks), !is.na(birth_state_code), preg_type == "Singleton") %>%
#       filter(birth_state_code %in% c(state_use)) %>%
#       group_by(race, gest_weeks, deliv_type, sex) %>%
#       summarise(n = n()) %>%
#       ungroup() %>%
#       mutate(deliv_count = str_c(str_sub(deliv_type, 1, 1), n)) %>%
#       group_by(race, gest_weeks, sex) %>%
#       summarise(deliv_count = paste(deliv_count, collapse = "\n")) %>%
#       ungroup()
#     
#     
#     
#     plot_year <- dat_year %>%
#       filter(!is.na(race), !is.na(sex), !is.na(deliv_type), !is.na(gest_weeks), !is.na(birth_state_code), preg_type == "Singleton") %>%
#       filter(birth_state_code %in% c(state_use)) %>%
#       ggplot(aes(x = gest_weeks, y = brthwt_g)) +
#       geom_boxplot(aes(color = deliv_type)) +
#       scale_color_manual(values = c("black", "darkgrey")) +
#       coord_cartesian(ylim = c(0, 7000)) +
#       facet_grid(sex~race) +
#       geom_hline(yintercept = 2500, linetype = "dashed", color = "darkgrey") +
#       geom_text(data = snsc_yearn, aes(label = deliv_count), y = 6200, size = 2.5) +
#       theme_bw() +
#       theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
#       labs(x = "Gestational Weeks at Birth", y = "Weight at birth (grams)", color = "Delivery\ntype",
#            title = paste0("Birth weight distribution for singleton births in ", i), subtitle = paste0(state_use, ", Brazil"))
#            
#     ggsave(plot_year,filename = paste0("hathaway/results/","brazil_", state_use, "_", i, ".png"), width = 15, height = 10)
#     print(i)
#            
#   } # end state
# } # end year loop
