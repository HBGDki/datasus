library(tidyverse)
library(ggthemes)
library(fs)
library(osfr)
library(sp)
library(geosphere)
library(parallel)
#https://www.r-bloggers.com/speed-up-your-code-parallel-processing-with-multidplyr/
library(multidplyr)

#(cl <- detectCores()-1)
#cluster <- create_cluster(cores = cl)


if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

snsc <- snsc %>% as.tibble()

##### Make stratified subset  ######

dat <- snsc %>%
  filter(preg_type == "Singleton", gest_weeks %in% c("37-41 weeks", "42 weeks and more"), birth_year == 2005, sex == "Female", deliv_type == "Vaginal")

write_rds(dat, "data/artifacts/singleton_fullterm_vaginal_female_2005.rds")


muni_dat <- read_rds("data/geo/state_muni_codes_latlong_elev_pop2017.Rds") 

muni_mom <- muni_dat %>%
  select(muni_code, muni_state_name, state_code, region_code, lon, lat, elev_m) %>%
  rename_all(.funs = function(x) paste0(x,"_mom"))  %>%
  rename(m_muni_code = muni_code_mom) 

muni_birth <- muni_dat %>%
  select(muni_code, muni_state_name, state_code, region_code, lon, lat, elev_m, pop2017) %>%
  rename_all(.funs = function(x) paste0(x,"_birth")) %>%
  rename(birth_muni_code = muni_code_birth) %>%
  mutate(birth_muni_code = as.integer(birth_muni_code))

dat_m <- dat %>%
  mutate(birth_muni_code = parse_character(birth_muni_code))
  left_join(muni_birth)

sum(dat_m$birth_muni_code %in% muni_dat$muni_code)


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