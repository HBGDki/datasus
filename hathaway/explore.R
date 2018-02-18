library(tidyverse)
library(ggthemes)
library(fs)
#https://github.com/rCarto/photon
library(photon)
library(ggmap)
#https://github.com/ropensci/geonames
library(geonames)
library(brazilmaps)

brazil_pop <- pop2017 %>%
  mutate( muni_code = as.character(mun)) %>%
  as.tibble() %>%
  select(-mun)

write_csv(brazil_pop, "hathaway/brazil_pop.csv")

if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

load("data/geo/state_muni_codes.Rdata")

locales <- muni_codes %>%
  left_join(state_codes) %>% 
  left_join(brazil_pop) %>%
  as.tibble() %>%
  mutate(muni_state_name = paste(muni_name, state_name, sep = ", "))



#glimpse(snsc)

options(geonamesUsername="hathawayj")


spat_details <- function(x){
  
  location <- photon::geocode(x)[1,]
  height <- GNgtopo30(lat = location$lat, lng = location$lon) # returns meter height
  location$elev_m <- height$gtopo30
  location$elev_f <- location$elev_m * 3.28084
  print(x)
  print(height)
  cat(paste(location$lat, location$lon, x, "\n", sep = "_"), file = "hathaway/data_notes.md", append = TRUE)
  location
}

spatial_locales <- locales %>%
  arrange(muni_state_name) %>%
  split(.$muni_state_name) %>%
  map(~spat_details(.x$muni_state_name))

spatial_locales_tbl <- bind_rows(spatial_locales) %>%
  mutate(muni_state_name = names(spatial_locales)) %>%
  as.tibble()

dat <- locales %>% 
  left_join(spatial_locales_tbl) %>%
  filter(!is.na(lat))

write_rds(dat, "data/state_muni_latlong_pop2017.Rds")

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
# https://stackoverflow.com/questions/32504880/street-address-to-geolocation-lat-long
# geocodeAdddress <- function(address) {
#   require(RJSONIO)
#   url <- "http://maps.google.com/maps/api/geocode/json?address="
#   url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
#   x <- fromJSON(url, simplify = FALSE)
#   if (x$status == "OK") {
#     out <- c("lat" = x$results[[1]]$geometry$location$lat, 
#              "lng" = x$results[[1]]$geometry$location$lng)
#   } else {
#     out <- NA
#   }
#   Sys.sleep(0.2)  # API only allows 5 requests per second
#   out
# }