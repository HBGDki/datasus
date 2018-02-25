library(tidyverse)
library(ggthemes)
library(fs)
#https://github.com/rCarto/photon
library(photon)
library(ggmap)
#https://github.com/ropensci/geonames
library(geonames)
library(osfr)

#' @title Get lat, long, and elevation
#' @description For a specified location name get the latitude, longitude, and elevation. Requires the libraries photon and geonames.
#' @param x is a location
#' @author J. Hathaway
#' @return A data.frame with lat, long, and elevation.
#' @export 

spat_details <- function(x){
  
  city <- str_split(x, ", ")[[1]][1]
  location <- photon::geocode(x)
  
  if(nrow(location) > 1){
    
    location_out <- location %>% 
      filter(osm_type == "R", osm_key == "place")
    if(nrow(location_out) == 1) location <- location_out
  }
  location <- location[1,]
  height <- GNsrtm3(lat = location$lat, lng = location$lon) # returns meter height
  location$elev_m <- height$srtm3
  location$elev_f <- location$elev_m * 3.28084
  print(x)
  print(height)
#   cat(paste(height$srtm3, location$lat, location$lon, x, "\n", sep = "_"), file = "hathaway/data_notes_srtm3.md", append = TRUE)
  location
}

# on windows machine.  Couldn't get package to install on linux.
#library(brazilmaps)

#brazil_pop <- pop2017 %>%
# mutate( muni_code = as.character(mun)) %>%
# as.tibble() %>%
# select(-mun)

#write_csv(brazil_pop, "hathaway/brazil_pop.csv")



#osf login
login(pat = "")

# for geonames package
#options(geonamesUsername="")
#


brazil_pop <- read_csv("hathaway/brazil_pop.csv", col_types = 
                         cols(nome_mun = col_character(),
                              pop2017 = col_integer(),
                              muni_code = col_character()))
brazil_codes <- read_csv("data/artifacts/Brazil_Municipal_Identifiers.csv")

brazil_segregation <- read_csv("data/artifacts/Segregation.csv")

brazil_extra <- brazil_codes %>% left_join(brazil_segregation)


load("data/geo/state_muni_codes.Rdata")

# Two of the Brazilian states are misnamed in the data.
# https://en.wikipedia.org/wiki/States_of_Brazil
locales <- muni_codes %>%
  left_join(state_codes) %>% 
  left_join(brazil_pop) %>%
  as.tibble() %>%
  mutate(state_name_fixed = str_replace_all(state_name, "Paraba", "Parabia"),
         state_name_fixed = str_replace_all(state_name_fixed, "Piau", "Piaui"),
    muni_state_name = paste(muni_name, state_name_fixed, sep = ", "))


spatial_locales <- locales %>%
  arrange(desc(muni_state_name)) %>%
  split(.$muni_state_name) %>%
  #.[length(.):1] %>%
  map(~spat_details(.x$muni_state_name))

spatial_locales_tbl <- bind_rows(spatial_locales) %>%
  mutate(muni_state_name = names(spatial_locales)) %>%
  as.tibble()

dat <- locales %>% 
  left_join(spatial_locales_tbl) 

# one elevation was wrong
# https://www.google.com/maps/place/Pilar+-+State+of+Para?ba,+Brazil/@-7.2856667,-35.3413584,12z/data=!3m1!4b1!4m5!3m4!1s0x7ac6054ba1d50ab:0x6ed1016faf316902!8m2!3d-7.2787651!4d-35.2697802
dat[dat$muni_state_name == "Pilar, Parabia",c("elev_m", "elev_f")] <- c(35, 115)

# lat longs and elevations that don't come from place or boundary are assumed to be wrong
dat[!dat$osm_key %in% c("place", "boundary"), c("lat", "lon", "elev_m", "elev_f")] <- NA

# dat_fix <- dat %>%
#   filter(osm_key %in% c("boundary"))
# 
# dat_fix %>% select(muni_state_name, elev_m, elev_f, lat, lon, osm_key)

# dat <- read_rds("data/geo/state_muni_codes_latlong_elev_pop2017.Rds")

dat <- dat %>%
  rename(ibge7_code = muni_code) %>%
  mutate(ibge7_code = parse_integer(ibge7_code)) %>%
  left_join(brazil_extra) %>%
  select(ibge7_code, ibge6_code, tse_code, mesoregion, microregion, UF, everything()) %>%
  select(-nome_mun, -name, -housenumber, -street, -city)

write_rds(dat, "data/state_muni_latlong_elev_pop2017.Rds")

osfr::upload_file(id = "nxh36", path = "data/state_muni_latlong_pop2017.Rds", dest = "data/geo/state_muni_codes_latlong_elev_pop2017.Rds")


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