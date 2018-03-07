## pre-processing script to generate geo shape and code files
##-----------------------------------------------------------

# devtools::install_github("hafen/geofacet@feature-hexmapr")
# devtools::install_github("jbaileyh/geogrid")
library(hexmapr)
library(geofacet)

ff <- list.files("data/geo/municipalities/data/", full.names = TRUE)

muni_shp <- lapply(ff, read_polygons)
names(muni_shp) <- gsub(".*\\/(.*)\\.json", "\\1", ff)

br_shp <- muni_shp$Brasil
muni_shp$Brasil <- NULL

save(br_shp, muni_shp, file = "data/geo/state_muni_shapefiles.Rdata")

muni_codes <- dplyr::bind_rows(lapply(muni_shp, function(x) x@data))
names(muni_codes) <- c("muni_code", "muni_name", "state_code")
muni_codes <- muni_codes[!duplicated(muni_codes), ]

state_codes <- br_shp@data
names(state_codes) <- c("state_code", "state_name", "region_code")
for (ii in seq_along(state_codes))
  state_codes[[ii]] <- as.character(state_codes[[ii]])

rgnm <- c(
  CO = "Center-West",
  NE = "Northeast",
  NO = "North",
  SE = "Southeast",
  SU = "South"
)
state_codes$region_name <- unname(rgnm[as.character(state_codes$region_code)])

save(muni_codes, state_codes, file = "data/geo/state_muni_codes.Rdata")
