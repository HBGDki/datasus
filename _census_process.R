
source("_fns.R")
source("_census_dict.R")
load("data/geo/state_muni_codes.Rdata")
muni_codes$code2 <- substr(muni_codes$muni_code, 1, 6)

census <- list()

for (type in names(census_dict)) {
  message(type)
  # type <- names(census_dict)[[1]]
  ff <- list.files("data/datasus/IBGE/censo", full.names = TRUE, pattern = type)
  nms_keep <- names(census_dict[[type]])
  res <- list()
  for (ii in seq_along(ff)) {
    message(ii)
    res[[ii]] <- read_and_transform(ff[ii], census_dict[[type]], nms_keep)
  }
  tmp <- bind_rows(res)

  tmp <- left_join(tmp, select(muni_codes, code2, state_code), by = c(muni_code = "code2"))
  tmp$state_code <- factor(tmp$state_code)

  # make muni codes integer
  tmp$muni_code <- as.integer(tmp$muni_code)

  census[[type]] <- tmp
}

census$RENDABR$situation <- NULL

lapply(census, nrow)

save(census, file = "data/census.Rdata")


##
##---------------------------------------------------------

# this has total population, sex, urban/rural, age group for each municipality / year
# see docs/populacao_en
ff <- list.files("data/datasus/IBGE/POP", recursive = TRUE, full.names = TRUE, pattern = "\\.csv")
a <- readr::read_csv(ff[1])


# this just has total population for each municipality / year
# see docs/populacaoTCU_en
ff <- list.files("data/datasus/IBGE/POPTCU", recursive = TRUE, full.names = TRUE, pattern = "\\.csv")
a <- readr::read_csv(ff[1])




## extra stuff
##---------------------------------------------------------

# tmp2 <- lapply(ff, function(x) foreign::read.dbf(x))
# tmp2 <- bind_rows(tmp2)
# table(tmp2$IDADE, useNA = "always")
# table(tmp2[[3]]$ESCOLARID, useNA = "always")

#  0000  0103  0407  0899  ALFA  IGNO  <NA>
# 71475 72701 76054 67027 10936 30608     0
#  0003  0407  0899  IGNO  <NA>
# 80530 71397 78677 68744     0
#  0000  0103  0407  0899  IGNO  <NA>
# 55929 52861 53168 43536  3228     0

# tmp <- bind_rows(res)
table(tmp$race, useNA = "always")
table(tmp$sex, useNA = "always")
table(tmp$situation, useNA = "always")
table(tmp$age_group, useNA = "always")

# ESCABR and ESCBBR
table(tmp$schooling, useNA = "always") ###
# these need to be normalized across years
