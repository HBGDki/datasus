## script to process the SINASC data
##---------------------------------------------------------

source("_fns.R")
source("_sinasc_dict.R")
load("data/geo/state_muni_codes.Rdata")

ff <- list.files("data/datasus/SINASC/DNRES", full.names = TRUE)

# these are the names that are in common across all datasets
nms_keep <- c("NUMERODN", "LOCNASC", "CODESTAB", "CODMUNNASC", "IDADEMAE",
  "ESTCIVMAE", "ESCMAE", "CODOCUPMAE", "QTDFILVIVO", "QTDFILMORT", "CODMUNRES",
  "GESTACAO", "GRAVIDEZ", "PARTO", "CONSULTAS", "DTNASC", "SEXO", "APGAR1",
  "APGAR5", "RACACOR", "PESO", "IDANOMAL", "CODANOMAL")

res <- vector(length = length(ff), mode = "list")

for (ii in seq_along(ff)) {
  message(ii)
  res[[ii]] <- read_and_transform(ff[ii], sinasc_dict, nms_keep)
}

snsc <- bind_rows(res)


# fix weird number fields
snsc$dn_number[9467890] <- NA
snsc$dn_number[9471401] <- NA
snsc$dn_number[9689749] <- NA
snsc$health_estbl_code[9467890] <- NA
# iconv("109\xf33447", "latin1", "UTF-8")
# iconv("105168\xb75", "latin1", "UTF-8")
# iconv("21036\xf719", "latin1", "UTF-8")
# iconv("0\xf300037", "latin1", "UTF-8")

# make apgar codes integer
snsc$apgar1 <- as.integer(snsc$apgar1)
snsc$apgar5 <- as.integer(snsc$apgar5)
snsc$apgar1[snsc$apgar1 == 99] <- NA
snsc$apgar5[snsc$apgar5 == 99] <- NA

# we'll use this variable a lot so we'll add it here
snsc$birth_year <- as.integer(format(snsc$birth_date, "%Y"))

# need to add in state codes (just have municipality codes)
# ac <- nchar(snsc$birth_muni_code)
# table(snsc$birth_year[which(ac == 6)])
# #    2006    2007    2008    2009    2010    2011    2012    2013    2014    2015
# # 2944928 2891328 2934828 2881581 2861868 2913160 2905789 2904027 2979259 3017668
# table(snsc$birth_year[which(ac == 7)])
# #    2001    2002    2003    2004    2005
# # 3115474 3059402 3038251 3026548 3035096

# it turns out that the 7th character of municipality codes doesn't matter?
snsc$birth_muni_code <- substr(snsc$birth_muni_code, 1, 6)
snsc$m_muni_code <- substr(snsc$m_muni_code, 1, 6)

# ubc <- unique(snsc$birth_muni_code)
# umc <- unique(snsc$m_muni_code)

muni_codes$code2 <- substr(muni_codes$muni_code, 1, 6)

# setdiff(ubc, muni_codes$code2)
# setdiff(umc, muni_codes$code2)

# muni <- AmostraBrasil::MUNICIPIOS.IBGE
# names(muni) <- tolower(names(muni))
# muni$municipio <- iconv(muni$municipio, "latin1", "UTF-8")
# muni$code2 <- substr(as.character(muni$codibge), 1, 6)

# setdiff(ubc, muni$code2)
# setdiff(umc, muni$code2)

# setdiff(muni$code2, muni_codes$code2)
# filter(muni, code2 == "220672")
# # uf municipio codibge  code2
# # PI   Nazária 2206720 220672

# ucd <- as.character(unique(a$MUNCOD))
# tmp <- lapply(ucd, function(x) muni_codes$muni_code[grepl(paste0("^", x), muni_codes$muni_code)])
# names(tmp) <- ucd
# idx <- unname(which(sapply(tmp, length) != 1))
# tmp[idx]

# filter(muni_codes, muni_code == "2103703")

snsc <- left_join(snsc, select(muni_codes, code2, state_code), by = c(m_muni_code = "code2"))
snsc <- rename(snsc, m_state_code = state_code)
snsc <- left_join(snsc, select(muni_codes, code2, state_code), by = c(birth_muni_code = "code2"))
snsc <- rename(snsc, birth_state_code = state_code)

# make muni codes integer
snsc$birth_muni_code <- as.integer(snsc$birth_muni_code)
snsc$m_muni_code <- as.integer(snsc$m_muni_code)


# # fix implausible birth weight values
# a <- snsc$brthwt_g
# a <- a[!is.na(a)]
# # length(which(a == 0))
# # # 22385 - this isn't possible!
# a <- a[a != 0]
# # length(which(a == 9999))
# # # 1359 - not possible either!
# a <- a[a != 9999]
# min(a)
# # [1] 53
# max(a)
# # [1] 9400
# a <- sort(a)
# tail(a, 100)
# head(a, 100)

snsc$brthwt_g[snsc$brthwt_g == 0] <- NA
snsc$brthwt_g[snsc$brthwt_g == 9999] <- NA

# fix implausible mother's age values
snsc$m_age_yrs[snsc$m_age_yrs == 0] <- NA
snsc$m_age_yrs[snsc$m_age_yrs == 99] <- NA

save(snsc, file = "data/snsc_all.Rdata")

## extra investigation
##---------------------------------------------------------

# # some of these start with letters, so we can't convert to number
# # just one record has 7 digits:
# snsc$dn_number[nchar(snsc$dn_number) == 7]

# table(nchar(snsc$health_estbl_code))
# table(substr(snsc$health_estbl_code, 1, 1))

# table(nchar(snsc$birth_muni_code))
# table(substr(snsc$birth_muni_code, 1, 1))

# icd10 <- icd::icd10cm2016
# subset(icd10, code == "D180")

## make a data frame of current variables
##---------------------------------------------------------

source("_sinasc_dict.R")

dvars <- lapply(names(sinasc_dict), function(nm) {
  tmp <- sinasc_dict[[nm]]
  data_frame(
    name = nm,
    # label = tmp$label,
    name_en = tmp$name_en,
    label_en = tmp$label_en)
})
dvars <- bind_rows(dvars)

## look at how variables evolve over time
##---------------------------------------------------------

ff <- list.files("data/datasus/SINASC/DNRES", full.names = TRUE, pattern = "DNBA")

d <- lapply(ff, function(f) read.dbc::read.dbc(f))

vrbls <- lapply(seq_along(d), function(x) data_frame(year = x + 2000, name = names(d[[x]])))
vrbls <- bind_rows(vrbls)
vrbls <- left_join(vrbls, dvars)

idx <- which(!is.na(vrbls$name_en))
vrbls$name[idx] <- vrbls$name_en[idx]
vrbls$name <- factor(vrbls$name)
vrbls$name <- forcats::fct_reorder(vrbls$name, vrbls$year, function(x) length(unique(x)))

save(vrbls, file = "data/artifacts/vrbls.Rdata")

ggplot(vrbls, aes(year, name)) +
  geom_point() +
  theme_bw() +
  labs(x = "Year", y = NULL)
