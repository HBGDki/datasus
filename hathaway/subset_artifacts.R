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

##### Make stratified subset  ######

sampled <- snsc %>%
  filter(preg_type != "Singleton") %>%
  group_by(birth_muni_code, deliv_type, sex, birth_year, race) %>%
  sample_n(5, replace = TRUE) 

sampled <- sampled %>%
  distinct(.keep_all = TRUE)

quantile(gn$n, .9)

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