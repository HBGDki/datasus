

#ssh -L 4321:127.0.0.1:4321 -L 8101:127.0.0.1:8101 j.hathaway@52.90.44.172	
#screen
# http://aperiodic.net/screen/quick_reference
# R
#colorout::setOutputColors256(202, 214, 209, 184, 172, 179, verbose = FALSE)
#for rmote
#options(width=140) # works for pushing to surface4 screen full screen

#cat(OSF_PAT="\n",
#    file = file.path(normalizePath("~/"), ".Renviron"), append = TRUE)


rmote::start_rmote(port = 4321)
rmote::rmote_device(width = 1400, height = 800)
library(tidyverse)
library(hbgd) #loads datadr and trelliscope
library(osfr)
setwd("../workspace/datasus")
