

#ssh -L 4321:127.0.0.1:4321 -L 8101:127.0.0.1:8101 j.hathaway@34.229.214.116	
#screen
# http://aperiodic.net/screen/quick_reference
# R
#colorout::setOutputColors256(202, 214, 209, 184, 172, 179, verbose = FALSE)
#for rmote
#options(width=140) # works for pushing to surface4 screen full screen


rmote::start_rmote(port = 4321)
rmote::rmote_device(width=1900,height=1000)
library(tidyverse)
library(hbgd) #loads datadr and trelliscope
setwd("../workspace/datasus")
