library(tidyverse)
library(ggthemes)
library(fs)
# library(plotly)

if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

cat(glimpse(snsc), file = "hathaway/data_notes.md")
