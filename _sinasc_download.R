## pre-processing scripts to download the SINASC data
##---------------------------------------------------------

source("_fns.R")

tb_uf <- get_tb_uf()

a <- expand.grid(state = tb_uf$state, year = 2013:2001)
a <- apply(a, 1, function(x) paste0("DN", x[1], x[2], ".DBC"))
fin <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/", a)
fout <- paste0("data/datasus/SINASC/DNRES/", a)

download.file(fin[1], destfile = "/tmp/test.DBC")

for (i in 2:length(fout))
  download.file(fin[i], destfile = fout[i], mode = "wb")
