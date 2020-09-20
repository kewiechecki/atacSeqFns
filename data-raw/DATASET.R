## write CrobustaMotifs to data/

source("readMotifs.R")
source("getMotifs.R")
CrobustaMotifs <- mergeMotifs()
usethis::use_data(CrobustaMotifs, overwrite = TRUE)
