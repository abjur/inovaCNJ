da_incos <- readr::read_rds("../AnaliseR/data-raw/p02_01_da_incos_basic.rds")
# app mais rapido?
da_incos <- dplyr::sample_n(da_incos, 100000)
usethis::use_data(da_incos, overwrite = TRUE)
