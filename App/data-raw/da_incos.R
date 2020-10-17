library(magrittr)
da_incos <- readr::read_rds("../AnaliseR/data-raw/p02_01_da_incos_basic.rds")
## testar app mais rapido?
set.seed(42)
da_incos <- dplyr::sample_n(da_incos, 100000)
usethis::use_data(da_incos, overwrite = TRUE)


da_totais <- readr::read_rds("../AnaliseR/data-raw/p02_da_totais.rds")
usethis::use_data(da_totais, overwrite = TRUE)


set.seed(1)
da_sample <- readr::read_rds("../dados/processados/da_basic_transform.rds") %>%
  dplyr::sample_n(30)

usethis::use_data(da_sample, overwrite = TRUE)
#
