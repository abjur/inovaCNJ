library(tidyverse)
devtools::load_all('../App')

da_basic_transform <- readr::read_rds("../dados/processados/da_basic_transform.rds")
# %>%
  # dplyr::semi_join(inovaCNJ::da_incos, by = c('file_json','rowid')) #Essa linha de codigo nao eh necessario, ele soh ta aqui pra agilizar o processo

da_incos <- readr::read_rds("data-raw/p02_01_da_incos_basic.rds")


da_totais_basic <- da_basic_transform %>%
  dplyr::count(justica, tribunal)

da_totais_incos <- da_incos %>%
  dplyr::group_by(justica, tribunal) %>%
  dplyr::summarise(
    dplyr::across(matches("^inc_mov"), ~sum(map_lgl(.x, is.null))),
    dplyr::across(matches("^inc_[a-z]{3}[^_]"), ~sum(!is.na(.x))),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(dplyr::starts_with("inc")) %>%
  dplyr::group_by(justica, tribunal) %>%
  dplyr::summarise(media = mean(value), .groups = "drop")

da_totais <- da_totais_basic %>%
  dplyr::inner_join(da_totais_incos, c("justica", "tribunal")) %>%
  dplyr::group_by(justica) %>%
  dplyr::mutate(
    indice = media/n,
    indice_normalizado = indice / max(indice),
    indice = 1 - indice,
    indice_normalizado = 1 - indice_normalizado,
    ranking = min_rank(1 - indice)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(justica, ranking)

readr::write_rds(da_totais, "data-raw/p02_da_totais.rds")
