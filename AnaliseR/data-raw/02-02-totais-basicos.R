library(tidyverse)

da_basic_transform <- readr::read_rds("../dados/processados/da_basic_transform.rds")
da_incos <- readr::read_rds("data-raw/p02_01_da_incos_basic.rds")


da_totais_basic <- da_basic_transform %>%
  count(justica, tribunal)

da_totais_incos <- da_incos %>%
  group_by(justica, tribunal) %>%
  summarise(
    across(matches("^inc_mov"), ~sum(map_lgl(.x, is.null))),
    across(matches("^inc_[a-z]{3}[^_]"), ~sum(!is.na(.x))),
    .groups = "drop"
  ) %>%
  pivot_longer(starts_with("inc")) %>%
  group_by(justica, tribunal) %>%
  summarise(media = mean(value), .groups = "drop")

da_totais <- da_totais_basic %>%
  inner_join(da_totais_incos, c("justica", "tribunal")) %>%
  group_by(justica) %>%
  mutate(
    indice = media/n,
    indice_normalizado = indice / max(indice),
    indice = 1 - indice,
    indice_normalizado = 1 - indice_normalizado,
    ranking = min_rank(1 - indice)
  ) %>%
  ungroup() %>%
  arrange(justica, ranking)

readr::write_rds(da_totais, "data-raw/p02_da_totais.rds")
