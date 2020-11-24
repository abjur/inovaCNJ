library(tidyverse)
devtools::load_all('../App')
da_basic_transform <- readr::read_rds("../dados/processados/da_basic_transform.rds")

assuntos <- readr::read_rds('../dados/processados/da_assuntos.rds') %>% dplyr::distinct()
sgt_assunto <- sgt_assuntos

mov <- readr::read_csv(
  '../dados/processados/mov_incos.csv',
  col_types = readr::cols(.default = 'c')
)
#' esse código cria bases com id e coluna para cada possível inconsistência.
#' as colunas sempre apresentam uma descrição da inconsistência
#' e as vezes apresentam a solução da inconsistencia
#'
#' prefixos:
#' inc_xxx: inconsistência do problema xxx
#' info_xxx: coluna que tem problema xxx
#' sol_xxx: solução do problema xxx

# export ------------------------------------------------------------------

inc_funcs<- ls("package:inovaCNJ")

list_incos <- inc_funcs[str_detect(inc_funcs, "^inc_") & !str_detect(inc_funcs, 'assunto|mov')] %>%
  purrr::map(~get(.x)(da_basic_transform))

tab_assunto <- inc_assuntos_fun(da_assunto = assuntos,sgt_assunto = sgt_assunto)
tab_classe_assunto = inc_classe_assunto_fun(
  da_assunto = assuntos,
  da_basic = da_basic_transform,
  sgt_assunto = sgt_assunto)

tab_incos_movs <- ls()[str_detect(ls(), "^inc_mov")] %>%
  purrr::set_names() %>%
  purrr::map(~get(.x)(mov)) %>%
  purrr::imap(~{
    .x %>%
      dplyr::group_by(id) %>%
      nest() %>%
      ungroup() %>%
      set_names(c("id", .y))
  }) %>%
  reduce(left_join, by = "id", .init = distinct(mov, id)) %>%
  mutate(id = as.integer(id))

da_inicial <- da_basic_transform %>%
  select(id, rowid, numero, file_json, justica, tribunal)

da_incos <- list_incos %>%
  reduce(left_join, by = "id", .init = da_inicial) %>%
  filter_at(vars(starts_with("inc_")), any_vars(!is.na(.))) %>%
  left_join(tab_assunto,by = c('file_json', 'rowid')) %>%
  left_join(tab_classe_assunto, by = c('file_json', 'rowid')) %>%
  left_join(tab_incos_movs, by = "id")

readr::write_rds(
  da_incos,
  "data-raw/p02_01_da_incos_basic.rds",
  compress = "xz"
)

glimpse(da_incos)
