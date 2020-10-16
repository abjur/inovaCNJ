library(tidyverse)


# assuntos ----------------------------------------------------------------

assuntos <- readr::read_rds('../dados/processados/da_assuntos.rds') %>% dplyr::distinct()
assuntos_cnj <- readr::read_delim(file = 'data-raw/arquivos_base/sgt_assuntos.csv',
                                  delim = ';',col_types = readr::cols(cod_filhos = 'c')) %>%
  dplyr::transmute(codigo,dscr= stringr::str_squish(descricao))

# Assuntos genéricos
generico <- c('9985','12734','899	','9633','12480','1156','864','11428','10739','6191',
              '1146','287','11068','195','8826','1209','11049','14','12467','7724')

ass <- assuntos %>%
  dplyr::select(-file,-descricao) %>%
  tidyr::pivot_longer(cols = -c(file_json,rowid,principal),names_to = 'tipo_codigo',values_to = 'codigo') %>%
  dplyr::filter(!is.na(codigo)) %>%
  dplyr::left_join(assuntos_cnj,'codigo') %>%
  dplyr::transmute(file_json,
                   rowid,
                   dscr,
                   info_assunto = codigo,
                   inc_nao_e_assunto_principal = ifelse(!principal, 'Nenhum assunto indicado como "principal"', ''),
                   inc_assunto_generico = ifelse(info_assunto %in% generico, 'Assunto genérico', ''),
                   # inc_assunto_vazio = ifelse(is.na(info_assunto), 'Assunto vazio', ''),
                   inc_assunto_nao_bate_com_tpu = ifelse(is.na(dscr) & !is.na(info_assunto), 'Código do assunto não bate com a TPU', '')) %>%
  dplyr::group_by(file_json,rowid) %>%
  dplyr::summarise(info_assunto = paste0(info_assunto,collapse = ', '),
                   info_assunto = stringr::str_remove(info_assunto, ', $'),
                   info_assunto_descr = paste0(dscr,collapse = ', '),
                   info_assunto_descr = stringr::str_remove(info_assunto_descr, ', $'),
                   inc_nao_possui_assunto_principal = min(inc_nao_e_assunto_principal),
                   inc_assunto_generico = min(inc_assunto_generico),
                   # inc_assunto_vazio = min(inc_assunto_vazio),
                   inc_assunto_nao_bate_com_tpu = max(inc_assunto_nao_bate_com_tpu))



b %>% filter(inc_assunto_nao_bate_com_tpu != '') %>% View
