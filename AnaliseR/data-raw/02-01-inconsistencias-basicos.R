library(tidyverse)
da_basic_transform <- readr::read_rds("../dados/processados/da_basic_transform.rds")

assuntos <- readr::read_rds('../dados/processados/da_assuntos.rds') %>% dplyr::distinct()
sgt_assunto <- readr::read_delim(file = '../dados/brutos/sgt_assuntos.csv',
                                  delim = ';',col_types = readr::cols(cod_filhos = 'c')) %>%
  dplyr::transmute(codigo,dscr= stringr::str_squish(descricao))

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

# número incoerente com tribunal / justiça --------------------------------
inc_numero_justica_tribunal_fun <- function(da) {
  message("justica")

  da %>%
    mutate(id_justica = case_when(
      justica == "estadual" ~ "8",
      justica == "federal" ~ "4",
      justica == "trabalho" ~ "5",
      justica == "militar" ~ "9",
      justica == "eleitoral" ~ "6",
      justica == "superiores" & tribunal == "STJ" ~ "3",
      justica == "superiores" & tribunal == "TST" ~ "5",
      justica == "superiores" & tribunal == "STM" ~ "7",
      TRUE ~ NA_character_
    )) %>%
    left_join(
      select(forosCNJ::da_tribunal, sigla, id_tribunal),
      c("tribunal" = "sigla")
    ) %>%
    mutate(
      justica_nproc = str_sub(numero, 14L, 14L),
      tribunal_nproc = str_sub(numero, 15L, 16L)
    ) %>%
    transmute(
      id,
      inc_justica = case_when(
        is.na(id_justica) ~ "Justiça inválida",
        (justica_nproc != id_justica) & tribunal != "STJ" ~ "Justiça inválida",
        is.na(id_tribunal) ~ "Tribunal inválido",
        (tribunal_nproc != id_tribunal) & !tribunal %in% c("STJ", "TST", "STM") ~ "Tribunal inválido"
      ),
      sol_justica = paste0(justica_nproc, tribunal_nproc),
      sol_justica = case_when(
        str_detect(sol_justica, "^0") ~ NA_character_,
        TRUE ~ sol_justica
      )
    ) %>%
    filter(!is.na(inc_justica))

}

# classe fora da SGT ------------------------------------------------------


inc_classe_fun <- function(da) {

  message("classe")

  classe_sgt <- readr::read_delim(
    delim = ";",
    "../dados/brutos/sgt_classes.csv",
    col_types = cols(.default = col_character())
  ) %>%
    distinct(codigo) %>%
    filter(!is.na(codigo))

  da %>%
    transmute(
      id,
      info_classe = classe_processual,
      inc_classe = case_when(
        is.na(classe_processual) ~ "Vazio",
        !classe_processual %in% classe_sgt$codigo ~ "Classe fora da SGT"
      )
    ) %>%
    filter(!is.na(inc_classe))
}

# digito verificador ------------------------------------------------------

calc_dig <- function(num) {
  NNNNNNN <- substr(num, 1L, 7L)
  AAAA <- substr(num, 8L, 11L)
  JTR <- substr(num, 12L, 14L)
  OOOO <- substr(num, 15L, 18L)
  n1 <- sprintf("%02d", as.numeric(NNNNNNN)%%97)
  n2 <- sprintf("%02d", as.numeric(sprintf("%s%s%s", n1, AAAA, JTR))%%97)
  sprintf("%02d", 98-((as.numeric(sprintf("%s%s", n2, OOOO))*100)%%97))
}

inc_digito_fun <- function(da) {
  message("digito")

  da %>%
    mutate(
      dig = str_sub(numero, 8, 9),
      num_sub = paste0(str_sub(numero, 1L, 7L), str_sub(numero, 10L, 20L)),
      dig_calculado = calc_dig(num_sub),
      inc_digito = case_when(
        is.na(numero) ~ "Vazio",
        dig != dig_calculado ~ "Dígito inválido"
      )
    ) %>%
    transmute(
      id,
      info_digito = numero,
      sol_digito = dig_calculado,
      inc_digito
    )
}


# data ajuizamento --------------------------------------------------------

inc_data_ajuizamento_fun <- function(da) {
  message("data de ajuizamento")
  da %>%
    mutate(
      data = data_ajuizamento,
      data = str_sub(data, 1L, 8L),
      data = lubridate::ymd(data, quiet = TRUE),
      ano_cnj = str_sub(numero, 10L, 13L)
    ) %>%
    mutate(inc_data = case_when(
      is.na(data) ~ "Data em formato incorreto",
      ano_cnj != lubridate::year(data) ~ "Ano de ajuizamento diferente de ano do processo"
    )) %>%
    transmute(
      id,
      info_data = data_ajuizamento,
      inc_data
    )
}

# sistema com código bugado -----------------------------------------------
# 1 – Pje, 2 – Projudi, 3 – SAJ, 4 – EPROC, 5 – Apolo, 6 – Themis, 7 – Libra, 8 – Outros;

inc_sistema_fun <- function(da) {
  message("sistema")
  da %>%
    group_by(justica, tribunal) %>%
    mutate(
      n_sistema = length(dsc_sistema),
      sistema_provavel = dsc_sistema[which.max(n_sistema)]
    ) %>%
    ungroup() %>%
    mutate(sistema_provavel = case_when(
      sistema_provavel %in% 1:8 ~ sistema_provavel,
      str_detect(tribunal, "TRE|TRT") ~ "1",
      str_detect(tribunal, "TJAL") ~ "3",
      TRUE ~ NA_character_
    )) %>%
    mutate(inc_sistema = case_when(
      is.na(dsc_sistema) ~ "Vazio",
      !dsc_sistema %in% 1:8 ~ "Sistema inconsistente"
    )) %>%
    transmute(
      id,
      info_sistema = dsc_sistema,
      inc_sistema,
      sol_sistema = sistema_provavel
    ) %>%
    filter(!is.na(inc_sistema))
}

# proc eletrônico com código bugado ---------------------------------------

inc_proc_el_fun <- function(da) {
  message("eletronico")
  da %>%
    mutate(
      ano_distribuicao = str_sub(numero, 10, 13),
      sol_eletronico = if_else(ano_distribuicao >= 2015, "1", "2")
    ) %>%
    mutate(inc_eletronico = case_when(
      is.na(proc_el) ~ "Vazio",
      !proc_el %in% 1:2 ~ "Código de processo eletrônico inconsistente"
    )) %>%
    transmute(
      id,
      info_eletronico = proc_el,
      inc_eletronico,
      sol_eletronico
    ) %>%
    filter(!is.na(inc_eletronico))
}


# valor da causa aberrante ------------------------------------------------

inc_valor_fun <- function(da) {
  message("valor")
  da %>%
    group_by(justica, tribunal) %>%
    mutate(valor_corte = quantile(valor_causa, probs = .99, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(inc_valor = case_when(
      valor_causa < 0 ~ "Valor da causa menor que zero",
      valor_causa > valor_corte ~ "Valor acima do esperado para o tribunal"
    )) %>%
    transmute(
      id,
      info_valor = valor_causa,
      inc_valor
    ) %>%
    filter(!is.na(inc_valor))
}


# orgao vazio ou inconsistente --------------------------------------------

inc_orgao_fun <- function(da) {

  message("orgao")

  orgaos_base <- readr::read_delim(
    delim = ";",
    "../dados/brutos/mpm_serventias.csv",
    col_types = cols(.default = col_character())
  ) %>%
    distinct(codigo = SEQ_ORGAO) %>%
    filter(!is.na(codigo))

  da %>%
    transmute(
      id,
      info_orgao = codigo_orgao,
      inc_orgao = case_when(
        is.na(codigo_orgao) ~ "Vazio",
        !codigo_orgao %in% orgaos_base$codigo ~ "Classe fora do Anexo II CNJ"
      )
    ) %>%
    filter(!is.na(inc_orgao))
}

# codigo ibge incoerente --------------------------------------------------

# mapa <- geobr::read_municipality(year = 2019)
# readr::write_rds(mapa, "data-raw/p02_01_mapa_brasil.rds", compress = "xz")

library(sf)

inc_municipio_fun <- function(da) {
  message("municipio")

  mapa <- readr::read_rds("data-raw/p02_01_mapa_brasil.rds") %>%
    mutate(code_muni = as.character(code_muni))


  da %>%
    transmute(
      id,
      numero,
      code_muni = str_pad(codigo_municipio_ibge, 7, "left", "0")
    ) %>%
    anti_join(mapa, "code_muni") %>%
    mutate(
      id_justica = str_sub(numero, 14, 14),
      id_tribunal = str_sub(numero, 15, 16),
      id_foro = str_sub(numero, 17, 20)
    ) %>%
    left_join(forosCNJ::da_foro_comarca, c("id_justica", "id_tribunal", "id_foro")) %>%
    transmute(
      id,
      info_municipio = code_muni,
      inc_municipio = "Código do município não é compatível com base do IBGE",
      sol_municipio = ibge
    )
}

# Assunto -----------------------------------------------------------------


# assuntos genéricos, fora da sgt, sem assunto principal ------------------------------------------------------
inc_assuntos_fun <- function(da_assunto,sgt_assunto){

  # Assuntos genéricos
  generico <- c('9985','12734','899	','9633','12480','1156','864','11428','10739','6191',
                '1146','287','11068','195','8826','1209','11049','14','12467','7724')
  ass <- assuntos %>%
    dplyr::select(-file,-descricao) %>%
    tidyr::pivot_longer(cols = -c(file_json,rowid,principal),names_to = 'tipo_codigo',values_to = 'codigo') %>%
    dplyr::left_join(sgt_assunto,'codigo') %>%
    dplyr::transmute(file_json,
                     rowid,
                     dscr,
                     info_assunto = codigo,
                     inc_nao_e_assunto_principal = ifelse((!principal | is.na(principal)), 'Nenhum assunto indicado como "principal"', ''),
                     inc_assunto_generico = ifelse(info_assunto %in% generico | is.na(info_assunto), 'Assunto genérico', ''),
                     inc_assunto_nao_bate_com_tpu = ifelse(is.na(dscr) & !is.na(info_assunto), 'Código do assunto não bate com a TPU', ''),
                     inc_assunto_vazio = ifelse(is.na(info_assunto), 'Assunto vazio', ''),
                     dscr = dplyr::case_when(is.na(dscr) & is.na(info_assunto)~NA_character_,
                                             is.na(dscr) & !is.na(info_assunto)~'(Vazio)',
                                             TRUE ~dscr)) %>%
    dplyr::group_by(file_json,rowid) %>%
    dplyr::summarise(

      info_assunto = paste0(na.exclude(info_assunto),collapse = ', '),

      # nao eh assunto principal
      inc_principal = ifelse(info_assunto == '', '',min(inc_nao_e_assunto_principal)),
      info_principal = info_assunto,

      # assunto generico
      inc_generico = ifelse(info_assunto == '', '',min(inc_assunto_generico)),
      info_generico = info_assunto,

      # assunto tpu
      inc_assunto_tpu = max(inc_assunto_nao_bate_com_tpu),
      info_assunto_tpu = info_assunto,

      # assunto vazio
      inc_assunto_vazio = min(inc_assunto_vazio)

    ) %>%
    dplyr::select(-info_assunto) %>%
    dplyr::filter_at(dplyr::vars(dplyr::starts_with('inc')),dplyr::any_vars(. != '')) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with('inc'),~ifelse(.x=='',NA_character_,.x)))

  return(ass)
}

# combinacoes raras classe-assunto ----------------------------------------
inc_classe_assunto_fun <- function(da_assunto,da_basic,sgt_assunto){
  da_assunto %>%
    dplyr::select(-file,-descricao,-principal) %>%
    dplyr::left_join(dplyr::select(da_basic,file_json,rowid,classe_processual),c('file_json','rowid')) %>%
    tidyr::pivot_longer(cols = -c(file_json,rowid,classe_processual),names_to = 'tipo_codigo',values_to = 'codigo') %>%
    dplyr::left_join(sgt_assunto,'codigo') %>%
    dplyr::filter(!is.na(codigo)) %>%
    dplyr::group_by(classe_processual,codigo) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(inc_classe_assunto_raro = ifelse(n <= quantile(n,0.05),'Combinação rara de classe e assunto','')) %>%
    dplyr::filter(inc_classe_assunto_raro != "") %>%
    dplyr::select(
      file_json,
      rowid,
      info_classe_assunto_raro_classe = classe_processual,
      info_classe_assunto_raro_tpassunto = tipo_codigo,
      info_classe_assunto_raro_assunto = codigo,
      info_classe_assunto_raro_descr = dscr,
      inc_classe_assunto_raro
    )
}

tab_assunto <- inc_assuntos_fun(da_assunto = assuntos,sgt_assunto = sgt_assunto)
tab_classe_assunto = inc_classe_assunto_fun(
  da_assunto = assuntos,
  da_basic = da_basic_transform,
  sgt_assunto = sgt_assunto
)


# movimentação deveria ser do magistrado mas é de servidor ----------------
inc_responsavel_mov_fun <- function(mov) {
  message('responsável movimentação')

  mov %>%
    dplyr::mutate(
      inc_responsavel_mov = case_when(
        (tipoResponsavelMovimento == '1' &
         !(movimentoLocal.codigoPaiNacional %in% c('1', '12524'))) |
        (tipoResponsavelMovimento == '0' &
         movimentoLocal.codigoPaiNacional %in% c('1', '12524'))
        ~ 'Movimentação Com Responsável Incorreto'
      ),
      sol_responsavel_mov = case_when(
        movimentoLocal.codigoPaiNacional %in% c('1', '12524') ~ '1',
        !(movimentoLocal.codigoPaiNacional %in% c('1', '12524')) ~ '0'
      )
    ) %>%
    transmute(
      id,
      inc_responsavel_mov,
      info_responsavel_mov = tipoResponsavelMovimento,
      sol_responsavel_mov
    ) %>%
    filter(!is.na(inc_responsavel))
}

# tempo de movimentação muito demorado -------------------------------------
inc_mov_demorada_fun <- function(mov) {
  message('movimentação muito demorada')

  mov %>%
    dplyr::mutate(
      data = dataHora,
      data = str_sub(data, 1L, 8L),
      data = lubridate::ymd(data, quiet = TRUE)
    ) %>%
    dplyr::group_by(file_json, rowid) %>%
    dplyr::arrange(file_json, rowid, desc(data)) %>%
    dplyr::select(id, data, dataHora, file_json, rowid) %>%
    dplyr::mutate(movimentacao_demorada = -1*as.numeric(lead(data) - data)) %>%
    dplyr::mutate(
      inc_mov_demorada = case_when(
        movimentacao_demorada > 1825 ~
        'Tempo desde última movimentação acima de 5 anos.'
      )
    ) %>%
    ungroup() %>%
    dplyr::transmute(
      id,
      inc_mov_demorada,
      info_mov_demorada = dataHora,
    ) %>%
    filter(!is.na(inc_mov_demorada))
}

# código pai faltante na movimentação local --------------------------------
inc_cod_pai_faltante_fun <- function (mov) {
  message('código pai faltante')

  sgt_movs <- '../dados/brutos/sgt_movimentos.csv' %>%
    readr::read_delim(
      delim = ';', col_types = readr::cols(.default = 'c')
    ) %>%
    dplyr::transmute(
      codigo,
      dscr = stringr::str_squish(descricao),
      cod_pai = str_replace(cod_pai, '(\\.0)$', ''),
      cod_filhos
    )

  mov %>%
    dplyr::select(
      id, movimentoNacional.codigoNacional, movimentoLocal.codigoPaiNacional
    ) %>%
    filter(is.na(movimentoLocal.codigoPaiNacional)) %>%
    dplyr::mutate(
      inc_cod_pai_faltante = 'Não há código pai da movimentação local.'
    ) %>%
    dplyr::left_join(
      select(sgt_movs, codigo, cod_pai), by = c(
        'movimentoNacional.codigoNacional' = 'codigo'
      )
    ) %>%
    dplyr::transmute(
      id,
      inc_cod_pai_faltante,
      info_cod_pai_faltante = movimentoLocal.codigoPaiNacional,
      sol_cod_pai_faltante = cod_pai
    ) %>%
    filter(!is.na(inc_cod_pai_faltante))
}

# apontar processos cuja duração é maior do que o 75º percentil -----------
inc_processo_longo_fun <- function(mov){
  message('processos longos')

  mov %>%
    dplyr::mutate(
      data = dataHora,
      data = str_sub(data, 1L, 8L),
      data = lubridate::ymd(data, quiet = TRUE)
    ) %>%
    dplyr::group_by(file_json, rowid) %>%
    dplyr::arrange(file_json, rowid, desc(data)) %>%
    dplyr::select(id, data, dataHora, file_json, rowid) %>%
    dplyr::mutate(processo_longo = as.numeric(first(data) - last(data))) %>%
    ungroup() %>%
    select(-data) %>%
    distinct_at(vars(-dataHora), .keep_all = TRUE) %>%
    mutate(
      tempo_corte = quantile(processo_longo, probs = .75, na.rm = TRUE),
      inc_processo_longo = case_when(
      processo_longo > tempo_corte ~
      'Duração do processo é maior do que 75% de todos os processos.'
    )) %>%
    transmute(
      id,
      inc_processo_longo,
      info_processo_longo = dataHora,
    ) %>%
    filter(!is.na(inc_processo_longo))
}


# export ------------------------------------------------------------------

list_incos <- ls()[str_detect(ls(), "^inc_") & !str_detect(ls(), 'assunto')] %>%
  purrr::map(~get(.x)(da_basic_transform))

da_inicial <- da_basic_transform %>%
  select(id, rowid, numero, file_json, justica, tribunal)

da_incos <- list_incos %>%
  reduce(left_join, by = "id", .init = da_inicial) %>%
  filter_at(vars(starts_with("inc_")), any_vars(!is.na(.))) %>%
  left_join(tab_assunto,by = c('file_json', 'rowid')) %>%
  left_join(tab_classe_assunto,by = c('file_json', 'rowid'))

readr::write_rds(
  da_incos,
  "data-raw/p02_01_da_incos_basic.rds",
  compress = "xz"
)

glimpse(da_incos)
