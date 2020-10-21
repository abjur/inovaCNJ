library(tidyverse)

da_incos <- '../dados/processados/da_incos.csv' %>%
  readr::read_csv(col_types = readr::cols(.default = 'c'))
0
mov <- readr::read_rds('../dados/processados/mov_incos.rds')
mov <- mov %>%
  dplyr::mutate(file_json = paste0('../', file_json)) %>%
  dplyr::inner_join(
    dplyr::select(da_incos, file_json, rowid, id),
    by = c('file_json', 'rowid')
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

  sgt_movs <- 'dados/brutos/sgt_movimentos.csv' %>%
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
