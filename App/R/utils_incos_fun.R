#' inc_numero_justica_tribunal_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_numero_justica_tribunal_fun <- function(da) {
  message("justica")
  r <- da %>%
    dplyr::mutate(id_justica = dplyr::case_when(
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
    dplyr::left_join(
      dplyr::select(forosCNJ::da_tribunal, sigla, id_tribunal),
      c("tribunal" = "sigla")
    ) %>%
    dplyr::mutate(
      justica_nproc = stringr::str_sub(numero, 14L, 14L),
      tribunal_nproc = stringr::str_sub(numero, 15L, 16L)
    ) %>%
    dplyr::transmute(
      id,
      inc_justica = dplyr::case_when(
        is.na(id_justica) ~ "Justiça inválida",
        (justica_nproc != id_justica) & tribunal != "STJ" ~ "Justiça inválida",
        is.na(id_tribunal) ~ "Tribunal inválido",
        (tribunal_nproc != id_tribunal) & !tribunal %in% c("STJ", "TST", "STM") ~ "Tribunal inválido"
      ),
      sol_justica = paste0(justica_nproc, tribunal_nproc),
      sol_justica = dplyr::case_when(
        stringr::str_detect(sol_justica, "^0") ~ NA_character_,
        TRUE ~ sol_justica
      )
    ) %>%
    dplyr::filter(!is.na(inc_justica)) %>%
    dplyr::distinct()

  return(r)

}

#' inc_classe_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
#'
inc_classe_fun <- function(da) {

  message("classe")

  classe_sgt <- sgt_classes %>%
    dplyr::distinct(codigo) %>%
    dplyr::filter(!is.na(codigo))

  r<- da %>%
    dplyr::transmute(
      id,
      info_classe = classe_processual,
      inc_classe = dplyr::case_when(
        is.na(classe_processual) ~ "Vazio",
        !classe_processual %in% classe_sgt$codigo ~ "Classe fora da SGT"
      )
    ) %>%
    dplyr::filter(!is.na(inc_classe)) %>%
    dplyr::distinct()

  return(r)
}

#' calc_dig
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
calc_dig <- function(num) {
  NNNNNNN <- substr(num, 1L, 7L)
  AAAA <- substr(num, 8L, 11L)
  JTR <- substr(num, 12L, 14L)
  OOOO <- substr(num, 15L, 18L)
  n1 <- sprintf("%02d", as.numeric(NNNNNNN)%%97)
  n2 <- sprintf("%02d", as.numeric(sprintf("%s%s%s", n1, AAAA, JTR))%%97)
  sprintf("%02d", 98-((as.numeric(sprintf("%s%s", n2, OOOO))*100)%%97))
}

#' inc_digito_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_digito_fun <- function(da) {
  message("digito")

  r<- da %>%
    dplyr::mutate(
      dig = stringr::str_sub(numero, 8, 9),
      num_sub = paste0(stringr::str_sub(numero, 1L, 7L), stringr::str_sub(numero, 10L, 20L)),
      dig_calculado = calc_dig(num_sub),
      inc_digito = dplyr::case_when(
        is.na(numero) ~ "Vazio",
        dig != dig_calculado ~ "Dígito inválido"
      )
    ) %>%
    dplyr::transmute(
      id,
      info_digito = numero,
      sol_digito = dig_calculado,
      inc_digito
    ) %>%
    dplyr::distinct()

  return(r)
}

#' inc_data_ajuizamento_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_data_ajuizamento_fun <- function(da) {
  message("data de ajuizamento")
  r<- da %>%
    dplyr::mutate(
      data = data_ajuizamento,
      data = stringr::str_sub(data, 1L, 8L),
      data = lubridate::ymd(data, quiet = TRUE),
      ano_cnj = stringr::str_sub(numero, 10L, 13L)
    ) %>%
    dplyr::mutate(inc_data = dplyr::case_when(
      is.na(data) ~ "Data em formato incorreto",
      ano_cnj != lubridate::year(data) ~ "Ano de ajuizamento diferente de ano do processo"
    )) %>%
    dplyr::transmute(
      id,
      info_data = data_ajuizamento,
      inc_data
    ) %>%
    dplyr::distinct()

  return(r)
}

#' inc_sistema_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_sistema_fun <- function(da) {
  message("sistema")
  r<- da %>%
    dplyr::group_by(justica, tribunal) %>%
    dplyr::mutate(
      n_sistema = length(dsc_sistema),
      sistema_provavel = dsc_sistema[which.max(n_sistema)]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sistema_provavel = dplyr::case_when(
      sistema_provavel %in% 1:8 ~ sistema_provavel,
      stringr::str_detect(tribunal, "TRE|TRT") ~ "1",
      stringr::str_detect(tribunal, "TJAL") ~ "3",
      TRUE ~ NA_character_
    )) %>%
    dplyr::mutate(inc_sistema = dplyr::case_when(
      is.na(dsc_sistema) ~ "Vazio",
      !dsc_sistema %in% 1:8 ~ "Sistema inconsistente"
    )) %>%
    dplyr::transmute(
      id,
      info_sistema = dsc_sistema,
      inc_sistema,
      sol_sistema = sistema_provavel
    ) %>%
    dplyr::filter(!is.na(inc_sistema)) %>%
    dplyr::distinct()

  return(r)
}

#' inc_proc_el_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_proc_el_fun <- function(da) {
  message("eletronico")
  r<- da %>%
    dplyr::mutate(
      ano_distribuicao = stringr::str_sub(numero, 10, 13),
      sol_eletronico = dplyr::if_else(ano_distribuicao >= 2015, "1", "2")
    ) %>%
    dplyr::mutate(inc_eletronico = dplyr::case_when(
      is.na(proc_el) ~ "Vazio",
      !proc_el %in% 1:2 ~ "Código de processo eletrônico inconsistente"
    )) %>%
    dplyr::transmute(
      id,
      info_eletronico = proc_el,
      inc_eletronico,
      sol_eletronico
    ) %>%
    dplyr::filter(!is.na(inc_eletronico)) %>%
    dplyr::distinct()

  return(r)
}

#' inc_valor_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_valor_fun <- function(da) {
  message("valor")
  if('valor_causa' %in% names(da)){
    r<- da %>%
      dplyr::group_by(justica, tribunal) %>%
      dplyr::mutate(valor_corte = quantile(valor_causa, probs = .99, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(inc_valor = dplyr::case_when(
        valor_causa < 0 ~ "Valor da causa menor que zero",
        valor_causa > valor_corte ~ "Valor acima do esperado para o tribunal"
      )) %>%
      dplyr::transmute(
        id,
        info_valor = valor_causa,
        inc_valor
      ) %>%
      dplyr::filter(!is.na(inc_valor)) %>%
      dplyr::distinct()
  } else {
    r <- da %>%
      dplyr::transmute(id,
                       info_valor = NA_real_,
                       inc_valor = NA_character_)
  }

  return(r)
}

#' inc_orgao_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_orgao_fun <- function(da) {

  message("orgao")

  r<- da %>%
    dplyr::transmute(
      id,
      info_orgao = codigo_orgao,
      inc_orgao = dplyr::case_when(
        is.na(codigo_orgao) ~ "Vazio",
        !codigo_orgao %in% orgaos_base$codigo ~ "Classe fora do Anexo II CNJ"
      )
    ) %>%
    dplyr::filter(!is.na(inc_orgao)) %>%
    dplyr::distinct()
  return(r)
}

#' inc_municipio_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_municipio_fun <- function(da) {
  message("municipio")

  r<- da %>%
    dplyr::transmute(
      id,
      numero,
      code_muni = stringr::str_pad(codigo_municipio_ibge, 7, "left", "0")
    ) %>%
    dplyr::anti_join(mapa, "code_muni") %>%
    dplyr::mutate(
      id_justica = stringr::str_sub(numero, 14, 14),
      id_tribunal = stringr::str_sub(numero, 15, 16),
      id_foro = stringr::str_sub(numero, 17, 20)
    ) %>%
    dplyr::left_join(forosCNJ::da_foro_comarca, c("id_justica", "id_tribunal", "id_foro")) %>%
    dplyr::transmute(
      id,
      info_municipio = code_muni,
      inc_municipio = "Código do município não é compatível com base do IBGE",
      sol_municipio = ibge
    ) %>%
    dplyr::distinct()

  return(r)
}

#' inc_assuntos_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_assuntos_fun <- function(da_assunto,sgt_assunto){

  # Assuntos genéricos
  generico <- c('9985','12734','899	','9633','12480','1156','864','11428','10739','6191',
                '1146','287','11068','195','8826','1209','11049','14','12467','7724')
  ass <- da_assunto %>% {
    if('descricao' %in% names(.)){
      dplyr::select(.,-descricao)
    } else{
      .
    }
  } %>%
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

#' inc_classe_assunto_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_classe_assunto_fun <- function(da_assunto,da_basic,sgt_assunto){
  r<- da_assunto %>% {
    if('descricao' %in% names(.)){
      dplyr::select(.,-descricao,-principal)
    } else{
      dplyr::select(.,-principal)
    }
  } %>%
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
  return(r)
}

#' inc_mov_responsavel_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_mov_responsavel_fun <- function(mov) {
  message('responsável movimentação')

  if("codigoPaiNacional" %in% names(mov) & "tipoResponsavelMovimento" %in% names(mov)){
    r<- mov %>%
      dplyr::mutate(
        inc_responsavel_mov = dplyr::case_when(
          (tipoResponsavelMovimento == '1' &
             !(codigoPaiNacional %in% c('1', '12524'))) |
            (tipoResponsavelMovimento == '0' &
               codigoPaiNacional %in% c('1', '12524'))
          ~ 'Movimentação Com Responsável Incorreto'
        ),
        sol_responsavel_mov = dplyr::case_when(
          codigoPaiNacional %in% c('1', '12524') ~ '1',
          !(codigoPaiNacional %in% c('1', '12524')) ~ '0'
        )
      ) %>%
      dplyr::transmute(
        file_json,
        rowid,
        inc_responsavel_mov,
        info_responsavel_mov = as.character(tipoResponsavelMovimento),
        sol_responsavel_mov
      ) %>%
      dplyr::filter(!is.na(inc_responsavel_mov)) %>%
      dplyr::distinct()
  } else{
    r <- mov %>%
      dplyr::transmute(file_json,
                       rowid,
                       inc_responsavel_mov = NA_character_,
                       info_responsavel_mov = NA_character_,
                       sol_responsavel_mov = NA_character_)
  }

  return(r)
}

#' inc_mov_demorada_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_mov_demorada_fun <- function(mov) {
  message('movimentação muito demorada')

  if("dataHora" %in% names(mov)){
    r<- mov %>%
      dplyr::mutate(
        data = dataHora,
        data = stringr::str_sub(data, 1L, 8L),
        data = lubridate::ymd(data, quiet = TRUE)
      ) %>%
      dplyr::group_by(file_json, rowid) %>%
      dplyr::arrange(file_json, rowid, dplyr::desc(data)) %>%
      dplyr::select(file_json, rowid, data, dataHora) %>%
      dplyr::mutate(movimentacao_demorada = -1*as.numeric(dplyr::lead(data) - data)) %>%
      dplyr::mutate(
        inc_mov_demorada = dplyr::case_when(
          movimentacao_demorada > 1825 ~
            'Tempo desde última movimentação acima de 5 anos.'
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        file_json,
        rowid,
        inc_mov_demorada,
        info_mov_demorada = dataHora,
      ) %>%
      dplyr::filter(!is.na(inc_mov_demorada)) %>%
      dplyr::distinct()
  } else{
    r <- mov %>%
      dplyr::transmute(
        file_json,
        rowid,
        inc_mov_demorada = NA_character_,
        info_mov_demorada = NA_character_,
      )
  }

  return(r)
}

#' inc_mov_cod_pai_faltante_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_mov_cod_pai_faltante_fun <- function (mov) {
  message('código pai faltante')

  sgt_movs <-sgt_movs %>%
    dplyr::transmute(
      codigo = as.integer(codigo),
      dscr = stringr::str_squish(descricao),
      cod_pai = stringr::str_replace(cod_pai, '(\\.0)$', ''),
      cod_filhos
    )
  if("codigoPaiNacional" %in% names(mov) & "codigoNacional" %in% names(mov)){
    r<- mov %>%
      dplyr::select(
        file_json,
        rowid,
        codigoNacional,
        codigoPaiNacional
      ) %>%
      dplyr::filter(is.na(codigoPaiNacional)) %>%
      dplyr::mutate(
        inc_cod_pai_faltante = 'Não há código pai da movimentação local.'
      ) %>%
      dplyr::left_join(
        dplyr::select(sgt_movs, codigo, cod_pai), by = c('codigoNacional' = 'codigo')
      ) %>%
      dplyr::transmute(
        file_json,
        rowid,
        inc_cod_pai_faltante,
        info_cod_pai_faltante = as.character(codigoPaiNacional),
        sol_cod_pai_faltante = cod_pai
      ) %>%
      dplyr::filter(!is.na(inc_cod_pai_faltante)) %>%
      dplyr::distinct()
  } else{
    r <- mov %>%
      dplyr::transmute(
        file_json,
        rowid,
        inc_cod_pai_faltante = NA_character_,
        info_cod_pai_faltante = NA_character_,
        sol_cod_pai_faltante = NA_character_
      )

  }

  return(r)
}

#' inc_mov_processo_longo_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_mov_processo_longo_fun <- function(mov){
  message('processos longos')

  if("dataHora" %in% names(mov)){
    r<- mov %>%
      dplyr::mutate(
        data = dataHora,
        data = stringr::str_sub(data, 1L, 8L),
        data = lubridate::ymd(data, quiet = TRUE)
      ) %>%
      dplyr::group_by(file_json, rowid) %>%
      dplyr::arrange(file_json, rowid, dplyr::desc(data)) %>%
      dplyr::select(file_json, rowid, data, dataHora) %>%
      dplyr::mutate(processo_longo = as.numeric(dplyr::first(data) - dplyr::last(data))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-data) %>%
      dplyr::distinct_at(dplyr::vars(-dataHora), .keep_all = TRUE) %>%
      dplyr::mutate(
        tempo_corte = quantile(processo_longo, probs = .75, na.rm = TRUE),
        inc_processo_longo = dplyr::case_when(
          processo_longo > tempo_corte ~
            'Duração do processo é maior do que 75% de todos os processos.'
        )) %>%
      dplyr::transmute(
        file_json, rowid,
        inc_processo_longo,
        info_processo_longo = dataHora
      ) %>%
      dplyr::filter(!is.na(inc_processo_longo)) %>%
      dplyr::distinct()
  } else{
    r <- mov %>%
      dplyr::transmute(
        file_json, rowid,
        inc_processo_longo = NA_character_,
        info_processo_longo = NA_character_
      )
  }

  return(r)
}

#' inc_id_mov_segue_ordem_cronologica_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_mov_id_segue_ordem_cronologica_fun <- function(mov){
  message('id de movimentos não corresponde à ordem cronológica das movs')

  if("identificadorMovimento" %in% names(mov) & "dataHora" %in% names(mov)){
    mov_ordenada <- mov %>%
      dplyr::select(file_json, rowid,identificadorMovimento,dataHora) %>%
      dplyr::filter(!is.na(identificadorMovimento)) %>%
      dplyr::distinct(file_json, rowid, identificadorMovimento,.keep_all = TRUE) %>%
      dplyr::mutate(
        data = dataHora,
        data = stringr::str_sub(data, 1L, 8L),
        data = lubridate::ymd(data, quiet = TRUE)
      ) %>%
      dplyr::group_by(file_json, rowid) %>%
      dplyr::arrange(file_json, rowid, dplyr::desc(data), .by_group = TRUE) %>%
      dplyr::mutate_at(dplyr::vars(identificadorMovimento), as.integer) %>%
      dplyr::mutate(
        inc_id_mov_segue_ordem_cronologica = identificadorMovimento - dplyr::lead(identificadorMovimento) > 0,
        inc_id_mov_segue_ordem_cronologica = dplyr::if_else(
          is.na(inc_id_mov_segue_ordem_cronologica),
          TRUE,
          inc_id_mov_segue_ordem_cronologica
        ),
        inc_id_mov_segue_ordem_cronologica = dplyr::if_else(
          all(inc_id_mov_segue_ordem_cronologica == TRUE),
          'ID do movimento segue ordem cronológica',
          'ID do movimento NÃO segue ordem cronológica'
        )
      ) %>%
      dplyr::filter(
        inc_id_mov_segue_ordem_cronologica == 'ID do movimento NÃO segue ordem cronológica'
      ) %>%
      dplyr::filter(!is.na(identificadorMovimento))

    ordem_correta <- mov_ordenada %>%
      dplyr::group_by(file_json, rowid) %>%
      dplyr::arrange(
        file_json, rowid, dplyr::desc(identificadorMovimento), .by_group = TRUE
      ) %>%
      dplyr::ungroup() %>%
      {.$identificadorMovimento}

    mov_ordenada$sol_id_mov_segue_ordem_cronologica <- ordem_correta
    r <- mov_ordenada %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        file_json, rowid,
        inc_id_mov_segue_ordem_cronologica,
        info_id_mov_segue_ordem_cronologica = as.character(identificadorMovimento),
        sol_id_mov_segue_ordem_cronologica = as.character(sol_id_mov_segue_ordem_cronologica)
      ) %>%
      dplyr::distinct()
  } else{
    r <- mov %>%
      dplyr::transmute(
        file_json, rowid,
        inc_id_mov_segue_ordem_cronologica = NA_character_,
        info_id_mov_segue_ordem_cronologica = NA_character_,
        sol_id_mov_segue_ordem_cronologica = NA_character_
      )

  }
  return(r)
}

#' inc_mov_relevante_ordenada_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_mov_relevante_ordenada_fun <- function(mov){
  message('movimentações relevantes foram reportadas fora de ordem')

  sgt_movs <- sgt_movs %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate(cod_pai = as.character(as.integer(cod_pai)))

  mov_relevantes <- sgt_movs %>%
    dplyr::filter(codigo %in% c('22', '26', '193', '228', '848')) %>%
    dplyr::mutate(cod_filhos = paste(codigo, cod_filhos, sep = ',')) %>%
    dplyr::mutate(cod_filhos = stringr::str_replace(cod_filhos, ',NA', '')) %>%
    tidyr::separate_rows(cod_filhos, sep = ',') %>%
    dplyr::select(descricao, codigo = cod_filhos) %>%
    dplyr::mutate(
      descricao = dplyr::if_else(
        descricao == 'Baixa Definitiva' | descricao == 'Arquivamento',
        'Arquivamento/Baixa Definitiva', descricao
      )
    )

  mov_relevantes <- mov_relevantes %>%
    dplyr::left_join(sgt_movs, by = c('codigo' = 'codigo')) %>%
    dplyr::select(descricao = descricao.x, codigo, descricao_filha = descricao.y)

  ordem_correta <- c(
    'Arquivamento/Baixa Definitiva', 'Trânsito em julgado', 'Distribuição',
    'Julgamento'
  )

  if("codigoNacional" %in% names(mov)){
    mov_relevantes_ordenadas <- mov %>%
      dplyr::mutate(codigoNacional = as.character(codigoNacional)) %>%
      dplyr::mutate(
        data = dataHora,
        data = stringr::str_sub(data, 1L, 8L),
        data = lubridate::ymd(data, quiet = TRUE)
      ) %>%
      dplyr::filter(!is.na(codigoNacional)) %>%
      dplyr::filter(codigoNacional %in% mov_relevantes$codigo) %>%
      dplyr::left_join(
        mov_relevantes, by = c('codigoNacional' = 'codigo')
      ) %>%
      dplyr::select(-descricao_filha) %>%
      dplyr::arrange(file_json, rowid, dplyr::desc(data), .by_group = TRUE)

    r <- mov_relevantes_ordenadas %>%
      dplyr::group_by(file_json, rowid) %>%
      dplyr::mutate(mov_ordem = dplyr::row_number()) %>%
      dplyr::mutate(
        descricao = factor(descricao, ordered = TRUE, levels = ordem_correta)
      ) %>%
      dplyr::arrange(file_json, rowid, descricao, .by_group = TRUE) %>%
      dplyr::mutate(
        inc_mov_relevante_ordenada = mov_ordem - dplyr::lead(mov_ordem) < 0,
        inc_mov_relevante_ordenada = dplyr::if_else(
          is.na(inc_mov_relevante_ordenada), TRUE, inc_mov_relevante_ordenada
        ),
        inc_mov_relevante_ordenada = all(inc_mov_relevante_ordenada == TRUE),
        inc_mov_relevante_ordenada = dplyr::if_else(
          inc_mov_relevante_ordenada == TRUE,
          'Movimentos relevantes estão na ordem correta',
          'Movimentos relevantes estão fora de ordem'
        )
      ) %>%
      dplyr::filter(
        inc_mov_relevante_ordenada == 'Movimentos relevantes estão fora de ordem'
      ) %>%
      dplyr::arrange(
        file_json, rowid, mov_ordem, dplyr::desc(descricao), .by_group = TRUE
      ) %>%
      dplyr::mutate(sol_mov_relevante_ordenada = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        file_json, rowid, inc_mov_relevante_ordenada,
        info_mov_relevante_ordenada = identificadorMovimento,
        sol_mov_relevante_ordenada = as.character(sol_mov_relevante_ordenada)
      ) %>%
      dplyr::distinct()
  } else{
    r <- mov %>%
      dplyr::transmute(
        file_json, rowid,
        inc_mov_relevante_ordenada = NA_character_,
        info_mov_relevante_ordenada = NA_character_,
        sol_mov_relevante_ordenada = NA_character_
      )

  }
  return(r)
}


#' inc_mov_relevante_faltante_fun
#'
#' @description funções inc_ geram as inconsistências da base "da"
#'
#' @export
inc_mov_relevante_faltante_fun <- function(mov){
  message('processos já arquivados não têm todas as movs anteriores registradas')

  sgt_movs <- sgt_movs %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate(cod_pai = as.character(as.integer(cod_pai)))

  mov_relevantes <- sgt_movs %>%
    dplyr::filter(codigo %in% c('22', '26', '193', '228', '848')) %>%
    dplyr::mutate(cod_filhos = paste(codigo, cod_filhos, sep = ',')) %>%
    dplyr::mutate(cod_filhos = stringr::str_replace(cod_filhos, ',NA', '')) %>%
    tidyr::separate_rows(cod_filhos, sep = ',') %>%
    dplyr::select(descricao, codigo = cod_filhos) %>%
    dplyr::mutate(
      descricao = dplyr::if_else(
        descricao == 'Baixa Definitiva' | descricao == 'Arquivamento',
        'Arquivamento/Baixa Definitiva', descricao
      )
    )

  mov_relevantes <- mov_relevantes %>%
    dplyr::left_join(sgt_movs, by = c('codigo' = 'codigo')) %>%
    dplyr::select(descricao = descricao.x, codigo, descricao_filha = descricao.y)
  ordem_correta <- c(
    'Arquivamento/Baixa Definitiva', 'Trânsito em julgado', 'Distribuição',
    'Julgamento'
  )
  if("codigoNacional" %in% names(mov)){
    mov_faltantes <- mov %>%
      dplyr::mutate(codigoNacional = as.character(codigoNacional)) %>%
      dplyr::mutate(
        data = dataHora,
        data = stringr::str_sub(data, 1L, 8L),
        data = lubridate::ymd(data, quiet = TRUE)
      ) %>%
      dplyr::filter(!is.na(codigoNacional)) %>%
      dplyr::filter(codigoNacional %in% mov_relevantes$codigo) %>%
      dplyr::left_join(
        mov_relevantes, by = c('codigoNacional' = 'codigo')
      ) %>%
      dplyr::select(-descricao_filha) %>%
      dplyr::arrange(file_json, rowid, dplyr::desc(data), .by_group = TRUE) %>%
      dplyr::mutate(
        descricao = factor(descricao, ordered = TRUE, levels = ordem_correta)
      )
    r <- mov_faltantes %>%
      dplyr::group_by(file_json, rowid) %>%
      dplyr::arrange(file_json, rowid, dplyr::desc(data), .by_group = TRUE) %>%
      dplyr::filter(dplyr::first(descricao) == 'Arquivamento/Baixa Definitiva') %>%
      dplyr::mutate(info_mov_faltante = all(levels(descricao) %in% descricao)) %>%
      dplyr::filter(info_mov_faltante == FALSE) %>%
      dplyr::transmute(
        file_json = file_json, rowid = rowid,
        inc_mov_faltante = paste0(
          'Processos já arquivados não contêm',
          ' todas as movimentações principais anteriores'
        ),
        info_mov_faltante = descricao
      )
  } else{
    r <- mov %>%
      dplyr::transmute(
        file_json, rowid,
        inc_mov_faltante = NA_character_,
        info_mov_faltante = NA_character_
      )

  }
  return(r)
}
