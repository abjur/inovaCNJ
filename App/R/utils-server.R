#' Import files
#'
#' @description Lê um arquivo .json ou .xml e retorna uma base de dados pronta para o uso
#'
#' @param inflie Arquivo .json ou .xml a ser estruturado
#'
#' @export
parse_file <- function(infile,names){
  # infile <- fs::dir_ls("../dados/brutos/", regexp = "json", recurse = TRUE)[385:386]
  # names = str_extract(infile,'processos.[^/]*json$')

  aux_files <- infile %>%
    tibble::enframe() %>%
    dplyr::transmute(
      path = infile,
      justica = dplyr::case_when(stringr::str_detect(names,'tre') ~ 'eleitoral',
                                 stringr::str_detect(names,'tj') ~ 'estadual',
                                 stringr::str_detect(names,'trf') ~ 'federal',
                                 stringr::str_detect(names,'trt') ~ 'trabalho',
                                 stringr::str_detect(names,'stm') ~ 'militar',
                                 stringr::str_detect(names,'tst|stj|stf') ~ 'superior'),
      tribunal = stringr::str_extract(names, "(?<=processos-)[^/]+(?=_[0-9]\\.json)")
    )
  # print('parte1 sucesso')

  # print('parte2')
  ler_basic_one <- function(file) {
    da <- jsonlite::read_json(
      file,
      simplifyDataFrame = TRUE,
      flatten = FALSE
    )

    da$dadosBasicos %>%
      tibble::as_tibble() %>%
      tidyr::chop(orgaoJulgador) %>%
      tidyr::unnest(orgaoJulgador) %>%
      dplyr::bind_cols(dplyr::select(da,dplyr::contains('movimento')),
                       dplyr::select(da, where(~!is.list(.x)))) %>%
      janitor::clean_names() %>%
      dplyr::mutate(dplyr::across(c(where(~!is.list(.x))), .fns = as.character)) %>%
      tibble::rowid_to_column()
  }

  ler_basic <- function(file) {
    furrr::future_map_dfr(purrr::set_names(file), ~{
      ler_basic_one(.x)
    }, .id = "file_json")
  }

  arquivos_nested <- aux_files %>%
    dplyr::group_by(justica, tribunal) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    tidyr::unite(jj, justica, tribunal, sep = "_")

  da_basic <- purrr::pmap_dfr(arquivos_nested, function(jj,data){
    message(jj)
    ler_basic(data$path)
  })

  parse_mv <- function(mv) {
    if (!is.null(mv)) {
      colunas <- c("tipoResponsavelMovimento", "complementoNacional",
                   "idDocumentoVinculado", "nivelSigilo", "dataHora", "tipoDecisao")
      nm <- colunas[colunas %in% names(mv)]
      dplyr::bind_cols(
        mv[,nm],
        mv[["movimentoLocal"]],
        mv[["movimentoNacional"]],
        mv[["orgaoJulgador"]],
        .name_repair = "minimal"
      )
    }
  }

  movs_fast <- function(da_basic) {
    da_basic <- da_basic %>%
      dplyr::mutate(id = paste0(file_json,"_rowid_",rowid))

    movs <- purrr::set_names(da_basic$movimento, da_basic$id)
    l_res <- purrr::map(movs, parse_mv) %>%
      data.table::rbindlist(fill = TRUE, idcol = "id") %>%
      tibble::as_tibble() %>%
      dplyr::mutate(file_json = stringr::str_extract(id,'.*json'),
                    rowid = stringr::str_extract(id,'[0-9]*$')) %>%
      dplyr::select(-id) %>%
      dplyr::select(file_json,rowid, dplyr::everything())
    l_res
  }

  da_movs <- movs_fast(da_basic)

  da_basic_assuntos <- da_basic %>%
    dplyr::select(file_json, rowid, assunto) %>%
    tidyr::unnest(assunto) %>% {
      dplyr::bind_cols(
        dplyr::select(.,file_json,rowid,principal,codigoNacional),
        .$assuntoLocal
      )
    } %>%
    janitor::clean_names()

  da_basic_transform <- da_basic %>%
    dplyr::select(-assunto,-movimento) %>% {
      if('valor_causa' %in% names(.)){
        dplyr::mutate(.,valor_causa = as.numeric(valor_causa))
      } else{
        .
      }
    } %>%
    tibble::rowid_to_column("id") %>%
    dplyr::left_join(aux_files,c('file_json' = 'path')) %>%
    dplyr::mutate(
      tribunal = stringr::str_remove(tribunal, "-"),
      tribunal = toupper(tribunal),
      tribunal = stringr::str_replace(tribunal, "DF$", "DFT"),
      tribunal = stringr::str_replace(tribunal, "(?<=TRT)([1-9])$", "0\\1")
    )


  return(list(assuntos = da_basic_assuntos,
              basic = da_basic_transform,
              movs = da_movs))
}

#' Estrutura as bases de dados
#'
#' @description Lê os assuntos e dados basicos (saida de parse_files) e retorna o da_incos
#'
#' @param baiscs lista contendo assuntos e basics
#'
#' @export
cria_da_incos <- function(lista_bases,session){
  # infile <- fs::dir_ls("../dados/brutos/", regexp = "json", recurse = TRUE)[385:386]
  # lista_bases <- parse_file(infile = infile,names = str_extract(infile,'processos.[^/]*json$'))
  da_basic_transform <- lista_bases$basic
  assuntos <- lista_bases$assuntos %>% dplyr::distinct()
  mov <- lista_bases$movs

  shinyWidgets::updateProgressBar(session = session,id = "pb_upload",value = 50)

  # export ------------------------------------------------------------------

  inc_funcs<- ls("package:inovaCNJ")

  list_incos <- inc_funcs[stringr::str_detect(inc_funcs, "^inc_") & !stringr::str_detect(inc_funcs, 'assunto|mov')] %>%
    purrr::map(~get(.x)(da_basic_transform))

  shinyWidgets::updateProgressBar(session = session,id = "pb_upload",value = 70)


  tab_assunto <- inc_assuntos_fun(da_assunto = assuntos,sgt_assunto = sgt_assuntos)
  tab_classe_assunto = inc_classe_assunto_fun(
    da_assunto = assuntos,
    da_basic = da_basic_transform,
    sgt_assunto = sgt_assuntos)

  shinyWidgets::updateProgressBar(session = session,id = "pb_upload",value = 85)


  tab_incos_movs <- inc_funcs[stringr::str_detect(inc_funcs, "^inc_mov")] %>%
    purrr::set_names() %>%
    purrr::map(~get(.x)(mov)) %>%
    purrr::imap(~{
      .x %>%
        dplyr::group_by(file_json,rowid) %>%
        tidyr::nest() %>%
        dplyr::ungroup() %>%
        purrr::set_names(c("file_json","rowid", .y))
    }) %>%
    purrr::reduce(dplyr::left_join, by = c("file_json",'rowid'), .init = dplyr::distinct(mov, file_json,rowid)) %>%
    dplyr::mutate(rowid = as.integer(rowid))

  da_inicial <- da_basic_transform %>%
    dplyr::select(id, rowid, numero, file_json, justica, tribunal)

  da_incos <- list_incos %>%
    purrr::reduce(dplyr::left_join, by = "id", .init = da_inicial) %>%
    dplyr::filter_at(dplyr::vars(dplyr::starts_with("inc_")), dplyr::any_vars(!is.na(.))) %>%
    dplyr::left_join(tab_assunto,by = c('file_json', 'rowid')) %>%
    dplyr::left_join(tab_classe_assunto, by = c('file_json', 'rowid')) %>%
    dplyr::left_join(tab_incos_movs, by = c('file_json', 'rowid')) %>%
    dplyr::distinct()

  shinyWidgets::updateProgressBar(session = session,id = "pb_upload",value = 95)

  return(da_incos)
}

#' Estrutura as bases de dados (da_totais)
#'
#' @description Lê o da_incos e o basics e cria o da_totais
#'
#' @param baiscs lista contendo assuntos e basics
#'
#' @export
cria_da_totais <- function(lista_bases,da_incos){
  #infile <- fs::dir_ls("../dados/brutos/", regexp = "json", recurse = TRUE)
  # lista_bases <- parse_file(infile[1:6])
  # da_incos <- cria_da_incos(lista_bases = lista_bases)

  da_basic_transform <- lista_bases$basic
  da_incos <- da_incos


  da_totais_basic <- da_basic_transform %>%
    dplyr::count(justica, tribunal)

  da_totais_incos <- da_incos %>%
    dplyr::group_by(justica, tribunal) %>%
    dplyr::summarise(
      dplyr::across(matches("^inc_mov"), ~sum(map_lgl(.x, is.null))),
      dplyr::across(matches("^inc_[a-z]{3}[^_]"), ~sum(!is.na(.x))),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(starts_with("inc")) %>%
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
      ranking = dplyr::min_rank(1 - indice)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(justica, ranking)

  return(da_totais)
}
