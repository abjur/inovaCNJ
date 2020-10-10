library(tidyverse)

da_basic_transform <- readr::read_rds("../dados/processados/da_basic_transform.rds")

#' esse código cria bases com id e coluna para cada possível inconsistência.
#' as colunas sempre apresentam uma descrição da inconsistência
#' e as vezes apresentam a solução da inconsistencia
#'
#' sufixos:
#' inc_xxx: inconsistência do problema xxx
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

# assuntos genéricos ------------------------------------------------------
# assuntos fora da SGT ----------------------------------------------------
# contagem de assuntos ----------------------------------------------------
# combinacoes raras classe-assunto ----------------------------------------




# export ------------------------------------------------------------------

list_incos <- ls()[str_detect(ls(), "^inc_")] %>%
  purrr::map(~get(.x)(da_basic_transform))

da_inicial <- da_basic_transform %>%
  select(id, rowid, file_json, justica, tribunal)
da_incos <- list_incos %>%
  reduce(left_join, by = "id", .init = da_inicial) %>%
  filter_at(vars(starts_with("inc_")), any_vars(!is.na(.)))

readr::write_rds(
  da_incos,
  "data-raw/p02_01_da_incos_basic.rds",
  compress = "xz"
)

glimpse(da_incos)
