library(tidyverse)

da_basic <- readr::read_rds("../dados/processados/da_basic.rds") %>%
  mutate(valor_causa = as.numeric(valor_causa)) %>%
  rowid_to_column() %>%
  mutate(justica_tribunal = fs::path_file(fs::path_ext_remove(file))) %>%
  separate(justica_tribunal, c("justica", "tribunal"), sep = "_")

da_basic_transform <- da_basic %>%
  mutate(
    tribunal = str_remove(tribunal, "-"),
    tribunal = toupper(tribunal),
    tribunal = str_replace(tribunal, "DF$", "DFT"),
    tribunal = str_replace(tribunal, "(?<=TRT)([1-9])$", "0\\1")
  )

#' esse código cria bases com id e coluna para cada possível inconsistência.
#' as colunas sempre apresentam uma descrição da inconsistência
#' e as vezes apresentam a solução da inconsistencia
#'
#' sufixos:
#' inc_xxx: inconsistência do problema xxx
#' sol_xxx: solução do problema xxx



# número incoerente com tribunal / justiça --------------------------------

inc_numero_justica_tribunal <- function(da) {

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
    mutate(
      inc_justica = case_when(
        is.na(id_justica) ~ "Justiça inválida",
        (justica_nproc != id_justica) & tribunal != "STJ" ~ "Justiça inválida",
        is.na(id_tribunal) ~ "Tribunal inválido",
        (tribunal_nproc != id_tribunal) & !tribunal %in% c("STJ", "TST", "STM") ~ "Tribunal inválido"
      )
    )

}



# classe fora da SGT ------------------------------------------------------


inc_classe <- function(da) {
  classe_sgt <- readr::read_delim(
    delim = ";",
    "../dados/brutos/sgt_classes.csv",
    col_types = cols(.default = col_character())
  ) %>%
    distinct(codigo) %>%
    filter(!is.na(codigo))

  da %>%
    mutate(inc_classe_sgt = case_when(
      is.na(classe_processual) ~ "Vazio",
      !classe_processual %in% classe_sgt$codigo ~ "Classe fora da SGT"
    ))
}


# digito verificador ------------------------------------------------------

# data ajuizamento --------------------------------------------------------

# sistema com código bugado -----------------------------------------------
# 1 – Pje, 2 – Projudi, 3 – SAJ, 4 – EPROC, 5 – Apolo, 6 – Themis, 7 – Libra, 8 – Outros;

# proc eletrônico com código bugado ---------------------------------------



# combinacoes raras classe-assunto ----------------------------------------

# codigo ibge incoerente --------------------------------------------------

# valor da causa aberrante ------------------------------------------------

# grau sup inconsistente --------------------------------------------------

# orgao vazio

# competencia -------------------------------------------------------------

# assuntos genéricos ------------------------------------------------------

# assuntos fora da SGT ----------------------------------------------------

# contagem de assuntos ----------------------------------------------------

# tamanho do processo -----------------------------------------------------


# da_basic$assunto[[97081]]
#
#
#
#
# sample(da_basic$assunto, 10)
#
# da_basic %>%
#   count(proc_el)
#
# da_basic %>%
#   filter(grau == "TRU") %>%
#   count(sigla_tribunal)
#
# da_basic %>%
#   filter(!is.na(valor_causa), valor_causa == "3e+06") %>%
#   select(file, file_json)
#
#
# file <- "../dados/brutos/justica_estadual/processos-tjap/processos-tjap_2.json"
#
# da <- jsonlite::read_json(
#   file,
#   simplifyDataFrame = TRUE,
#   flatten = FALSE
# )
# da$dadosBasicos %>%
#   filter(numero == "00008406219948030001") %>%
#   with(valorCausa) %>%
#   scales::number()
#
# aff <- ler_basic_one(file)
# aff %>%
#   filter(numero == "00008406219948030001")
#   filter(valor_causa == "3e+06") %>%
#   select(numero)



# export ------------------------------------------------------------------

da_incos <- da_basic_transform %>%
  inc_numero_justica_tribunal() %>%
  inc_classe() %>%
  select(
    rowid, justica, tribunal,
    numero, classe_processual,
    starts_with("inc_"),
    starts_with("sol_")
  ) %>%
  filter(!is.na(inc_justica) | !is.na(inc_classe_sgt))

readr::write_rds(da_incos, "data-raw/p02_01_da_incos_basic.rds")


