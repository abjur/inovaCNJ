sgt_assuntos <- readr::read_delim(file = '../App/data-raw/sgt_assuntos.csv',
                                  delim = ';',col_types = readr::cols(cod_filhos = 'c')) %>%
  dplyr::transmute(codigo,dscr= stringr::str_squish(descricao))


sgt_classes <- readr::read_delim(
  delim = ";",
  "../App/data-raw/sgt_classes.csv",
  col_types = cols(.default = col_character())
)

sgt_movs <- '../App/data-raw/sgt_movimentos.csv' %>%
  readr::read_delim(
    delim = ';', col_types = readr::cols(.default = 'c')
  )

mapa <- geobr::read_municipality(year = 2019) %>%
  dplyr::mutate(code_muni = as.character(code_muni)) %>%
  tibble::as_tibble() %>%
  dplyr::select(-geom)

orgaos_base <- readr::read_delim(
  delim = ";",
  "../dados/brutos/mpm_serventias.csv",
  col_types = cols(.default = col_character())
) %>%
  distinct(codigo = SEQ_ORGAO) %>%
  filter(!is.na(codigo))


save(sgt_assuntos,file = '../App/data/sgt_assuntos.rda')
save(sgt_classes,file = '../App/data/sgt_classes.rda')
save(sgt_movs,file = '../App/data/sgt_movs.rda')
save(mapa,file = '../App/data/mapa.rda')
save(orgaos_base,file = '../App/data/orgaos_base.rda')
