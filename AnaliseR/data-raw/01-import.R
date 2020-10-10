library(magrittr)

# processamento dados basicos brutos ---------------------------------------

all_files <- fs::dir_ls("../dados/brutos/", regexp = "json", recurse = TRUE)

aux_files <- all_files %>%
  tibble::enframe() %>%
  dplyr::transmute(
    path = name,
    justica = stringr::str_extract(value, "(?<=(justica|tribunais)_)[^/]+"),
    tribunal = stringr::str_extract(value, "(?<=processos-)[^/]+")
  )

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
    dplyr::bind_cols(dplyr::select(da, where(~!is.list(.x)))) %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(c(where(~!is.list(.x)), -valor_causa), .fns = as.character))
}

ler_basic <- function(file) {
  pb <- progressr::progressor(length(file))
  furrr::future_map_dfr(purrr::set_names(file), ~{
    pb()
    ler_basic_one(.x)
  }, .id = "file_json")
}

arquivos_nested <- aux_files %>%
  dplyr::group_by(justica, tribunal) %>%
  tidyr::nest() %>%
  dplyr::ungroup() %>%
  tidyr::unite(jj, justica, tribunal, sep = "_")

# future::plan(future::multisession, workers = 4)

purrr::walk2(arquivos_nested$jj, arquivos_nested$data, ~{
  message(.x)
  f <- paste0("../dados/processados/basicos/", .x, ".rds")
  if (!file.exists(f)) {
    progressr::with_progress({
      da_basic <- ler_basic(.y$path)
      readr::write_rds(da_basic, f)
    })
  }
})



# bind dos dados processados ----------------------------------------------


library(magrittr)

da_basic <- fs::dir_ls("../dados/processados/basicos/") %>%
  purrr::map_dfr(readr::read_rds, .id = "file")

# da_basic %>%
#   dplyr::group_by(file) %>%
#   dplyr::summarise(
#     n = dplyr::n(),
#     n_processos = length(unique(numero)),
#     .groups = "drop"
#   ) %>%
#   dplyr::mutate(file = fs::path_file(fs::path_ext_remove(file))) %>%
#   tidyr::separate(file, c("justica", "tribunal"), sep = "_") %>%
#   dplyr::arrange(justica) %>%
#   knitr::kable()

# dplyr::glimpse(da_basic)

readr::write_rds(da_basic, "../dados/processados/da_basic.rds", compress = "xz")

da_basic_transform <- da_basic %>%
  select(-assunto) %>%
  mutate(valor_causa = as.numeric(valor_causa)) %>%
  rowid_to_column() %>%
  mutate(justica_tribunal = fs::path_file(fs::path_ext_remove(file))) %>%
  separate(justica_tribunal, c("justica", "tribunal"), sep = "_") %>%
  mutate(
    tribunal = str_remove(tribunal, "-"),
    tribunal = toupper(tribunal),
    tribunal = str_replace(tribunal, "DF$", "DFT"),
    tribunal = str_replace(tribunal, "(?<=TRT)([1-9])$", "0\\1")
  )

readr::write_rds(da_basic_transform, "../dados/processados/da_basic_transform.rds", compress = "xz")

## export para python
# da_basic_transform %>%
#   select(-assunto) %>%
#   feather::write_feather("../dados/processados/da_basic_transform.feather")

da_basic_assuntos <- da_basic %>%
  rowid_to_column() %>%
  select(rowid, assunto) %>%
  unnest(assunto)

da_basic_assuntos %>%
  unchop(assuntoLocal)

da_basic_assuntos %>%
  feather::write_feather("../dados/processados/da_basic_assuntos.feather")



# drafts ------------------------------------------------------------------

file <- "../dados/brutos/justica_trabalho/processos-trt10/processos-trt10_2.json"
json <- jsonlite::read_json(
  file,
  simplifyDataFrame = TRUE,
  flatten = FALSE
)
as_tibble(json$dadosBasicos) %>%
  sample_n(10) %>%
  glimpse()


as.character(json$dadosBasicos$dataAjuizamento[100])
json$movimento[[100]]
