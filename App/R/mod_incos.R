#' Contem a lista de inconsistencias mapeadas no app.
incos_mapeadas <- function() {
  list(
    "assunto_vazio" = list(
      nome = "Assunto (i)",
      desc = "Assunto vazio"
    ),
    "generico" = list(
      nome = "Assunto (ii)",
      desc = 'Assunto genérico'
    ),
    "assunto_tpu" = list(
      nome = "Assunto (iii)",
      desc = "Código CNJ do assunto não bate com a TPU (Res. 46 CNJ)."
    ),
    "principal" = list(
      nome = "Assunto (iv)",
      desc = 'Não possui identificação de assunto "principal"'
    ),
    "classe_assunto_raro" = list(
      nome = "Classe/Assunto",
      desc = 'Combinação rara de classe e assunto'
    ),
    "classe" = list(
      nome = "Classe",
      desc = "Código CNJ da classe não bate com a TPU (Res. 46 CNJ)."
    ),
    "data" = list(
      nome = "Data de ajuizamento",
      desc = "Data de ajuizamento está vazia ou não bate com o número do processo."
    ),
    "digito" = list(
      nome = "Dígito verificador",
      desc = "Dígito verificador inconsistente com o número do processo (Res. 65 CNJ).",
      sol = TRUE
    ),
    "municipio" = list(
      nome = "Código IBGE",
      desc = "Código IBGE do município inconsistente com a base de dados do IBGE.",
      sol = TRUE
    ),
    "justica" = list(
      nome = "Justiça/Tribunal",
      desc = "Número CNJ do processo não bate com a Justiça ou o Tribunal (Res. 65 CNJ).",
      sol = TRUE
    ),
    "orgao" = list(
      nome = "Código do Órgão",
      desc = "Código do órgão não bate com os órgãos oficiais (Anexo II Res. 76 CNJ)"
    ),
    "eletronico" = list(
      nome = "Processo eletrônico",
      desc = "Identificador de processo eletrônico fora do padrão.",
      sol = TRUE
    ),
    "sistema" = list(
      nome = "Código do sistema",
      desc = "Identificador de sistema processual fora do padrão.",
      sol = TRUE
    ),
    "valor" = list(
      nome = "Valor da causa",
      desc = "Valor negativo ou muito alto para os padrões do Tribunal."
    ),
    "mov_responsavel_fun" = list(
      nome = "Responsável pela movimentação",
      desc = "Movimentação deveria ser do magistrado mas é de servidor",
      sol = TRUE
    ),
    "mov_cod_pai_faltante_fun" = list(
      nome = "Código pai faltante",
      desc = "Código pai está faltante na movimentação local.",
      sol = TRUE
    ),
    "mov_processo_longo_fun" = list(
      nome = "Tempo alto de tramitação",
      desc = "Processos cuja duração é maior que o 75º percentil dos dados de todos os processos."
    ),
    "mov_demorada_fun" = list(
      nome = "Movimentação demorada",
      desc = "Tempo desde última movimentação acima de 5 anos."
    ),
    "mov_id_segue_ordem_cronologica_fun" = list(
      nome = "Identificador de Movimento Fora de Ordem Cronológica",
      desc = "id de movimentos não corresponde à ordem cronológica das movs"
    ),
    "mov_relevante_ordenada_fun" = list(
      nome = "Movimentações fora de ordem",
      desc = "A numeração do identificador único de movimentos não seguem padrão compatível com a ordem cronológica dos movimentos."
    ),
    "mov_relevante_faltante_fun" = list(
      nome = "Movimentações relevante faltante",
      desc = "Ausência de movimentação importante em processos arquivados/baixados definitivamente"
    )
  )
}

#' incos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param mostrar mostrar botoes de upload
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_incos_ui <- function(id, mostrar){
  ns <- shiny::NS(id)


  caixas <- purrr::imap(incos_mapeadas(), ~{
    bs4Dash::box(
      width = 4,
      inputId = ns(paste0(.y, "_box")),
      title = textOutput(ns(paste0(.y, "_lab"))),
      collapsible = FALSE,
      closable = FALSE,
      maximizable = TRUE,
      shiny::tags$p(.x$desc),
      status = ifelse(is.null(.x$sol), "secondary", "primary"),

      shiny::fluidRow(
        shiny::downloadButton(ns(paste0(.y, "_dld")))
      ),

      shiny::conditionalPanel(
        stringr::str_glue("document.getElementById('{ns(.y)}_box').classList.contains('maximized-card')"),
        shiny::div(style='overflow-y: auto;height:80vh', shiny::column(width = 12,
          shiny::fluidRow(
            shiny::column(
              width = 12,
              reactable::reactableOutput(ns(paste0(.y, "_tab")))
            )
          ),
          if (mostrar) {
            shiny::tagList(
              shiny::fluidRow(
                shiny::fileInput(
                  ns(paste0(.y, "_up")), "Upload de base arrumada",
                  accept = ".xlsx"
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  reactable::reactableOutput(ns(paste0(.y, "_tabview")))
                )
              ),
              shiny::fluidRow(
                shiny::actionButton(ns(paste0(.y, "_btn")), "Submeter")
              )
            )
          }
        )),
        ns = ns
      )
    )
  })

  tagList(
    shiny::fluidRow(bs4Dash::box(
      title = "Informações básicas",
      width = 12,
      shiny::fluidRow(caixas[!stringr::str_detect(names(incos_mapeadas()), "classe|assunto|mov|generico|principal")]),
      collapsed = FALSE
    )),
    shiny::fluidRow(bs4Dash::box(
      title = "Classe/Assunto",
      width = 12,
      shiny::fluidRow(caixas[stringr::str_detect(names(incos_mapeadas()), "classe|assunto|generico|principal")]),
      collapsed = FALSE
    )),
    shiny::fluidRow(bs4Dash::box(
      title = "Movimentações",
      width = 12,
      shiny::fluidRow(caixas[stringr::str_detect(names(incos_mapeadas()), "mov")]),
      collapsed = FALSE
    ))
  )

}

vazio <- function(x) {
  if (is.list(x)) {
    purrr::map_lgl(x, is.null)
  } else {
    is.na(x)
  }
}

#' incos Server Functions
#'
#' @noRd
mod_incos_server <- function(id, app_data) {

  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    # browser()

    incos <- incos_mapeadas()
    nm <- names(incos)

    usuario <- session$userData$auth0_info$name
    usuario <- ifelse(is.null(usuario), "local", usuario)

    purrr::map(seq_along(nm), ~{

      # label
      output[[paste0(nm[.x], "_lab")]] <- shiny::renderText({
        shiny::validate(shiny::need(!is.null(app_data()$incos[[paste0("inc_", nm[.x])]]),
                                    "Inconsistência indisponível"))

        n <- app_data()$incos %>%
          dplyr::filter(!vazio(.data[[paste0("inc_", nm[.x])]])) %>%
          nrow()
        stringr::str_glue("{incos[[.x]]$nome} ({n})")
      })

      # download
      output[[paste0(nm[.x], "_dld")]] <- shiny::downloadHandler(
        filename = function() {
          paste0(Sys.Date(), "-inc_", nm[.x], ".xlsx")
        },
        content = function(file) {
          app_data()$incos %>%
            dplyr::filter(!vazio(.data[[paste0("inc_", nm[.x])]])) %>%
            dplyr::select(
              id, numero, justica, tribunal,
              dplyr::matches(paste0("^(inc_|sol_|info_).*", nm[.x]))
            ) %>%
            tidyr::unnest(where(is.list)) %>%
            writexl::write_xlsx(file)
        }
      )

      # tabela
      output[[paste0(nm[.x], "_tab")]] <- reactable::renderReactable({

        app_data()$incos %>%
          dplyr::filter(!vazio(.data[[paste0("inc_", nm[.x])]])) %>%
          dplyr::select(
            id,
            numero,
            justica, tribunal,
            dplyr::matches(paste0("^(inc_|sol_|info_).*", nm[.x]))
          ) %>%
          tidyr::unnest(where(is.list)) %>%
          reactable::reactable(compact = TRUE, defaultPageSize = 8)

      })

      output[[paste0(nm[.x], "_tabview")]] <- reactable::renderReactable({

        path <- input[[paste0(nm[.x], "_up")]][["datapath"]]
        if (!is.null(path)) {
          da <- readxl::read_excel(path)
          reactable::reactable(da, compact = TRUE, defaultPageSize = 8)
        }
      })

      # upload

      shiny::observe({
        path <- input[[paste0(nm[.x], "_up")]][["datapath"]]
        if (!is.null(path)) da <- readxl::read_excel(path)
      })

      shiny::observeEvent(input[[paste0(nm[.x], "_btn")]], {

        path <- input[[paste0(nm[.x], "_up")]][["datapath"]]


        if (!is.null(path)) {

          idate <- lubridate::now()
          id_sub <- as.character(round(as.numeric(idate)*1000))
          da <- readxl::read_excel(path) %>%
            dplyr::mutate(
              id_sub = id_sub,
              user = usuario,
              input_date = idate
            )

          da_incos_filter <- app_data()$incos %>%
            dplyr::filter(!is.na(.data[[paste0("inc_", nm[.x])]])) %>%
            dplyr::select(
              id,
              numero,
              justica, tribunal,
              dplyr::matches(paste0("^(inc_|sol_|info_).*", nm[.x]))
            ) %>%
            tidyr::unnest(where(is.list))

          if (all(names(da_incos_filter) %in% names(da)) && nrow(da_incos_filter) == nrow(da)) {

            try({

              con <- conectar()

              # browser()

              RPostgres::dbWriteTable(con, nm[.x], da, append = TRUE)

              da_sugestao <- tibble::tibble(
                id_sub = id_sub,
                user = usuario,
                input_date = idate,
                inconsistencia = nm[.x]
              )

              # browser()

              RPostgres::dbWriteTable(con, "sugestoes", da_sugestao, append = TRUE)

              desconectar(con)

              shinyalert::shinyalert(
                "Arquivo submetido com sucesso!",
                stringr::str_glue(
                  "O arquivo com correções foi submetido com sucesso",
                  " e será analisado pela equipe do CNJ."
                ),
                type = "success",
                closeOnClickOutside = TRUE
              )

            })


          } else {
            shinyalert::shinyalert(
              "Insira um arquivo válido",
              stringr::str_glue(
                "Um arquivo válido contém as mesmas linhas e colunas da base",
                "disponível para Download e, eventualmente, uma coluna",
                "adicional contendo observações e justificativas."
              ),
              type = "error",
              closeOnClickOutside = TRUE
            )
          }
        } else {
          shinyalert::shinyalert(
            "Insira um arquivo válido",
            stringr::str_glue(
              "Um arquivo válido contém as mesmas linhas e colunas da base ",
              "disponível para Download e, eventualmente, uma coluna ",
              "adicional contendo observações e justificativas."
            ),
            type = "error",
            closeOnClickOutside = TRUE
          )
        }

      })

    })





  })
}

## To be copied in the UI
# mod_incos_ui("incos_ui_1")

## To be copied in the server
# mod_incos_server("incos_ui_1")
