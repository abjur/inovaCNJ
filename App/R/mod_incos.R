#' Contem a lista de inconsistencias mapeadas no app.
incos_mapeadas <- function() {
  list(
    "assunto_vazio" = list(
      nome = "Assunto (i)",
      desc = "Assunto vazio"
    ),
    "assunto_generico" = list(
      nome = "Assunto (ii)",
      desc = 'Assunto genérico'
    ),
    "assunto_nao_bate_com_tpu" = list(
      nome = "Assunto (iii)",
      desc = "Código CNJ do assunto não bate com a TPU (Res. 46 CNJ)."
    ),
    "nao_possui_assunto_principal" = list(
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
      desc = "Dígito verificador inconsistente com o número do processo (Res. 65 CNJ)."
    ),
    "municipio" = list(
      nome = "Código IBGE",
      desc = "Código IBGE do município inconsistente com a base de dados do IBGE."
    ),
    "justica" = list(
      nome = "Justiça/Tribunal",
      desc = "Número CNJ do processo não bate com a Justiça ou o Tribunal (Res. 65 CNJ)."
    ),
    "orgao" = list(
      nome = "Código do Órgão",
      desc = "Código do órgão não bate com os órgãos oficiais (Anexo II Res. 76 CNJ)"
    ),
    "eletronico" = list(
      nome = "Processo eletrônico",
      desc = "Identificador de processo eletrônico fora do padrão."
    ),
    "sistema" = list(
      nome = "Código do sistema",
      desc = "Identificador de sistema processual fora do padrão."
    ),
    "valor" = list(
      nome = "Valor da causa",
      desc = "Valor negativo ou muito alto para os padrões do Tribunal."
    )
  )
}

#' incos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_incos_ui <- function(id){
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

      shiny::conditionalPanel(
        stringr::str_glue("document.getElementById('{ns(.y)}_box').classList.contains('maximized-card')"),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            reactable::reactableOutput(ns(paste0(.y, "_tab")))
          )
        ),
        shiny::fluidRow(
          shiny::fileInput(
            ns(paste0(.y, "_up")), "Upload de base arrumada",
            accept = ".xlsx"
          )
        ),
        shiny::fluidRow(
          shiny::actionButton(ns(paste0(.y, "_btn")), "Submeter")
        ),
        ns = ns
      ),
      shiny::fluidRow(
        shiny::downloadButton(ns(paste0(.y, "_dld")))
      )
    )
  })

  tagList(
    fluidRow(caixas)
    # bs4Dash::box(
    #   title = "Inconsistências",
    #   width = 12,
    #   caixas
    # )
  )

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

    purrr::map(seq_along(nm), ~{





      # label
      output[[paste0(nm[.x], "_lab")]] <- shiny::renderText({
        n <- app_data()$incos %>%
          dplyr::filter(!is.na(.data[[paste0("inc_", nm[.x])]])) %>%
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
            dplyr::filter(!is.na(.data[[paste0("inc_", nm[.x])]])) %>%
            dplyr::select(
              id, justica, tribunal,
              dplyr::matches(paste0("^(inc_|sol_|info_).*", nm[.x]))
            ) %>%
            writexl::write_xlsx(file)
        }
      )

      # tabela
      output[[paste0(nm[.x], "_tab")]] <- reactable::renderReactable({

        app_data()$incos %>%
          dplyr::filter(!is.na(.data[[paste0("inc_", nm[.x])]])) %>%
          dplyr::select(
            id, justica, tribunal,
            dplyr::matches(paste0("^(inc_|sol_|info_).*", nm[.x]))
          ) %>%
          reactable::reactable()

      })


      # upload

      shiny::observe({
        path <- input[[paste0(nm[.x], "_up")]][["datapath"]]
        if (!is.null(path)) da <- readxl::read_excel(path)
      })

      shiny::observeEvent(input[[paste0(nm[.x], "_btn")]], {

        path <- input[[paste0(nm[.x], "_up")]][["datapath"]]


        if (!is.null(path)) {

          da <- readxl::read_excel(path)

          da_incos <- app_data()$incos %>%
            dplyr::filter(!is.na(.data[[paste0("inc_", nm[.x])]])) %>%
            dplyr::select(
              id, justica, tribunal,
              dplyr::matches(paste0("^(inc_|sol_|info_).*", nm[.x]))
            )

          if (all(names(da_incos) %in% names(da)) && nrow(da_incos) == nrow(da)) {
            shinyalert::shinyalert(
              "Arquivo submetido com sucesso!",
              stringr::str_glue(
                "O arquivo com correções foi submetido com sucesso",
                "e será analisado pela equipe do CNJ."
              ),
              type = "success",
              closeOnClickOutside = TRUE
            )
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
              "Um arquivo válido contém as mesmas linhas e colunas da base",
              "disponível para Download e, eventualmente, uma coluna",
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
