#' Contem a lista de inconsistencias mapeadas no app.
incos_mapeadas <- function() {
  list(
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
      title = textOutput(ns(paste0(.y, "_lab"))),
      collapsible = FALSE,
      closable = FALSE,
      shiny::tags$p(.x$desc),
      shiny::downloadButton(ns(paste0(.y, "_dld")))
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
mod_incos_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    # browser()

    incos <- incos_mapeadas()
    nm <- names(incos)

    purrr::map(seq_along(nm), ~{
      output[[paste0(nm[.x], "_lab")]] <- shiny::renderText({
        n <- inovaCNJ::da_incos %>%
          dplyr::filter(!is.na(.data[[paste0("inc_", nm[.x])]])) %>%
          nrow()
        stringr::str_glue("{incos[[.x]]$nome} ({n})")
      })
    })

    purrr::map(seq_along(nm), ~{
      output[[paste0(nm[.x], "_dld")]] <- shiny::downloadHandler(
        filename = function() {
          paste0(Sys.Date(), "-inc_", nm[.x], ".xlsx")
        },
        content = function(file) {
          inovaCNJ::da_incos %>%
            dplyr::select(
              rowid, numero, justica, tribunal,
              dplyr::matches(paste("^(inc_|sol_|info_).*", nm[.x]))
            ) %>%
            writexl::write_xlsx(file)
        }
      )
    })

  })
}

## To be copied in the UI
# mod_incos_ui("incos_ui_1")

## To be copied in the server
# mod_incos_server("incos_ui_1")
