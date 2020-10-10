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
  ns <- NS(id)
  tagList(

    bs4Dash::box(
      title = "Inconsistências",
      bs4Dash::box(
        title = textOutput(ns("incos_justica_lab")),
        collapsible = FALSE,
        closable = FALSE,
        shiny::tags$p("Casos em que o número CNJ do processo não bate com a Justiça ou o Tribunal."),
        shiny::downloadButton(ns("incos_justica_dld"))
      ),
      bs4Dash::box(
        title = textOutput(ns("incos_classe_lab")),
        collapsible = FALSE,
        closable = FALSE,
        shiny::tags$p("Casos em que o código CNJ da classe não bate com a TPU."),
        shiny::downloadButton(ns("incos_classe_dld"))
      )
    )

  )
}

#' incos Server Functions
#'
#' @noRd
mod_incos_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns


    # justica
    output$incos_justica_lab <- shiny::renderText({
      n <- cnjInova::da_incos %>%
        dplyr::filter(!is.na(inc_justica)) %>%
        nrow()
      stringr::str_glue("Justiça/Tribunal ({n})")
    })
    output$incos_justica_dld <- shiny::downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "-inc_justica.xlsx")
      },
      content = function(file) {
        cnjInova::da_incos %>%
          dplyr::select(rowid, numero, justica, tribunal, inc_justica) %>%
          writexl::write_xlsx(file)
      }
    )

    # classe
    output$incos_classe_lab <- shiny::renderText({
      n <- cnjInova::da_incos %>%
        dplyr::filter(!is.na(inc_classe_sgt)) %>%
        nrow()
      stringr::str_glue("Classe ({n})")
    })
    output$incos_classe_dld <- shiny::downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "-inc_classe.xlsx")
      },
      content = function(file) {
        cnjInova::da_incos %>%
          dplyr::filter(!is.na(inc_classe_sgt)) %>%
          dplyr::select(rowid, numero, justica, tribunal, classe_processual, inc_classe_sgt) %>%
          writexl::write_xlsx(file)
      }
    )




  })
}

## To be copied in the UI
# mod_incos_ui("incos_ui_1")

## To be copied in the server
# mod_incos_server("incos_ui_1")
