#' geral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_geral_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(

    bs4Dash::box(
      width = 12

    ),

    bs4Dash::box(
      width = 12,
      reactable::reactableOutput(ns("tabela")) %>%
        shinycssloaders::withSpinner()
    )
  )
}

#' geral Server Functions
#'
#' @noRd
mod_geral_server <- function(id, app_data) {
  shiny::moduleServer(id, function(input, output, session){


    output$tabela <- reactable::renderReactable({
      reactable::reactable(utils::head(app_data()$incos))
    })


  })
}

