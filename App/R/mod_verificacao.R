#' verificacao UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_verificacao_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(

  )
}

#' verificacao Server Functions
#'
#' @noRd
mod_verificacao_server <- function(id){
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns






  })
}

## To be copied in the UI
# mod_verificacao_ui("verificacao_ui_1")

## To be copied in the server
# mod_verificacao_server("verificacao_ui_1")
