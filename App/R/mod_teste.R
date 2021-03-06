#' teste UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_teste_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::verbatimTextOutput(ns("teste"))

  )
}

#' teste Server Functions
#'
#' @noRd
mod_teste_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$teste <- shiny::renderPrint({

      utils::str(session$userData$auth0_info)

    })

  })
}

## To be copied in the UI
# mod_teste_ui("teste_ui_1")

## To be copied in the server
# mod_teste_server("teste_ui_1")
