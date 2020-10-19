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


    bs4Dash::box(
      width = 12,
      inputId = ns("caixa"),
      title = "Inconsistências",
      collapsible = FALSE,
      closable = FALSE,
      maximizable = FALSE,

      reactable::reactableOutput(ns("tabela")),

      shiny::actionButton(
        ns("atualizar"), "Atualizar",
        icon = shiny::icon("sync")
      )

    )



  )
}

#' verificacao Server Functions
#'
#' @noRd
mod_verificacao_server <- function(id){
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$tabela <- reactable::renderReactable({

      shiny::validate(shiny::need(
        auth_admin(session),
        message = "Usuário precisa ser administrador para acessar essa parte da aplicação."
      ))

      input$atualizar
      con <- conectar()
      dados <- RPostgres::dbReadTable(con, "sugestoes")
      desconectar(con)
      reactable::reactable(dados)

    })




  })
}

## To be copied in the UI
# mod_verificacao_ui("verificacao_ui_1")

## To be copied in the server
# mod_verificacao_server("verificacao_ui_1")
