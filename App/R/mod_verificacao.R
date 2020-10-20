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

      shiny::fluidRow(col_12(
        reactable::reactableOutput(ns("tabela")) %>%
          shinycssloaders::withSpinner(),
        shiny::br()
      )),
      shiny::fluidRow(
        shiny::actionButton(
          ns("atualizar"), "Atualizar",
          icon = shiny::icon("sync")
        )
      )
    ),

    bs4Dash::box(
      width = 12,
      inputId = ns("caixa-tab"),
      title = "Tabela selecionada",
      collapsible = FALSE,
      closable = FALSE,
      maximizable = FALSE,

      shiny::fluidRow(col_12(
        reactable::reactableOutput(ns("tabela_selecionada")) %>%
          shinycssloaders::withSpinner() ,
      ))

    )
  )
}

#' verificacao Server Functions
#'
#' @noRd
mod_verificacao_server <- function(id){
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns


    dados_sugestoes <- shiny::reactive({
      input$atualizar
      con <- conectar()
      dados <- RPostgres::dbReadTable(con, "sugestoes")
      desconectar(con)
      dados
    })

    selected <- shiny::reactive(
      reactable::getReactableState(
        "tabela",
        "selected",
        session = session
      )
    )

    dados_tabela <- shiny::reactive({

      selecionado <- selected()
      linha <- dados_sugestoes()[selecionado, ]
      tab <- linha[["inconsistencia"]]
      num <- linha[["input_date"]]
      if (length(tab) > 0) {
        con <- conectar()
        dados <- con %>%
          dplyr::tbl(tab) %>%
          dplyr::filter(dplyr::between(input_date, num-1, num+1)) %>%
          dplyr::collect()
        desconectar(con)
        dados
      }
    })

    output$tabela <- reactable::renderReactable({

      shiny::validate(shiny::need(
        auth_admin(session),
        message = "Usuário precisa ser administrador para acessar essa parte da aplicação."
      ))

      reactable::reactable(
        dados_sugestoes(),
        selection = "single",
        onClick = "select"
      )

    })

    output$tabela_selecionada <- reactable::renderReactable({

      shiny::validate(shiny::need(
        auth_admin(session),
        message = "Usuário precisa ser administrador para acessar essa parte da aplicação."
      ))

      shiny::validate(shiny::need(
        dados_tabela(),
        message = "Nenhuma inconsistência selecionada."
      ))

      input$atualizar
      dados_tabela() %>%
        dplyr::select(-user, -input_date) %>%
        reactable::reactable(
          compact = TRUE,
          defaultPageSize = 8
        )

    })


  })
}
