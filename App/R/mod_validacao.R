#' validacao UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_validacao_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(

    shiny::fluidRow(
      bs4Dash::box(
        width = 6, collapsible = FALSE, closable = FALSE,
        title = "Passo 1: Monte sua planilha",
        shiny::p("Crie uma planilha com seus dados. A planilha deve seguir os moldes do template abaixo."),
        shiny::div(
          shiny::downloadButton(
            outputId = ns("template"),
            label = "Template de planilha"
          ),
          align = "center"
        )
      ),
      bs4Dash::box(
        width = 6, collapsible = FALSE, closable = FALSE,
        title = "Passo 2: Insira seus processos",
        shiny::fileInput(
          width = "100%",
          inputId = ns("planilha"),
          label = "Fa\u00e7a o upload de seus processos",
          accept = c("xlsx"),
          buttonLabel = "Selecionar...",
          placeholder = "Nenhum arquivo selecionado"
        ),

        shiny::div(
          shiny::actionButton(
            ns("rodar"),
            label = "Rodar!"
          ),
          align = "center"
        )
      )
    ),
    shiny::fluidRow(
      bs4Dash::box(
        width = 12, collapsible = FALSE, closable = FALSE,
        title = "Passo 3: Resultados",
        shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela"))),
        shiny::br(),
        shiny::downloadButton(
          outputId = ns("download"),
          label = "Download do resultado"
        )
      )
    )

  )
}

#' validacao Server Functions
#'
#' @noRd
mod_validacao_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    # template ----
    output$template <- shiny::downloadHandler(
      filename = "template_processos.xlsx",
      content = function(file) {
        writexl::write_xlsx(inovaCNJ::da_sample, file)
      }
    )

    # tabela arrumada ----
    montar_tabela <- function(x) {
      x
    }

    tabela_arrumada <- shiny::eventReactive(input$rodar, {
      tabela <- input$planilha$datapath
      processos <- tabela %>%
        readxl::read_excel(n_max = 100)
      montar_tabela(processos)
    })

    # tabela resultado ----
    output$tabela <- reactable::renderReactable({
      tab <- utils::head(tabela_arrumada(), 100)
      shiny::validate(shiny::need(
        !is.null(tab),
        "N\u00e3o foi poss\u00edvel gerar a tabela com os par\u00e2metros especificados")
      )
      if (!is.null(tab)) {
        reactable::reactable(tab, wrap = FALSE)
      }
    })

    # download resultado ----
    output$download <- shiny::downloadHandler(
      filename = "resultados_diagnostico.xlsx",
      content = function(file) {
        tab <- tabela_arrumada()
        if (!is.null(tab)) writexl::write_xlsx(tab, file)
      }
    )

  })
}

## To be copied in the UI
# mod_validacao_ui("validacao_ui_1")

## To be copied in the server
# mod_validacao_server("validacao_ui_1")
