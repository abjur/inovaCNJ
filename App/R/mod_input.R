#' input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    bs4Dash::box(inputId = ns('file_upload'),title = 'Upload',width = 12,closable = FALSE,
                 shiny::fileInput(inputId = ns('input_file'),label = 'Faça o upload do(s) seu(s) arquivo(s',
                                  multiple = TRUE,buttonLabel = 'Upload',accept = ".json"),

                 shinyWidgets::progressBar(id = ns('pb_upload'),
                                           title = 'Estruturando os dados',
                                           value = 0,
                                           total = 100,
                                           display_pct = TRUE)),

    bs4Dash::box(inputId = ns('tabpanels'),title = 'Validação',width = 12,closable = FALSE,
      bs4Dash::bs4TabSetPanel(id = ns('tabpanel'),side = 'left',
                              bs4Dash::tabPanel(tabName = "Base estruturada",active = TRUE,
                                                reactable::reactableOutput(outputId = ns('input_table'))),
                              bs4Dash::tabPanel(tabName = "Inconsistências",active = FALSE,
                                                shiny::uiOutput(ns('inc_valid')))


      )
    )
  )

}

cria_tabela_json <- function(infile){

  paths <- infile$datapath
  names <- infile$name

  parse <- parse_file(infile = paths,names = names)

  da_incos <- parse %>%
    cria_da_incos()

  da_totais <- cria_da_totais(lista_bases = parse,da_incos = da_incos)

  return(list(da_incos = da_incos,
              da_totais = da_totais))

}

#' input Server Functions
#'
#' @noRd
mod_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    infile <- shiny::eventReactive(input$input_file,{


      file <- input$input_file

      paths <- file$datapath
      names <- file$name

      ext <- tools::file_ext(paths)
      validate(need(ext == "json", "O arquivo deve ser em formato json"))

      shinyWidgets::updateProgressBar(session = session,id = "pb_upload",value = 10)

      parse <- parse_file(infile = paths,names = names)

      shinyWidgets::updateProgressBar(session = session,id = "pb_upload",value = 25)

      da_incos <- parse %>%
        cria_da_incos(session)

      da_totais <- cria_da_totais(lista_bases = parse,da_incos = da_incos)

      base <- list(incos = da_incos,
                   totais = da_totais)

      shinyWidgets::updateProgressBar(session = session,id = "pb_upload",value = 100)

      return(base)
    })

    output$input_table <- reactable::renderReactable({
      reactable::reactable(
        infile()$incos %>%
          dplyr::select(-id,-rowid,-file_json,
                        -dplyr::starts_with("inc_"),
                        -dplyr::starts_with("info_"),
                        -dplyr::starts_with("sol_"))
      )
    })

    output$inc_valid <- shiny::renderUI({
      mod_incos_ui(ns("incos_ui_2"))
    })

    mod_incos_server(id = 'incos_ui_2',app_data = infile)

  })
}

## To be copied in the UI
# mod_input_ui("input_ui_1")

## To be copied in the server
# mod_input_server("input_ui_1")
