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

    shiny::fluidRow(
      col_3(
        shinycssloaders::withSpinner(
          bs4Dash::bs4ValueBoxOutput(ns("indice"), width = 12),
          color = "#1E90FF"
        )
      ),
      col_3(
        shinycssloaders::withSpinner(
          bs4Dash::bs4ValueBoxOutput(ns("ranking"), width = 12),
          color = "#1E90FF"
        )
      ),
      col_3(
        shinycssloaders::withSpinner(
          bs4Dash::bs4ValueBoxOutput(ns("problemas"), width = 12),
          color = "#1E90FF"
        )
      ),
      col_3(
        shinycssloaders::withSpinner(
          bs4Dash::bs4ValueBoxOutput(ns("correcoes"), width = 12),
          color = "#1E90FF"
        )
      )
    ),

    bs4Dash::box(
      width = 6,
      title = "Comparação entre os tribunais da mesma justiça",
      highcharter::highchartOutput(ns("grafico")) %>%
        shinycssloaders::withSpinner()
    ),

    bs4Dash::box(
      width = 6,
      title = "Principais inconsistências",
      reactable::reactableOutput(ns("tabela")) %>%
        shinycssloaders::withSpinner()
    )
  )
}

#' geral Server Functions
#'
#' @param id,app_data Internal parameters for {shiny}.
#'
#' @noRd
mod_geral_server <- function(id, app_data) {
  shiny::moduleServer(id, function(input, output, session){


    #' cards
    #'
    #' processos com inconsistência
    #' posição do tribunal
    #' quantidade de correções possíveis
    #' índice de inconsistência
    #'
    #'
    #'
    #' gráficos
    #'
    #' comparação de tribunais
    #' tabela de inconsistências
    #'


    output$grafico <- highcharter::renderHighchart({

      justica_tribunal <- app_data()$incos$justica[1]
      inovaCNJ::da_incos %>%
        dplyr::filter(justica == justica_tribunal) %>%
        dplyr::pull(tribunal) %>%
        forcats::fct_infreq() %>%
        highcharter::hchart(type = "bar")
    })

    output$tabela <- reactable::renderReactable({
      app_data()$incos %>%
        dplyr::select(dplyr::starts_with("inc")) %>%
        dplyr::summarise(dplyr::across(.fns = ~sum(!is.na(.x)))) %>%
        tidyr::pivot_longer(dplyr::everything()) %>%
        dplyr::arrange(dplyr::desc(value)) %>%
        reactable::reactable()
    })

    output$indice <- bs4Dash::renderbs4ValueBox({
      val <- nrow(app_data()$incos)
      text <- "texto"
      bs4Dash::bs4ValueBox(
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        subtitle = tags$p(HTML(text)),
        icon = "gauge",
        status = "success",
        width = NULL
      )
    })
    output$ranking <- bs4Dash::renderbs4ValueBox({
      val <- nrow(app_data()$incos)
      text <- "texto"

      # calcular_ranking <- function(tribunal)

      bs4Dash::bs4ValueBox(
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        subtitle = tags$p(HTML(text)),
        icon = "globe",
        status = "success",
        width = NULL
      )

    })
    output$problemas <- bs4Dash::renderbs4ValueBox({
      val <- nrow(app_data()$incos)
      text <- "texto"
      bs4Dash::bs4ValueBox(
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        subtitle = tags$p(HTML(text)),
        icon = "globe",
        status = "success",
        width = NULL
      )
    })
    output$correcoes <- bs4Dash::renderbs4ValueBox({
      val <- nrow(app_data()$incos)
      text <- "texto"
      bs4Dash::bs4ValueBox(
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        subtitle = tags$p(HTML(text)),
        icon = "globe",
        status = "success",
        width = NULL
      )
    })



  })
}

