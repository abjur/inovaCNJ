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
          bs4Dash::bs4ValueBoxOutput(ns("ranking"), width = 12),
          color = "#1E90FF"
        )
      ),
      col_3(
        shinycssloaders::withSpinner(
          bs4Dash::bs4ValueBoxOutput(ns("indice"), width = 12),
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

    shiny::fluidRow(
      bs4Dash::box(
        closable = FALSE,
        width = 6,
        title = "Comparação entre os tribunais da mesma justiça",
        highcharter::highchartOutput(ns("grafico"), height = "600px") %>%
          shinycssloaders::withSpinner()
      ),

      bs4Dash::box(
        closable = FALSE,
        width = 6,
        title = "Principais inconsistências",
        reactable::reactableOutput(ns("tabela")) %>%
          shinycssloaders::withSpinner()
      )
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

    output$grafico <- highcharter::renderHighchart({



      da <- app_data()$totais_justica %>%
        dplyr::mutate(
          indice = round(indice ^ 4 * 100),
          tribunal_escolhido = dplyr::case_when(
            tribunal == app_data()$totais_tribunal$tribunal ~ tribunal,
            TRUE ~ "Outro"
          ),
          um = "Índice",
        )

      da %>%
        highcharter::hchart(
          "bar", highcharter::hcaes(
            y = "indice",
            x = "tribunal",
            group = "um",
            color = "tribunal_escolhido"
          )
        ) %>%
        highcharter::hc_yAxis(title = list(text = 'Índice de Qualidade')) %>%
        highcharter::hc_xAxis(title = list(text = 'Tribunal'))

    })

    output$tabela <- reactable::renderReactable({
      app_data()$incos %>%
        dplyr::select(dplyr::starts_with("inc")) %>%
        dplyr::summarise(dplyr::across(.fns = ~sum(!is.na(.x)))) %>%
        tidyr::pivot_longer(dplyr::everything()) %>%
        dplyr::arrange(dplyr::desc(value)) %>%
        dplyr::mutate(
          name = stringr::str_replace(name,'inc_',''),
          name = stringr::str_to_title(name),
          name = stringr::str_replace_all(name, "_", " ")
        ) %>%
        reactable::reactable(
          defaultPageSize = 20,
          columns = list(
            name = reactable::colDef(name = 'Inconsistência'),
            value = reactable::colDef(name = 'Quantidade')
          )
        )
    })

    output$indice <- bs4Dash::renderbs4ValueBox({
      val <- round(app_data()$totais_tribunal$indice ^ 4 * 100)

      colour <- dplyr::case_when(
        val < 50 ~ "danger",
        val < 90 ~ "warning",
        TRUE ~ "success"
      )

      text <- "Índice de qualidade dos dados. Varia de zero a cem."
      bs4Dash::bs4ValueBox(
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        subtitle = tags$p(HTML(text)),
        icon = "chart-line",
        status = colour,
        width = NULL
      )
    })
    output$ranking <- bs4Dash::renderbs4ValueBox({
      val <- paste0(
        app_data()$totais_tribunal$tribunal, " (",
        app_data()$totais_tribunal$ranking, "/",
        max(app_data()$totais_justica$ranking), ")"
      )
      raz <- app_data()$totais_tribunal$ranking / max(app_data()$totais_justica$ranking)

      colour <- dplyr::case_when(
        raz < 0.2 ~ "success",
        raz < .8 ~ "warning",
        TRUE ~ "danger"
      )

      text <- "Ranking comparando com os tribunais da mesma justiça."

      bs4Dash::bs4ValueBox(
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        subtitle = tags$p(HTML(text)),
        icon = "trophy",
        status = colour,
        width = NULL
      )

    })

    output$problemas <- bs4Dash::renderbs4ValueBox({
      val <- nrow(app_data()$incos) %>%
        scales::number(big.mark = ".", decimal.mark = ",")
      text <- "Quantidade de processos com alguma inconsistência."
      bs4Dash::bs4ValueBox(
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        subtitle = tags$p(HTML(text)),
        icon = "exclamation",
        status = "danger",
        width = NULL
      )
    })
    output$correcoes <- bs4Dash::renderbs4ValueBox({
      val <- app_data()$incos %>%
        dplyr::summarise(dplyr::across(dplyr::starts_with("sol"), ~sum(!is.na(.x)))) %>%
        tidyr::pivot_longer(dplyr::everything()) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::pull(value) %>%
        scales::number(big.mark = ".", decimal.mark = ",")
      text <- "Quantidade de correções possíveis."
      bs4Dash::bs4ValueBox(
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        subtitle = tags$p(HTML(text)),
        icon = "thermometer",
        status = "primary",
        width = NULL
      )
    })



  })
}

