#' feedback UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_feedback_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      width = 12,
      inputId = ns("caixa"),
      title = "Feedbacks",
      collapsible = FALSE,
      closable = FALSE,
      maximizable = FALSE,
      shiny::fluidRow(col_12(
        shiny::textAreaInput(
          ns("feedback"),
          "Como a ferramenta poderia melhorar?",
          rows = 10,
          cols = 400
        )
      )),
      shiny::fluidRow(
        shiny::actionButton(ns("enviar"), "Enviar!", icon = shiny::icon("paper-plane"))
      )
    )
  )
}

#' feedback Server Functions
#'
#' @noRd
mod_feedback_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$enviar, {
      shinyalert::shinyalert(
        "Obrigado!",
        stringr::str_glue(
          "Assim que possível responderemos sua solicitação."
        ),
        type = "success",
        closeOnClickOutside = TRUE
      )
    })

  })
}
