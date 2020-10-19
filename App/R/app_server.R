#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  shiny::observe({
    print(as.list(session$userData))
  })

  mod_teste_server("teste_ui_1")

}
