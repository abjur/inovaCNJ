#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  # googledrive::drive_auth(
  #   cache = ".secrets",
  #   email = TRUE
  # )

  # autenticar_gsheets()
  #
  # # caminho_planilha <- paste0(tempdir(), "/planilha.xlsx")
  # id_planilha <- "12BgnjcC85kS7cVFkw83Xk7myyP_OXZEbNtTZyDQgiaM"
  # # baixar_planilha(id_planilha, caminho_planilha)
  #
  #
  # dados <- ler_planilha(id_planilha)
  # print(dados)

  # autenticar_gsheets()
  mod_geral_server("geral_ui_1")
  mod_incos_server("incos_ui_1")

}
