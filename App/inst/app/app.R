# shiny::shinyApp(inovaCNJ:::app_ui(), inovaCNJ:::app_server)
readRenviron(".Renviron")

a0_info <- auth0::auth0_info(auth0::auth0_config("_auth0.yml"))
shiny::shinyApp(
  auth0::auth0_ui(inovaCNJ:::app_ui(), a0_info),
  auth0::auth0_server(inovaCNJ:::app_server, a0_info)
)
