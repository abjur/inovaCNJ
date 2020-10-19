# shiny::shinyApp(inovaCNJ:::app_ui(), inovaCNJ:::app_server)
readRenviron(".Renviron")
auth0::shinyAppAuth0(inovaCNJ:::app_ui, inovaCNJ:::app_server)
