# shiny::shinyApp(inovaCNJ:::app_ui(), inovaCNJ:::app_server)
readRenviron("my_renviron")
auth0::shinyAppAuth0(inovaCNJ:::app_ui(), inovaCNJ:::app_server)
