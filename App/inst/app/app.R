# shiny::shinyApp(inovaCNJ:::app_ui(), inovaCNJ:::app_server)
readRenviron(".Renviron")

library(auth0)

auth0::shinyAppAuth0(inovaCNJ:::app_ui(), inovaCNJ:::app_server)
