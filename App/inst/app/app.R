# shiny::shinyApp(inovaCNJ:::app_ui(), inovaCNJ:::app_server)
readRenviron(".Renviron")


auth0_server_verify2 <- function (session, app, api, state) {
  u_search <- session[["clientData"]]$url_search
  params <- shiny::parseQueryString(u_search)
  if (auth0:::has_auth_code(params, state)) {

    cred <- httr::oauth2.0_access_token(
      api, app(redirect_uri),
      params$code
    )

    token <- httr::oauth2.0_token(
      app = app(redirect_uri),
      endpoint = api, cache = FALSE, credentials = cred,
      user_params = list(grant_type = "authorization_code")
    )

    userinfo_url <- sub("authorize", "userinfo", api$authorize)
    resp <- httr::GET(userinfo_url, httr::config(token = token))

    assign(
      "auth0_credentials",
      token$credentials,
      envir = session$userData
    )

    ## this is the hack. Had to do this
    ## in order to manage bs4Dash redirect issue
    saveRDS(httr::content(resp, "parsed"), "auth0_info_file.rds")

    assign(
      "auth0_info",
      httr::content(resp, "parsed"),
      envir = session$userData
    )

  }
}

auth0_server2 <- function(server, info) {
  if (missing(info)) info <- auth0::auth0_info()
  function(input, output, session) {
    shiny::isolate(auth0_server_verify2(session, info$app, info$api, info$state))
    shiny::observeEvent(input[["._auth0logout_"]], auth0::logout())
    server(input, output, session)
  }
}


a0_info <- auth0::auth0_info()
shiny::shinyApp(
  auth0::auth0_ui(inovaCNJ:::app_ui(), a0_info),
  auth0_server2(inovaCNJ:::app_server, a0_info)
)
