# write credentials to use auth0 inside app
yaml <- list(
  name = "cnjInova",
  remote_url = "https://abjur.shinyapps.io/inovaCNJ",
  auth0_config = list(
    api_url = "https://cnj-inova.us.auth0.com",
    credentials = list(
      key = Sys.getenv("AUTH0_KEY"),
      secret = Sys.getenv("AUTH0_SECRET")
    )
  )
)
yaml::write_yaml(yaml, "App/inst/app/_auth0.yml")

cat(stringr::str_glue(
  "BD_IP={Sys.getenv('BD_IP')}",
  "\nBD_PWD={Sys.getenv('BD_PWD')}"
), file = "App/inst/app/.Renviron")

# deploy app
rsconnect::setAccountInfo(
  name = 'abjur',
  token = Sys.getenv('SHINYAPPS_TOKEN'),
  secret = Sys.getenv('SHINYAPPS_SECRET')
)

rsconnect::deployApp('App/inst/app', appName = "inovaCNJ")
