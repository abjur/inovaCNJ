cat(stringr::str_glue(
  "BD_IP={Sys.getenv('BD_IP')}",
  "\nBD_PWD={Sys.getenv('BD_PWD')}",
  "\nAUTH0_KEY={Sys.getenv('AUTH0_KEY')}",
  "\nAUTH0_SECRET={Sys.getenv('AUTH0_SECRET')}",
  "\nGITHUB_PAT={Sys.getenv('GITHUB_PAT')}",
  "\n"
), file = "App/inst/app/.Renviron")

# deploy app
rsconnect::setAccountInfo(
  name = 'abjur',
  token = Sys.getenv('SHINYAPPS_TOKEN'),
  secret = Sys.getenv('SHINYAPPS_SECRET')
)

rsconnect::deployApp('App/inst/app', appName = "inovaCNJ")
