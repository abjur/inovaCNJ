conectar <- function() {
  RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = "inovaCNJ",
    host = Sys.getenv("BD_IP"),
    port = 5432,
    user = "admin",
    password = Sys.getenv("BD_PWD")
  )
}

desconectar <- function(con) {
  RPostgres::dbDisconnect(con)
}

usuarios_admin <- function() {
  c(
    "local",
    "admin@inovacnj.com"
  )
}

auth_admin <- function(session) {
  usuario <- session$userData$auth0_info$name
  usuario <- ifelse(is.null(usuario), "local", usuario)
  usuario %in% usuarios_admin()
}

deploy <- function() {
  r <- httr::POST(
    "https://api.github.com/repos/abjur/inovaCNJ/dispatches",
    httr::accept("application/vnd.github.everest-preview+json"),
    httr::authenticate("jtrecenti", Sys.getenv("GITHUB_PAT")),
    encode = "json",
    body = list(event_type = "update")
  )
  invisible()
}

apagar_todas_tabelas <- function() {
  con <- conectar()
  tabs <- RPostgres::dbListTables(con)
  purrr::walk(tabs, ~RPostgres::dbRemoveTable(con, .x))
  desconectar(con)
}
