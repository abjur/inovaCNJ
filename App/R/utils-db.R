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
