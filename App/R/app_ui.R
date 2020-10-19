#' The application User-Interface
#'
#' @import shiny
#' @noRd
app_ui <- function() {

  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    shinydashboard::dashboardPage(

      # ----
      header = shinydashboard::dashboardHeader(),

      # ----
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(
            "Feedback",
            tabName = "menu_teste",
            icon = shiny::icon("comments")
          )
        )
      ),

      # ----
      body = shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "menu_teste",
            mod_teste_ui("teste_ui_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'inovaCNJ'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

