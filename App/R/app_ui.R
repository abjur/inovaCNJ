#' The application User-Interface
#'
#' @import shiny
#' @noRd
app_ui <- function() {

  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    bs4Dash::dashboardPage(

      # ----
      navbar = bs4Dash::dashboardHeader(
        rightUi = auth0::logoutButton(icon = icon("sign-out-alt"))
      ),

      # ----
      sidebar = bs4Dash::dashboardSidebar(
        skin = "light",
        title = "{cnjInova}",
        bs4Dash::bs4SidebarMenu(
          bs4Dash::bs4SidebarMenuItem(
            "Feedback",
            tabName = "menu_teste",
            icon = "comments"
          )
        )
      ),

      # ----
      body = bs4Dash::dashboardBody(
        bs4Dash::bs4TabItems(
          bs4Dash::bs4TabItem(
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

