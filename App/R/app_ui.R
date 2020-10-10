#' The application User-Interface
#'
#' @import shiny
#' @noRd
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    bs4Dash::dashboardPage(

      # ----
      controlbar = bs4Dash::dashboardControlbar(),
      navbar = bs4Dash::dashboardHeader(
        rightUi = auth0::logoutButton(icon = icon("sign-out-alt"))
      ),

      # ----
      sidebar = bs4Dash::dashboardSidebar(
        skin = "light",
        title = "{cnjInova}",
        bs4Dash::bs4SidebarMenu(
          bs4Dash::bs4SidebarMenuItem(
            "Geral",
            tabName = "menu_geral",
            icon = "bullseye"
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Inconsistencias",
            tabName = "menu_incos",
            icon = "database"
          )
        )
      ),

      # ----
      body = bs4Dash::dashboardBody(
        fresh::use_theme(create_theme_css()),
        bs4Dash::bs4TabItems(
          bs4Dash::bs4TabItem(
            tabName = "menu_geral",
            mod_geral_ui("geral_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "menu_incos",
            mod_incos_ui("incos_ui_1")
          )
        )
      ),

      # ----
      footer = bs4Dash::dashboardFooter(
        copyrights = a(
          href = "https://abj.org.br",
          target = "_blank", "ABJ"
        ),
        right_text = "2020 | desenvolvido com <3 pela ABJ"
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
      app_title = 'cnjInova'
    ),
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

