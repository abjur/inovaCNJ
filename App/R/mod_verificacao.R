#' verificacao UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_verificacao_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' verificacao Server Functions
#'
#' @noRd 
mod_verificacao_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_verificacao_ui("verificacao_ui_1")
    
## To be copied in the server
# mod_verificacao_server("verificacao_ui_1")
