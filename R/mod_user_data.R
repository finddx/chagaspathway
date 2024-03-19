#' user_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_user_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    textInput("name", label=HTML("<b> User name </b>"), value="Please put your name"),

  )
}

#' pathways_tab Server Functions
#'
#' @noRd
mod_user_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # user_name <- input$name
    # date(format="%Y")
    # date <- format(Sys.time(), "%b %e %Y")
  })
}

