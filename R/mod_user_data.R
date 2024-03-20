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
    textInput(ns("name"), label=HTML("<b> User name </b>"),  width = "100%"),
    uiOutput(ns("date"))

  )
}

#' pathways_tab Server Functions
#'
#' @noRd
mod_user_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    current_date <- format(Sys.Date(), "%b %d %Y")
    output$date <- renderText({ paste0("<b>Date: </b>", current_date)})

  })
}

