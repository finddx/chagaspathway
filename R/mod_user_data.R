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
    fluidRow(
      column(3, textInput(ns("user_name"), label=HTML("<b> User name </b>"),  width = "100%"))
    )

  )
}

#' pathways_tab Server Functions
#'
#' @noRd
mod_user_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      list(
        user_name = reactive({ input$ user_name })
      )
    )

  })
}

