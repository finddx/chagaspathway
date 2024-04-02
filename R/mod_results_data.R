#' results_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_results_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("value1")),
    uiOutput(ns("value2"))
  )
}

#' results_data Server Functions
#'
#' @noRd
mod_results_data_server <- function(id, event_calculate, pathways, scenario_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    results_data <- eventReactive(event_calculate(), {

      sensitivity <- scenario_vars$sensitivity()
      specificity <-scenario_vars$specificity()

      list(sensitivity=sensitivity, specificity=specificity)
    })

    output$value1 <- renderText({
      paste0("<b>Sensitivity: </b>", results_data()$sensitivity)
    })

    output$value2 <- renderText({
      paste0("<b>Specificity: </b>", results_data()$specificity)
    })

  })
}
