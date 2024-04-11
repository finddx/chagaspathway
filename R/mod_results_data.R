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
    uiOutput(ns("value1"), style="text-align: center; width: 100%;"),
    uiOutput(ns("value2"), style="text-align: center; width: 100%;"),
    uiOutput(ns("value3"), style="text-align: center; width: 100%;")#,
    # uiOutput(ns("value4"), style="text-align: center; width: 100%;")
  )
}

#' results_data Server Functions
#'
#' @noRd
mod_results_data_server <- function(id, event_calculate, pathways, advance_settings_vars, scenarios_n, results_list){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    results_data <- results_list()
  # print(scenario_vars()$sensitivity)
    # results_data <-
    #   eventReactive(event_calculate(), {
    #
    #   scenario_vars <- reactiveValues(parameters=scenario_vars())
    #
    #   sensitivity <- scenario_vars$parameters$test1$test_type()
    #   sensitivity1 <- scenario_vars$parameters$test3$test_type()
    #   facility_type <- scenario_vars$parameters$pathway_type
    #
    #   list(sensitivity=sensitivity, sensitivity1=sensitivity1, facility_type=facility_type)#sensitivity2=sensitivity2,
    #
    # })

    # print(paste0("Res:",scenario_vars$test1$test_type(), " pah", scenario_vars$pathway_type(), "Res:",scenario_vars$test2$test_type()))

    # print(rv$x$test1$test_type())
    # print(scenario_vars()$test3$test_type())
    # print(results_data[[scenarios_n]]$test1)
    # print(scenarios_n)
    # results_data$scenario1$scenario1$test1$test_type


    output$value1 <- renderText({
      paste0("<b>Sensitivity: </b>", results_data[[scenarios_n]]$test1$test_type())
    })
    #
    output$value2 <- renderText({
      paste0("<b>facility_type: </b>", results_data[[scenarios_n]]$pathway_type)
    })
    #
    # output$value3 <- renderText({
    #   paste0("<b>facility_type: </b>", results_data$sensitivity1)
    # })



  })
}
