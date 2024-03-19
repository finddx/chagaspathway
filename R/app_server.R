#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom ggplot2 ggplot geom_point
#' @importFrom shinyjs runjs

#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  observeEvent(input$add_scenario, {
    current_scenario <- as.numeric(isolate(input$current_scenario))
    if (current_scenario < 3) {
      next_scenario <- current_scenario + 1
      updateTextInput(session, "current_scenario", value = next_scenario)
      show_scenario_id <- paste0("#scenarios_data_", next_scenario)
      runjs(paste0('$("#scenarios_data_', next_scenario, '").show();'))
    }
  })


  mod_user_data_server("user_data")
  mod_pathways_data_server("pathways_data")

  # execute plot variable selection modules
  model1_vars <- mod_scenarios_data_server("scenarios_data_1")
  # plot2vars <- callModule(varselect_mod_server, "plot2_vars")

  # execute scatterplot module
  # res <- callModule(scatterplot_mod_server,
  #                   "plots",
  #                   dataset = ames,
  #                   model1_vars = model1_vars)

  mod_results_server("results", model1_vars=model1_vars)

  # mod_scenarios_data_server("scenarios_data_2")
  # mod_scenarios_data_server("scenarios_data_3")


}
