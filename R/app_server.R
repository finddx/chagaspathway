#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom ggplot2 ggplot geom_point
#' @importFrom shinyjs runjs
#' @importFrom shinyBS bsCollapse bsCollapsePanel

#' @noRd
app_server <- function(input, output, session) {

  displayed_scenarios <- reactiveVal(c("scenario1"))

  #Add a new scenario (up tp 3)
  observeEvent(input$add_scenario, {
    if (length(displayed_scenarios()) < 3){
      current_scenario <- paste0("scenario", length(displayed_scenarios()) + 1)
      displayed_scenarios(c(displayed_scenarios(), current_scenario))
    }
  })

  #Generate UI elements for a scenario
  generate_scenario_ui <- function(scenario_id) {
    if (scenario_id %in% displayed_scenarios()) {
      scenario_number <- as.numeric(gsub("\\D", "", scenario_id))
      tagList(
          column(width=4, id=paste0("scenarios_data_", gsub("\\D", "", scenario_id)),
                 h4(paste("Scenario", scenario_number)),
                 mod_scenarios_data_ui(paste("scenarios_data_", scenario_number, sep = ""))
          )
      )
    }
  }

  #Render scenarios
  output$scenarios <- renderUI({
    scenario_list <- lapply(displayed_scenarios(), generate_scenario_ui)
    fluidRow(do.call(tagList, scenario_list))
  })


  #
  # output$scenario1 <- renderUI({
  #   if ("scenario1" %in% displayed_scenarios()) {
  #     tagList(
  #       h3("Scenario 1"),
  #       mod_scenarios_data_ui("scenarios_data_1")
  #     )
  #   }
  # })
  #
  # output$scenario2 <- renderUI({
  #   if ("scenario2" %in% displayed_scenarios()) {
  #     tagList(
  #       h3("Scenario 2"),
  #       mod_scenarios_data_ui("scenarios_data_2")
  #     )
  #   }
  # })
  #
  # output$scenario3 <- renderUI({
  #   if ("scenario3" %in% displayed_scenarios()) {
  #     tagList(
  #       h3("Scenario 3"),
  #       mod_scenarios_data_ui("scenarios_data_3")
  #     )
  #   }
  # })

  mod_user_data_server("user_data")


  #ADVANCE SETTINGS PER SCENARIO OR GENERAL???
  output$collapse_settings <- renderUI({

  bsCollapse(id="Advance settings",
             open=NULL,
             bsCollapsePanel("Advance settings", mod_advance_data_ui("advance_data"), style="info")
  )

  })


  pathways <- mod_pathways_data_server("pathways_data")
  advance_settins_vars <- mod_advance_data_server("advance_data")
  scenario1_vars <- mod_scenarios_data_server("scenarios_data_1")
  scenario2_vars <-mod_scenarios_data_server("scenarios_data_2")
  scenario3_vars <-mod_scenarios_data_server("scenarios_data_3")


  event_calculate <- eventReactive(input$calculate, {
    input$calculate
  })


  mod_results_server("results", event_calculate=event_calculate,  pathways=pathways, scenario1_vars=scenario1_vars, scenario2_vars=scenario2_vars, scenario3_vars=scenario3_vars, advance_settins_vars=advance_settins_vars)




}
