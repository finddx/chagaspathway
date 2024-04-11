#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom ggplot2 ggplot geom_point
#' @importFrom shinyjs runjs show hidden
#' @importFrom shinyBS bsCollapse bsCollapsePanel

#' @noRd
app_server <- function(input, output, session) {

  displayed_scenarios <- reactiveVal(c("scenario1"))


  # Define a list to store module instances
  # scenario_module_instances <- list()


  #Count and add a new scenario (up tp 3)
  observeEvent(input$add_scenario, {
    if (length(displayed_scenarios()) < 3){
      current_scenario <- paste0("scenario", length(displayed_scenarios()) + 1)
      displayed_scenarios(c(displayed_scenarios(), current_scenario))
    }
  })
  #Generate UI elements for a scenario
  # generate_scenario_ui <- function(scenario_id) {
  #   if (scenario_id %in% displayed_scenarios()) {
  #     scenario_number <- as.numeric(gsub("\\D", "", scenario_id))
  #     tagList(
  #       column(width=12, id=paste0("scenarios_data_", gsub("\\D", "", scenario_id)),
  #              h4(paste("Scenario", scenario_number)),
  #              mod_scenarios_data_ui(paste0("scenarios_data_", scenario_number))
  #       )
  #     )
  #   }
  # }
  # Generate server Scenario for each displayed scenario
  observe({
    lapply(displayed_scenarios(), function(scenario_id) {
      if (scenario_id %in% displayed_scenarios()) {
        scenario_number <- as.numeric(gsub("\\D", "", scenario_id))

        assign(
          paste0("scenario", scenario_number, "_vars"),
          mod_scenarios_data_server(
            id = paste0("scenarios_data_", scenario_number),
            scenarios_n = paste0("scenarios", scenario_number)
          ),
          envir = .GlobalEnv
        )
      }
    })
  })

  # output$scenarios <- renderUI({
  #   scenario_list <- lapply(displayed_scenarios(), generate_scenario_ui)
  #   fluidRow(do.call(tagList, scenario_list))
  # })
  # # Render scenarios



  output$scenarios <- renderUI({
    # Define UI elements for scenarios 1 to 3
    tagList(
      column(width=12, id="scenarios_data_1",
             h4("Scenario 1"),
             mod_scenarios_data_ui("scenarios_data_1")
      ),
      shinyjs::hidden(
        column(width=12, id="scenarios_data_2",
             h4("Scenario 2"),
             mod_scenarios_data_ui("scenarios_data_2")
      )),
      shinyjs::hidden(
      column(width=12, id="scenarios_data_3",
             h4("Scenario 3"),
             mod_scenarios_data_ui("scenarios_data_3")
      )
      )
    )

    # Get the number of displayed scenarios
    # num_displayed <- length(displayed_scenarios())
    #
    # # Show/hide UI elements based on the number of displayed scenarios
    # if (num_displayed == 1) {
    #   shinyjs::show("scenarios_data_1")
    # } else if (num_displayed == 2) {
    #     shinyjs::show("scenarios_data_2")
    # } else if (num_displayed == 3) {
    #   shinyjs::show("scenarios_data_3")
    # }
  })











  #Generate UI elements for a result
  generate_result_ui <- function(result_id) {
    if (result_id %in% displayed_scenarios()) {
      result_number <- as.numeric(gsub("\\D", "", result_id))
      tagList(
        column(width=4, id=paste0("results_data_", gsub("\\D", "", result_id)),
               h4(paste("Results scenario", result_number), style="text-align: center; width: 100%;"),
               mod_results_data_ui(paste0("results_data_", result_number))
        )
      )
    }
  }



 #Render results
  output$results <- renderUI({
    results_list <- lapply(displayed_scenarios(), generate_result_ui)
    fluidRow(do.call(tagList, results_list))
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

  user_data <- mod_user_data_server("user_data")
  pathways <- mod_pathways_data_server("pathways_data")
  advance_settings_vars <- mod_advance_data_server("advance_data")





  # scenario1_vars <- mod_scenarios_data_server("scenarios_data_1", scenarios_n="scenario1")
  # scenario2_vars <- mod_scenarios_data_server("scenarios_data_2", scenarios_n="scenario2")
  # scenario3_vars <- mod_scenarios_data_server("scenarios_data_3", scenarios_n="scenario3")

  #Generate server Result-scenario for each displayed scenario
  observe({
    lapply(displayed_scenarios(), function(result_id) {
      if (result_id %in% displayed_scenarios()) {
        result_number <- as.numeric(gsub("\\D", "", result_id))

        mod_results_data_server(
          id = paste0("results_data_", result_number),
          event_calculate = event_calculate,
          pathways = pathways,
          advance_settings_vars=advance_settings_vars,
          scenarios_n = paste0("scenario", result_number),
          results_list  = results_data #get(paste0("scenario", result_number, "_vars"))
        )
      }
    })
  })


  # #Generate mod_results_data_server for each displayed scenario
  # observe({
  #   lapply(displayed_scenarios(), function(result_id) {
  #     if (result_id %in% displayed_scenarios()) {
  #       result_number <- as.numeric(gsub("\\D", "", result_id))
  #
  #       mod_results_data_server(
  #         id = paste0("results_data_", result_number),
  #         event_calculate = event_calculate,
  #         pathways = pathways,
  #         advance_settings_vars=advance_settings_vars,
  #         results_list = results_data#get(paste0("scenario", result_number, "_vars"))
  #       )
  #     }
  #   })
  # })


  event_calculate <- eventReactive(input$calculate, {
    input$calculate
  })

  results_data <-
    eventReactive(event_calculate(), {

      list(
        pathways = pathways,
        scenario1 =scenario1_vars(),
        scenario2 = scenario2_vars(),
        scenario3 = scenario3_vars()
      )

    })



  mod_results_server("results_general", event_calculate=event_calculate, results_list=results_data )

                     # user_data=user_data, pathways=pathways, advance_settings_vars=advance_settings_vars, scenario1_vars=scenario1_vars, scenario2_vars=scenario2_vars, scenario3_vars=scenario3_vars

  # mod_results_data_server(
  #   id = "results_data_1",
  #   event_calculate = event_calculate,
  #   pathways = pathways,
  #   advance_settings_vars=advance_settings_vars,
  #   scenario_vars = "scenario1_vars"
  # )


  # results_data <- eventReactive(event_calculate(), {
  #
  #     sensitivity <- scenario_vars$test1$test_type()
  #     facility_type <- scenario_vars$pathway_type()
  #
  #
  #     list(sensitivity=sensitivity, facility_type=facility_type)
  #
  #
  #
  #
  #   })







}
