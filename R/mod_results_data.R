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
    grVizOutput(ns("out_fig_diagram"), width = "100%"),
    uiOutput(ns("value3"), style="text-align: center; width: 100%;"),
    uiOutput(ns("value4"), style="text-align: center; width: 100%;"),
    plotOutput(ns("out_plot_ppv"), width="100%"),
    plotOutput(ns("out_plot_npv"), width="100%"),
    plotOutput(ns("out_plot_cpc"), width="100%")
  )
}

#' results_data Server Functions
#'
#' @noRd
mod_results_data_server <- function(id, scenarios_n, results_list){# event_calculate,
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    results_data <- results_list()

    tmp_params <- format_app_params_react(scenario_vars=results_data[[scenarios_n]], global_vars=results_data$pathways, advance_vars=results_data$advance)

    params <- make_params(
      tmp_params$pathway,
      tmp_params$prev,
      tmp_params$test1,
      tmp_params$test2,
      tmp_params$test3,
      tmp_params$test4,
      tmp_params$test5,
      tmp_params$daly_avert_per_tx,
      tmp_params$tx_eff,
      tmp_params$n
    )

#
    out <- run_pathway(params)
    # prop diagnosed
#     out$n_true_pos/(params$n*params$prev)
#     # cost per case diagnosed
#     out$total_cost / out$n_true_pos
# #

    # Proportion cases diagnosed:
    prop_diagnosed <- round(out$prop_diagnosed*100, 1)#}%

    # Cost per case diagnosed:
    cost_per_true_pos <- round(out$cost_per_true_pos*100, 2)

    #Make plots
    plot_ppv <- make_plots(params, "ppv")
    plot_npv <- make_plots(params, "npv")
    plot_cpc <- make_plots(params, "cpc")

    #Make diagram
    fig_diagram <- render_graph(make_pathway_diagram(params))

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
    # print(scenario_vars()$test3$test_type())
    # print(results_data[[scenarios_n]]$test1)
    # print(results_data$scenario1$test1$test_type())


    output$value1 <- renderText({
      paste0("<b>Sensitivity: </b>", results_data[[scenarios_n]]$test1$test_type())
    })
    output$value2 <- renderText({
      paste0("<b>facility_type: </b>", results_data[[scenarios_n]]$pathway_type)
    })

    output$value3 <- renderText({
      paste0("<b>Proportion cases diagnosed: </b>", prop_diagnosed)
    })
    output$value4 <- renderText({
      paste0("<b>Cost per case diagnosed: </b>", cost_per_true_pos)
    })

    #Render plots
    output$out_fig_diagram <- renderGrViz({
      fig_diagram
    })

    output$out_plot_ppv <- renderPlot({
      plot_ppv
      })

    output$out_plot_npv <- renderPlot({
      plot_npv
      })

    output$out_plot_cpc <- renderPlot({
      plot_cpc
      })





  })
}
