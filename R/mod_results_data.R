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
    card(
      # card_header(h4(strong("User data"))),
      full_screen=TRUE,
      card_body(
        grVizOutput(ns("out_fig_diagram"), width="100%")
    )),
    card(
      # card_header(h4(strong("User data"))),
      full_screen=TRUE,
      card_body(
    uiOutput(ns("out_values_box"))
      ))
  )
}

#' results_data Server Functions
#'
#' @noRd
mod_results_data_server <- function(id, scenarios_n, results_list){# event_calculate,
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    results_data <- results_list()

    # tmp_params <- format_app_params_react(scenario_vars=results_data[[scenarios_n]], global_vars=results_data$pathways, advance_vars=results_data$advance, scn_lab=scenarios_n)
    #
    # params <- make_params(
    #   tmp_params$pathway,
    #   tmp_params$prev,
    #   tmp_params$test1,
    #   tmp_params$test2,
    #   tmp_params$test3,
    #   tmp_params$test4,
    #   tmp_params$test5,
    #   tmp_params$daly_avert_per_tx,
    #   tmp_params$tx_eff,
    #   tmp_params$n,
    #   tmp_params$scenario
    # )
    # out <- run_pathway(params)
    # #Make diagram
    fig_diagram <- render_graph(make_pathway_diagram(results_data[[paste0("params_" ,scenarios_n)]]))
    #Proportion cases diagnosed:
    prop_diagnosed <- round(results_data[[paste0("out_" ,scenarios_n)]]$prop_diagnosed*100, 1)
    #Cost per case diagnosed:
    cost_per_true_pos <- round(results_data[[paste0("out_" ,scenarios_n)]]$cost_per_true_pos, 2)
    #NPV:
    ppv <- round(results_data[[paste0("out_" ,scenarios_n)]]$ppv*100, 1)
    #PPV:
    npv <- round(results_data[[paste0("out_" ,scenarios_n)]]$npv*100, 1)
    #Make boxes
    values_box <- fluidRow(
      column(offset=0, width=12,
             layout_column_wrap(
                width = 1/2,
                # style = "display: flex; align-items: center; justify-content: center;",
                  value_box(
                    title = "Proportion cases diagnosed:",
                    value = paste0(prop_diagnosed, "%"),
                    showcase = bs_icon("search"),
                    theme = value_box_theme(bg="#489FA9", fg="#FFFFFF")
                  ),
                  value_box(
                    title = "Cost per case diagnosed:",
                    value = cost_per_true_pos,
                    showcase = bs_icon("currency-dollar"),
                    theme = value_box_theme(bg="#81969F", fg="#FFFFFF")
                  )
            )
          ),
      column(offset=0, width=12,
             layout_column_wrap(
               width = 1/2,
               value_box(
                 title = "Positive predictive value:",
                 value = paste0(ppv, "%"),
                 showcase = bs_icon("plus-circle-fill"),
                 theme = value_box_theme(bg="#354159", fg="#FFFFFF")
               ),
               value_box(
                 title = "Negative predictive value:",
                 value = paste0(npv, "%"),
                 showcase = bs_icon("dash-circle-fill"),
                 theme = value_box_theme(bg="#C7C5A7", fg="#FFFFFF")
               )
             )
      )
    )
    #Table
    table_params <- make_table_params(results_data[[paste0("params_" ,scenarios_n)]])


    #Render diagram
    output$out_fig_diagram <- renderGrViz({
      fig_diagram
    })
    #Render boxes
    output$out_values_box <- renderUI({
      values_box
    })

    #Return outputs for html report
    return(
      list(
        fig_diagram = fig_diagram,
        values_box = values_box,
        table_params =table_params
      )
    )


  })
}
