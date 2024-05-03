#' results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    layout_column_wrap(
       card(
        card_header(h4(strong("PPV plot"))),
        full_screen=TRUE,
        card_body(
          plotlyOutput(ns("out_plot_ppv"), width="100%")
        )),
      card(
        card_header(h4(strong("NPV plot"))),
        full_screen=TRUE,
        card_body(
          plotlyOutput(ns("out_plot_npv"), width="100%")
        )),
      card(
        card_header(h4(strong("OPC plot"))),
        full_screen=TRUE,
        card_body(
          plotlyOutput(ns("out_plot_cpc"), width="100%")
        ))
      ),
      card(
        # card_header(h4(strong("User data"))),
        full_screen=TRUE,
        card_body(
          gt_output(ns("out_table_res"))
        )
      )


  )
}

#' results Server Functions
#'
#' @noRd
#'
#'
#'
#'
mod_results_server <- function(id, results_list){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # req(results_list())
    results_data <- results_list()
    #Calculate plots
    df_params <- list()
    if (!is.null(results_data$params_scenario1)) {
      df_params [[1]] <- make_prev_df(results_data$params_scenario1)
    }
    if (!is.null(results_data$params_scenario2)) {
      df_params [[2]] <- make_prev_df(results_data$params_scenario2)
    }
    if (!is.null(results_data$params_scenario3)) {
      df_params [[3]] <- make_prev_df(results_data$params_scenario3)
    }
    prev_df <- do.call(rbind, df_params)
    plot_ppv <- ggplotly(make_plots(prev_df, "ppv")) %>%
      layout(legend=list(orientation="h", x=0.4, y=-0.2))
      # layout(legend=list(orientation="h", x=ifelse(is.null(results_data$params_scenario2) & is.null(results_data$params_scenario3), 0.4, ifelse(!is.null(results_data$params_scenario2) & is.null(results_data$params_scenario3), 0.3, 0.2)), y=-0.2))
    plot_npv <- ggplotly(make_plots(prev_df, "npv")) %>%
      layout(legend=list(orientation="h", x=0.4, y=-0.2))
    plot_cpc <- ggplotly(make_plots(prev_df, "cpc")) %>%
      layout(legend=list(orientation="h", x=0.4, y=-0.2))
    #Render plots
    output$out_plot_ppv <- renderPlotly({
      plot_ppv
      })
    output$out_plot_npv <- renderPlotly({
      plot_npv
      })
    output$out_plot_cpc <- renderPlotly({
      plot_cpc
      })

    # #Calculate table
    out <- rbind(results_data$out_scenario1, results_data$out_scenario2, results_data$out_scenario3)
    table_res <- make_table_results(out)
    #Render table
    output$out_table_res <-render_gt({
      table_res
    })

    # Return outputs for html report
    return(
      list(
        plot_ppv = plot_ppv,
        plot_npv = plot_npv,
        plot_cpc = plot_cpc,
        table_res = table_res
      )
    )

  })
}

