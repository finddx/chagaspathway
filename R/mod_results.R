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
       card(
        # card_header(h4(strong("User data"))),
        full_screen=TRUE,
        card_body(
          column(width=12,align="center",
                 plotlyOutput(ns("out_plot_ppv"), width="100%")
          )
        )),
      card(
        # card_header(h4(strong("User data"))),
        full_screen=TRUE,
        card_body(
          column(width=12,align="center",
                 plotlyOutput(ns("out_plot_npv"), width="100%")#plotOutput
          )
        )),
      card(
        # card_header(h4(strong("User data"))),
        full_screen=TRUE,
        card_body(
          column(width=12,align="center",
                 plotlyOutput(ns("out_plot_cpc"), width="100%")
          )
        )),

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
mod_results_server <- function(id, results_list){# event_calculate,out_scenario1=NULL, out_scenario2=NULL, out_scenario3=NULL
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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
    plot_ppv <- ggplotly(make_plots(prev_df, "ppv"))
    plot_npv <- ggplotly(make_plots(prev_df, "npv"))
    plot_cpc <- ggplotly(make_plots(prev_df, "cpc"))
    #Render plots
    output$out_plot_ppv <- renderPlotly({# renderPlot({
      plot_ppv
      })
    output$out_plot_npv <- renderPlotly({
      plot_npv
      })
    output$out_plot_cpc <- renderPlotly({
      plot_cpc
      })

    #Calculate table
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

    # results_data <- reactive({
    #   results_list()
    # })
    # observe({
      # res_pathway <- results_data()$scenario1$pathway_type
      # res_pathway <- results_data$scenario1$pathway_type


  })
}

