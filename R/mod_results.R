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
                 plotOutput(ns("out_plot_ppv"), width="100%")
          )
        )),
      card(
        # card_header(h4(strong("User data"))),
        full_screen=TRUE,
        card_body(
          column(width=12,align="center",
                 plotOutput(ns("out_plot_npv"), width="100%")
          )
        )),
      card(
        # card_header(h4(strong("User data"))),
        full_screen=TRUE,
        card_body(
          column(width=12,align="center",
                 plotOutput(ns("out_plot_cpc"), width="100%")
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


    # fig_diagram <- render_graph(make_pathway_diagram(results_data[[paste0("params_" ,scenarios_n)]]))




    # if (!is.null(out_scenario1)){
    out <- rbind(results_data$out_scenario1, results_data$out_scenario2, results_data$out_scenario3)
    table_res <- make_table_results(out)

    prev_df = rbind(make_prev_df(results_data$params_scenario1), make_prev_df(results_data$params_scenario1), make_prev_df(results_data$params_scenario3))
    plot_ppv <- make_plots(prev_df, "ppv")
    plot_npv <- make_plots(prev_df, "npv")
    plot_cpc <- make_plots(prev_df, "cpc")



    #Render plots
    output$out_plot_ppv <- renderPlot({
      plot_ppv
      })
    output$out_plot_npv <- renderPlot({
      plot_npv
      })
    output$out_plot_cpc <- renderPlot({
      plot_cpc
      })
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


    # }



    # observe({
    #   for (i in 1:3) {
    #     module_name <- paste0("results", i)
    #
    #     # Check if module exists
    #     if (!exists(module_name)) {
    #       # Dynamically create module if it doesn't exist
    #       module_results[[module_name]] <- callModule(plotModule, module_name)
    #     }
    #   }
    # })


    # results_data <- reactive({
    #   results_list()
    # })


    # params  <- results_list

    # print(params)
     # results_data[[scenarios_n]]$test1$test_type()

    # observe({
      # res_pathway <- results_data()$scenario1$pathway_type
      # res_pathway <- results_data$scenario1$pathway_type
      # res_prev <- 50
      # res_lftu <- results_data$pathways$lftu
      # res_test1 <- results_data$scenario1$test1


      # params <- format_app_params_react(scenario_vars=results_data$scenario1, global_vars=results_data$pathways, advance_vars=results_data$advance)
      # print(params)
      # print(results_data$scenario1$test1$test_type)

      # params <- make_params(pathway=res_pathway, prev=res_prev, test1=res_test1, test2=res_test2, test3=res_test3, test4=res_test4, test5=res_test5, daly_avert_per_tx=res_daly_avert_per_tx, treat_effect=res_treat_effect)
    # })


    # params
    #
    # DiagrammeR::render_graph(make_pathway_diagram(params))

    # pathways, user_data, advance_settings_vars, scenario1_vars, scenario2_vars, scenario3_vars

    #Get the reactive values
    # results_data <- eventReactive(event_calculate(), {
    #
    #   user_name <- user_data$user_name()
    #
    #   sensitivity_scenario1 <- scenario1_vars$sensitivity()
    #   specificity_scenario1 <- scenario1_vars$specificity()

    #
    #   list(user_name=user_name,
    #        sensitivity_scenario1=sensitivity_scenario1, specificity_scenario1=specificity_scenario1,
    #        sensitivity_scenario2=sensitivity_scenario2, specificity_scenario2=specificity_scenario2,
    #        sensitivity_scenario3=sensitivity_scenario3, specificity_scenario3=specificity_scenario3
    #        )
    # })


    # df_scatter <- reactive({
    #   req(results_data())
    #
    #   df <- data.frame(
    #     sensitivity = c(
    #       results_data()$sensitivity_scenario1,
    #       results_data()$sensitivity_scenario2,
    #       results_data()$sensitivity_scenario3
    #     ),
    #     specificity = c(
    #       results_data()$specificity_scenario1,
    #       results_data()$specificity_scenario2,
    #       results_data()$specificity_scenario3
    #     )
    #   )
    #   return(df)
    # })

    #Render the results based on the event reactive
    # scatterplot_plot <- reactive({
    #   req(df_scatter())
    #
    #   ggplot(df_scatter(), aes(x=sensitivity, y=specificity)) +
    #     geom_point() +
    #     labs(x = "Sensitivity", y = "Specificity", title = "Scatter Plot")
    # })
    #    #
    # output$scatterplot2 <- renderPlot({
    #   scatterplot2_plot()
    #   })






# PLOT from previous exercise
#     output$scatterplot <- renderPlot({
#       req(results_data())
#
#       plot_scatter <- ggplot(mapping = aes(x=results_data()$sensitivity_scenario1, y=results_data()$specificity_scenario1)) +
#         geom_point() +
#         labs(x = "Sensitivity", y = "Specificity", title = "Scatter Plot")
#
#       return(plot_scatter)
#     })
#
#     output$scatterplot2 <- renderPlot({
#       req(results_data())
#       plot_scatter <- ggplot(mapping = aes(x=results_data()$sensitivity_scenario1, y=results_data()$specificity_scenario1)) +
#         geom_point() +
#         labs(x = "Sensitivity", y = "Specificity", title = "Scatter Plot")
#       return(plot_scatter)
#     })






    # output$report <- downloadHandler(
    #
    #   filename <-  "chagaspathway_report.html",
    #   content = function(file) {
    #     tempReport <- file.path(tempdir(), "chagaspathway_report.Rmd")
    #     file.copy("chagaspathway_report.Rmd", tempReport, overwrite=TRUE)
    #
    #     rmarkdown::render(tempReport,
    #                       output_file=file,
    #                       params=list(
    #                         # user_name = results_data()$user_name,
    #                         # sensitivity_scenario1 = results_data()$sensitivity_scenario1,
    #                         # scatterplot_plot=scatterplot_plot(),
    #                         fig_diagram_scenarios1 = fig_diagram,
    #                         # value3_scenarios1 = value3_scenarios1(),
    #                         # value4_scenarios1 = value4_scenarios1(),
    #                         plot_ppv_scenarios1 = plot_ppv,
    #                         plot_npv_scenarios1 = plot_npv,
    #                         plot_cpc_scenarios1 = plot_cpc,
    #                         # fig_diagram_scenarios2 =  fig_diagram_scenarios2(),
    #                         # value3_scenarios2 = value3_scenarios2(),
    #                         # value4_scenarios2 = value4_scenarios2(),
    #                         # plot_ppv_scenarios2 = plot_ppv_scenarios2(),
    #                         # plot_npv_scenarios2 = plot_npv_scenarios2(),
    #                         # plot_cpc_scenarios2 = plot_cpc_scenarios2(),
    #                         # fig_diagram_scenarios3 = fig_diagram_scenarios3(),
    #                         # value3_scenarios3 = value3_scenarios3(),
    #                         # value4_scenarios3 = value4_scenarios3(),
    #                         # plot_ppv_scenarios3 = plot_ppv_scenarios3(),
    #                         # plot_npv_scenarios3 = plot_npv_scenarios3(),
    #                         # plot_cpc_scenarios3 = plot_cpc_scenarios3()
    #
    #                       ),
    #                       envir=new.env(parent = globalenv())
    #     )
    #   }
    # )

  })
}

