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

    fluidRow(
      column(width=8, offset=2,
             plotOutput(ns("plo1"))
      )
    ),
    fluidRow(
      column(width=4, offset=4,
              downloadButton(ns("report"), label="Generate report", style="text-align: center; width: 100%;", class="button-color", icon=NULL)
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
mod_results_server <- function(id, results_list){# event_calculate,
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # results_data <- reactive({
    #   results_list()
    # })


    results_data  <- results_list()


     # results_data[[scenarios_n]]$test1$test_type()

    # observe({
      # res_pathway <- results_data()$scenario1$pathway_type
      # res_pathway <- results_data$scenario1$pathway_type
      # res_prev <- 50
      # res_lftu <- results_data$pathways$lftu
      # res_test1 <- results_data$scenario1$test1
      # res_test2 <- results_data$scenario1$test2
      # res_test3 <- results_data$scenario1$test3
      # res_test4 <- results_data$scenario1$test4
      # res_test5 <- results_data$scenario1$test5
      # res_daly_avert_per_tx <- 12 #results_data$advance$avg_dalys
      # res_treat_effect <- treat_effect <- 15 #results_data$advance$treat_effect

      params <- format_app_params2(scenario_vars=results_data$scenario1, global_vars=results_data$pathways, advance_vars=results_data$advance)
      # print(results_data$scenario1$test1$test_type())

      # print(results_data$scenario1)
      # print(results_data$pathways)
      # print(results_data$advance)

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
    #   sensitivity_scenario2 <- scenario2_vars$sensitivity()
    #   specificity_scenario2 <- scenario2_vars$specificity()
    #   sensitivity_scenario3 <- scenario3_vars$sensitivity()
    #   specificity_scenario3 <- scenario3_vars$specificity()
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
    #
    # scatterplot2_plot <- reactive({
    #   req(df_scatter())
    #   ggplot(df_scatter(), aes(x=sensitivity, y=specificity)) +
    #     geom_point() +
    #     labs(x = "Sensitivity", y = "Specificity", title = "Scatter Plot")
    #
    # })
    #
    # output$scatterplot <- renderPlot({
    #   scatterplot_plot()
    #   })
    #
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
    #                         user_name = results_data()$user_name,
    #                         sensitivity_scenario1 = results_data()$sensitivity_scenario1,
    #                         specificity_scenario1 = results_data()$specificity_scenario1,
    #                         sensitivity_scenario2 = results_data()$sensitivity_scenario2,
    #                         specificity_scenario2 = results_data()$specificity_scenario2,
    #                         sensitivity_scenario3 = results_data()$sensitivity_scenario3,
    #                         specificity_scenario3 = results_data()$specificity_scenario3,
    #                         scatterplot_plot=scatterplot_plot(),
    #                         scatterplot2_plot=scatterplot2_plot()
    #                       ),
    #                       envir=new.env(parent = globalenv())
    #     )
    #   }
    # )

  })
}

