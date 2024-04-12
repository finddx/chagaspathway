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
             plotOutput(ns("scatterplot")),
             plotOutput(ns("scatterplot2"))
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
mod_results_server <- function(id, event_calculate, results_list){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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

