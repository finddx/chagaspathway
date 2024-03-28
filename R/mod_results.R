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
             plotOutput(ns("scatterplot2")),
             uiOutput(ns("user_output"))
      )
    )

  )
}

#' results Server Functions
#'
#' @noRd
#'
#'
mod_results_server <- function(id, event_calculate,  pathways, scenario1_vars, scenario2_vars, scenario3_vars, advance_settins_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # df_scatter <- data.frame(sensitivity = input$x_input, specificity = input$y_input)

    #Get the reactive values
    results_data <- eventReactive(event_calculate(), {

      sensitivity_scenario1 <- scenario1_vars$sensitivity()
      specificity_scenario1 <- scenario1_vars$specificity()

      list(sensitivity_scenario1 = sensitivity_scenario1, specificity_scenario1 = specificity_scenario1)
    })


    #Render the results based on the event reactive
    output$scatterplot <- renderPlot({
      req(results_data())

      plot_scatter <- ggplot(mapping = aes(x=results_data()$sensitivity_scenario1, y=results_data()$specificity_scenario1)) +
        geom_point() +
        labs(x = "Sensitivity", y = "Specificity", title = "Scatter Plot")

      return(plot_scatter)
    })

    output$scatterplot2 <- renderPlot({
      req(results_data())
      plot_scatter <- ggplot(mapping = aes(x=results_data()$sensitivity_scenario1, y=results_data()$specificity_scenario1)) +
        geom_point() +
        labs(x = "Sensitivity", y = "Specificity", title = "Scatter Plot")
      return(plot_scatter)
    })

    output$user_output <- renderText({
      paste0("<b>Result :</b>", results_data()$sensitivity_scenario1, " ", results_data()$specificity_scenario1)

      })
    # eventReactive(input$calculate, {

    #   output$scatterplot <- renderPlot({
    #
    #     plot_scatter <- ggplot(mapping = aes(x=scenario1_vars$sensitivity(), y=scenario1_vars$specificity())) +
    #              geom_point() +
    #       labs(x = "Sensitivity", y = "Specificity", title = "Scatter Plot")
    #     return(plot_scatter)
    # })

  })
}

