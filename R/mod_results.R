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
mod_results_server <- function(id, scenario1_vars, scenario2_vars, scenario3_vars, advance_settins_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # df_scatter <- data.frame(sensitivity = input$x_input, specificity = input$y_input)

    # observe({
    #   plot2_obj <- reactive({
    #     ggplot(mapping = aes(x = 10, y = 20)) +
    #       geom_point()
    #   })
    # })
    #
    # # observeEvent(input$calculate1, {
    #   output$scatterplot <- renderPlot({
    #     plot2_obj()
    #   })
    # })



    observeEvent(scenario1_vars$calculate(), {
      # print("Button clicked")
      # req(input$calculate)

      output$scatterplot <- renderPlot({
        plot_scatter <- ggplot(mapping = aes(x=scenario1_vars$sensitivity(), y=scenario1_vars$specificity())) +
                 geom_point() +
          labs(x = "Sensitivity", y = "Specificity", title = "Scatter Plot")

        return(plot_scatter)
      })

      output$scatterplot2 <- renderPlot({
        plot_scatter <- ggplot(mapping = aes(x=scenario1_vars$sensitivity(), y=scenario1_vars$specificity())) +
          geom_point() +
          labs(x = "Sensitivity", y = "Specificity", title = "Scatter Plot")

        return(plot_scatter)
      })

      output$user_output <- renderText({ paste0("<b>Result :</b>", scenario1_vars$sensitivity(), " ", scenario1_vars$specificity())})

    })



  })
}

