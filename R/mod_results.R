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
    plotOutput(ns("scatterplot")),
    selectInput("population2", label=HTML("<b> Population </b>"), choices=c("General", "Women of childbearing age", "Children"), multiple=FALSE, selected=NULL, width = "100%")

  )
}

#' results Server Functions
#'
#' @noRd
mod_results_server <- function(id, model1_vars){
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



    # observeEvent(input$calculate1, {
      # print(paste0(input$model_type, input$test_type))




    observeEvent(model1_vars$calculate(), {
      # print("Button clicked")
      output$scatterplot <- renderPlot({
        plot_scatter <- ggplot(mapping = aes(x=model1_vars$sensitivity(), y=model1_vars$specificity())) +
                 geom_point() +
          labs(x = "Sensitivity", y = "Specificity", title = "Scatter Plot")

        return(plot_scatter)
      }
      )
    })


      # plot2_obj <- reactive({
      #   p <- ggplot(mapping = aes(x=model1_vars$sensitivity(), y=model1_vars$specificity())) +
      #   # p <- ggplot(mapping = aes(x=10, y=20)) +
      #     geom_point()
      #   return(p)
      # })
      #
      # # observeEvent(input$calculate1, {
      #
      #   print("Button clicked")
      # output$scatterplot <- renderPlot({
      #   plot2_obj()
      # })


    # })
  })
}

## To be copied in the UI
# mod_results_ui("results_1")

## To be copied in the server
# mod_results_server("results_1")
