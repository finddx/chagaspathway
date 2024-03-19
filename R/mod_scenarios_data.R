#' scenarios_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_scenarios_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    # h3(paste("Scenario", id)),
      # radioGroupButtons("model", "Model Pathways",
      #                   choiceNames = c('<i class="model1"></i>',
      #                                   '<i class="model2"></i>'),
      #                   choiceValues = c("Model 1", "Model 2")),




    selectInput(ns("model_type"), label=HTML("<b> Model type </b>"), choices=c("Model 1", "Model 2", "Model 3"), multiple=FALSE, selected=NULL),
    selectInput(ns("test_type"), label=HTML("<b> Test type </b>"), choices=c("RDT", "Serological test"), multiple=FALSE, selected=NULL),
    textInput(ns("label"), label=HTML("<b> label </b>"), value="Please put a label for the output"),
    selectInput(ns("facility_type"), label=HTML("<b> Facility type </b>"), choices=c("High complexity", "Low complexity"), multiple=FALSE, selected=NULL),
    selectInput(ns("sample_type"), label=HTML("<b> Sample type </b>"), choices=c("Capillary", "Whole blood (NB: if low complexity, can only be capillary)"), multiple=FALSE, selected=NULL),
    sliderInput(ns("sensitivity"), label=HTML("<b> Sensitivity </b>"), min=0,  max=100, value=40),
    sliderInput(ns("specificity"), label=HTML("<b> Specificity </b>"), min=0, max=100, value=40),
    textInput(ns("cost_test"), label=HTML("<b> Cost per test (USD) </b>"), value=""),
    actionButton(ns("calculate"), "Calculate pathways")

  )
}

#' scenarios_data Server Functions
#'
#' @noRd
mod_scenarios_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # observeEvent(input$calculate, {
    #     print(paste0(input$model_type, input$test_type))
    #
    # })

    return(
      list(
        calculate = reactive({ input$calculate }),
        sensitivity = reactive({ input$sensitivity }),
        specificity = reactive({ input$specificity })
      )
    )

  })
}

