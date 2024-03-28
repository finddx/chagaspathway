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
    selectInput(ns("model_type"), label=HTML("<b> Model type </b>"), choices=c("Model 1", "Model 2", "Model 3"), multiple=FALSE, selected=NULL, width="100%"),
    selectInput(ns("test_type"), label=HTML("<b> Test type </b>"), choices=c("RDT", "Serological test"), multiple=FALSE, selected=NULL, width="100%"),
    textInput(ns("label"), label=HTML("<b> Label </b>"), width="100%"),
    selectInput(ns("facility_type"), label=HTML("<b> Facility type </b>"), choices=c("High complexity", "Low complexity"), multiple=FALSE, selected=NULL, width="100%"),
    selectInput(ns("sample_type"), label=HTML("<b> Sample type </b>"), choices=c("Capillary", "Whole blood (NB: if low complexity, can only be capillary)"), multiple=FALSE, selected=NULL, width="100%"),
    sliderInput(ns("sensitivity"), label=HTML("<b> Sensitivity </b>"), min=0,  max=100, value=40, width="100%"),
    sliderInput(ns("specificity"), label=HTML("<b> Specificity </b>"), min=0, max=100, value=40, width="100%"),
    textInput(ns("cost_test"), label=HTML("<b> Cost per test (USD) </b>"), value="", width="100%")
    # ,
    # actionButton(ns("calculate"), "Calculate pathways", width="100%")

  )
}

#' scenarios_data Server Functions
#'
#' @noRd
mod_scenarios_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      list(
        # calculate = reactive({ input$calculate }),
        sensitivity = reactive({ input$sensitivity }),
        specificity = reactive({ input$specificity })
      )
    )

  })
}

