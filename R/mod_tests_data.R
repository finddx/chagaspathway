#' tests_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tests_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    # fluidRow(column(12,
    #                 align='center',tags$style(HTML(".distribute { display: flex; justify-content: space-between; }")),
    radioButtons(ns("test_type"), label=HTML("<b> Test type </b>"), choices=c("RDT", "Serological test"), inline=TRUE, selected=NULL, width="100%"
    #              )
    # )
    ),#selected = character(0)
    textInput(ns("label"), label=HTML("<b> Label </b>"), width="100%"),
    radioButtons(ns("facility_type"), label=HTML("<b> Facility type </b>"), choices=c("High complexity", "Low complexity"), inline=TRUE, selected=NULL, width="100%"),
    # radioButtons(ns("sample_type"), label=HTML("<b> Sample type </b>"), choices=c("Capillary", "Whole blood (NB: if low complexity, can only be capillary)"),  inline=TRUE, selected=NULL, width="100%"),
    numericInput(ns("sens"), label=HTML("<b> Sensitivity </b>"), min=0,  max=100, value=NULL, width="100%"),
    numericInput(ns("spec"), label=HTML("<b> Specificity </b>"), min=0, max=100, value=NULL, width="100%"),
    numericInput(ns("cost_test"), label=HTML("<b> Cost per test (USD) </b>"), value=NULL, width="100%")
  )
}

#' tests_data Server Functions
#'
#' @noRd
mod_tests_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      list(
        test_type = reactive({ input$test_type }),
        label = reactive({ input$label }),
        facility_type = reactive({ input$facility_type }),
        # sample_type = reactive({ input$sample_type }),
        sens = reactive({ input$sens }),
        spec = reactive({ input$spec }),
        cost_test = reactive({ input$cost_test})
      )
    )

  })
}

