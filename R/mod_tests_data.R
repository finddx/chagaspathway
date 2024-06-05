#' tests_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tests_data_ui <- function(id, i18n){
  ns <- NS(id)
  tagList(
    radioButtons(ns("test_type"), label=strong(i18n$t("Test type")), choiceNames=c(i18n$t("RDT"), i18n$t("Laboratory-based")), choiceValues=c("RDT", "Serological test"), inline=TRUE, selected="RDT", width="100%"
    ),
    textInput(ns("label"), label=strong(i18n$t("Label")), width="100%"),
    radioButtons(ns("facility_type"), label=strong(i18n$t("Facility type")), choiceNames=c(i18n$t("High complexity"), i18n$t("Low complexity")), choiceValues=c("High complexity", "Low complexity"), inline=TRUE, selected="Low complexity", width="100%"),
    # radioButtons(ns("sample_type"), label=HTML("<b> Sample type </b>"), choices=c("Capillary", "Whole blood (NB: if low complexity, can only be capillary)"),  inline=TRUE, selected=NULL, width="100%"),
    numericInput(ns("sens"), label=strong(i18n$t("Sensitivity (0-100%)")), min=0,  max=100, value=85, width="100%"),
    numericInput(ns("spec"), label=strong(i18n$t("Specificity (0-100%)")), min=0, max=100, value=95, width="100%"),
    numericInput(ns("cost_test"), label=strong(i18n$t("Cost per test (USD)")), value=7.5, width="100%")
  )
}

#' tests_data Server Functions
#'
#' @noRd
mod_tests_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # observe({
    #   updateRadioButtons(session, "test_type", label=i18n_r()$t("Test type"), choiceNames=i18n_r()$t(c("RDT", "Laboratory-based")), choiceValues=c("RDT", "Serological test"))
    #   updateRadioButtons(session, "facility_type", label=i18n_r()$t("Facility type"), choiceNames=i18n_r()$t(c("High complexity", "Low complexity")), choiceValues=c("High complexity", "Low complexity"))
    # })

    return(
      list(
        test_type = reactive({ input$test_type }),
        label = reactive({ input$label }),
        facility_type = reactive({ input$facility_type }),
        # sample_type = reactive({ input$sample_type }),
        sens = reactive({ input$sens / 100 }),
        spec = reactive({ input$spec / 100 }),
        cost_test = reactive({ input$cost_test})
      )
    )

  })
}

