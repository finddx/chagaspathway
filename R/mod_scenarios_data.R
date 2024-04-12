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
    radioButtons(ns("pathway_type"), label=HTML("<b> Pathway type </b>"), choices=c("full", "parallel", "rule-out"), inline=TRUE, selected="full", width="100%"),
    uiOutput(ns("tests"))
  )
}

#' scenarios_data Server Functions
#'
#' @noRd
mod_scenarios_data_server <- function(id, scenarios_n, list_tests){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #Define N of tests
    test_n_out <- reactive({
      req(input$pathway_type)
      if (input$pathway_type == "full") {
        test_n <- c(1,2,3,4,5)
      } else {
        test_n <- c(1,2,3)
      }
      return(test_n)
    })

    #Generate tests UI
    generate_test_ui <- function(test_id) {
      if (test_id %in% test_n_out()) {
        test_number <- as.numeric(gsub("\\D", "", test_id))
        tagList(
          column(width=ifelse(length(test_n_out())==3,4,2), id=paste0("tests_data_", gsub("\\D", "", test_id)),
                 h5(strong(paste("Test", test_number))),
                 mod_tests_data_ui(ns(paste0(scenarios_n, "_tests_data_", test_number)))
          )
        )
      }
    }
    output$tests <- renderUI({
      test_list <- lapply(test_n_out(), generate_test_ui)
      fluidRow(do.call(tagList, test_list))
    })
    #


    #

    # if (length(test_n_out())==3){
    #   tagList(
    #     column(width=4, id="tests_data_1",
    #            h5(strong("Test 1")),
    #            radioButtons(ns("test_type"), label=HTML("<b> Test type </b>"), choices=c("RDT", "Serological test"), inline=TRUE, selected=NULL, width="100%"),
    #            textInput(ns("label"), label=HTML("<b> Label </b>"), width="100%"),
    #            radioButtons(ns("facility_type"), label=HTML("<b> Facility type </b>"), choices=c("High complexity", "Low complexity"), inline=TRUE, selected=NULL, width="100%"),
    #            radioButtons(ns("sample_type"), label=HTML("<b> Sample type </b>"), choices=c("Capillary", "Whole blood (NB: if low complexity, can only be capillary)"),  inline=TRUE, selected=NULL, width="100%"),
    #            numericInput(ns("sensitivity"), label=HTML("<b> Sensitivity </b>"), min=0,  max=100, value=NULL, width="100%"),
    #            numericInput(ns("specificity"), label=HTML("<b> Specificity </b>"), min=0, max=100, value=NULL, width="100%"),
    #            numericInput(ns("cost_test"), label=HTML("<b> Cost per test (USD) </b>"), value=NULL, width="100%")
    #     ),
    #
    #   )
    # }



    # output$scenarios <- renderUI({
    #   for ( i in 1:length(test_n_out())) {
    #
    #     # tagList(
    #       column(width=ifelse(length(test_n_out())==3,4,2), id=paste0("tests_data_", i),
    #              h5(strong("Test 1")),
    #              mod_scenarios_data_ui(ns(paste0(scenarios_n, "_tests_data_",paste0(i))))
    #       )
    #     # )
    #
    # }
    # })

    # observe({
    #   num_scenarios <- length(test_n_out())
    #   shinyjs::hide(id = c("scenarios_data_1", "scenarios_data_2", "scenarios_data_3"))
    #   if (num_scenarios >= 1) {
    #     shinyjs::show(id = "scenarios_data_1")
    #   }
    #   if (num_scenarios >= 2) {
    #     shinyjs::show(id = "scenarios_data_2")
    #   }
    #   if (num_scenarios >= 3) {
    #     shinyjs::show(id = "scenarios_data_3")
    #   }
    # })



    create_scenario_list <- function(test_count, pathway_type) {
      test_list <- list()
      for (i in 1:test_count) {
        test_name <- paste0("test", i)
        module_name <- paste0(scenarios_n,  "_tests_data_", i)
        test_list[[test_name]] <- mod_tests_data_server(module_name)
      }
      test_list$pathway_type <- pathway_type
      return(test_list)
    }


    scenarios_list <- reactive({
        create_scenario_list(test_count = length(test_n_out()), pathway_type=input$pathway_type)
    })

#
#     scenarios_list <- create_scenario_list(test_count=5)
    # scenarios_list$pathway_type <- reactive({ input$pathway_type })


    return(
      scenarios_list
    )


  })
}

