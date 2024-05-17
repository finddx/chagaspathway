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
    radioButtons(ns("pathway_type"), label=strong("Pathway type"), choiceNames=c("Parallel", "Serial (positive conf.)", "Serial (full conf.)"), choiceValues=c("parallel", "rule-out", "full"), inline=TRUE, selected="parallel", width="100%"),
    uiOutput(ns("tests"))
  )
}

#' scenarios_data Server Functions
#'
#' @noRd
mod_scenarios_data_server <- function(id, scenarios_n, i18n, i18n_r){#, list_tests
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      updateRadioButtons(session, "pathway_type", label=i18n_r()$t("Pathway type"), choiceNames=i18n_r()$t(c("Parallel", "Serial (positive conf.)", "Serial (full conf.)")), choiceValues=c("parallel", "rule-out", "full"))
    })

    #Define N of tests
    test_n_out <- reactive({
      req(input$pathway_type)
      if (input$pathway_type == "full") {
        test_n <- c(1,2,3,4,5)
      } else if (input$pathway_type == "parallel") {
        test_n <- c(1,2,3)
      } else {
        test_n <- c(1,2,3)
      }
      # else {
      #   test_n <- c(1,2,3)
      # }
      return(test_n)
    })

    #Generate tests UI
    generate_test_ui <- function(test_id) {
      if (test_id %in% test_n_out()) {
        test_number <- as.numeric(gsub("\\D", "", test_id))
        tagList(
          card(
            card_header(h5(strong(paste("Test", test_number)))),
            card_body(
              mod_tests_data_ui(ns(paste0(scenarios_n, "_tests_data_", test_number)), i18n=i18n)
              )
            )
        )
      }
    }
    output$tests <- renderUI({
      test_list <- lapply(test_n_out(), generate_test_ui)
      # fluidRow(do.call(tagList, test_list))
      fluidRow(do.call(layout_column_wrap, test_list))
    })

    create_scenario_list <- function(test_count, pathway_type) {
      test_list <- list()
      for (i in 1:test_count) {
        test_name <- paste0("test", i)
        module_name <- paste0(scenarios_n,  "_tests_data_", i)#, "_", test_count, "t"
        test_list[[test_name]] <- mod_tests_data_server(module_name, i18n_r=i18n_r)
      }
      test_list$pathway_type <- pathway_type
      return(test_list)
    }
    scenarios_list <- reactive({
      create_scenario_list(test_count=length(test_n_out()), pathway_type=input$pathway_type)
    })

    return(
      scenarios_list
    )

  })
}
