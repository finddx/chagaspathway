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
    radioButtons(ns("pathway_type"), label=HTML("<b> Pathway type </b>"), choices=c("Pathway 1", "Pathway 2", "Pathway 3"), inline=TRUE, selected="Pathway 1", width="100%"),
    uiOutput(ns("tests"))
  )
}

#' scenarios_data Server Functions
#'
#' @noRd
mod_scenarios_data_server <- function(id, scenarios_n, test1){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    test_n_out <- reactive({
      req(input$pathway_type)
      if (input$pathway_type == "Pathway 1") {
        test_n <- c(1,2,3)
      } else {
        test_n <- c(1,2,3,4,5)
      }
      return(test_n)
    })

    # displayed_scenarios <- reactiveVal(c("scenario1"))

    generate_test_ui <- function(test_id) {
      if (test_id %in% test_n_out()) {
        test_number <- as.numeric(gsub("\\D", "", test_id))
        tagList(
          column(width=ifelse(length(test_n_out())==3,4,2), id=paste0("tests_data_", gsub("\\D", "", test_id)),
                 h5(strong(paste("Test", test_number))),
                 mod_tests_data_ui(paste0(scenarios_n, "_tests_data_", test_number))
          )
        )
      }
    }

    output$tests <- renderUI({
      test_list <- lapply(test_n_out(), generate_test_ui)
      fluidRow(do.call(tagList, test_list))
    })



    # scenarios_list <- list()
    # #Generate mod_results_data_server for each displayed scenario
    # observe({
    #   lapply(test_n_out(), function(test_id) {
    #     if (test_id %in% test_n_out()) {
    #       test_number <- as.numeric(gsub("\\D", "", test_id))
    #
    #       scenario_object <- paste0("scenario", scenarios_n, "_tests_data_", test_number)
    #
    #       scenarios_list[[scenario_object]] <- mod_tests_data_server(
    #         id = paste0("scenario", scenarios_n, "_tests_data_", test_number)
    #       )
    #
    #
    #     }
    #   })
    # })


    # test1 <- mod_tests_data_server("scenario1_tests_data_1")
    # test2 <- mod_tests_data_server("scenario1_tests_data_2")
    # test3 <- mod_tests_data_server("scenario1_test_data_3")


    # scenarios_list <- list(test1=test1, test2=test2, test3=test3)#list(test_type="t")

    scenarios_list <- list(test1=test1)
    scenarios_list$pathway_type = reactive({ input$pathway_type })
    # scenarios_list$pathway_type = reactive({ input$pathway_type })
    # pathway_type = reactive({ input$pathway_type })
    return(
      # scenario_list$pathway_type <- scenarios_list
      scenarios_list
    )


  })
}

