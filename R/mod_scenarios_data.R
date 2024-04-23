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
mod_scenarios_data_server <- function(id, scenarios_n){#, list_tests
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #Define N of tests
    test_n_out <- reactive({
      req(input$pathway_type)
      if (input$pathway_type == "full") {
        test_n <- c(1,2,3,4,5)
      } else if (input$pathway_type == "parallel") {
        test_n <- c(1,2,3)
      } else {
        test_n <- c(1,2)
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
          # column(width=ifelse(length(test_n_out())==3,4,ifelse(length(test_n_out())==2,6,2)), id=paste0("tests_data_", gsub("\\D", "", test_id)),
                 card(
                   card_header(h5(strong(paste("Test", test_number)))),
                   card_body(
                     mod_tests_data_ui(ns(paste0(scenarios_n, "_tests_data_", test_number)))
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

    # output$tests <- renderUI({
    #   tagList(
    #     div(id="tests_5_div",
    #         fluidRow(
    #           splitLayout(
    #             cell_width="20%",
    #                card(
    #                  card_header(h5(strong("Test 1"))),
    #                  card_body(
    #                    mod_tests_data_ui(ns(paste0(scenarios_n,"_tests_data_1_5t")))
    #                )),
    #             card(
    #               card_header(h5(strong("Test 2"))),
    #               card_body(
    #                 mod_tests_data_ui(ns(paste0(scenarios_n,"_tests_data_2_5t")))
    #               )),
    #             card(
    #               card_header(h5(strong("Test 3"))),
    #               card_body(
    #                 mod_tests_data_ui(ns(paste0(scenarios_n,"_tests_data_3_5t")))
    #               )),
    #             card(
    #               card_header(h5(strong("Test 4"))),
    #               card_body(
    #                 mod_tests_data_ui(ns(paste0(scenarios_n,"_tests_data_4_5t")))
    #               )),
    #             card(
    #               card_header(h5(strong("Test 5"))),
    #               card_body(
    #                 mod_tests_data_ui(ns(paste0(scenarios_n,"_tests_data_5_5t")))
    #               ))
    #     ))),
    #     shinyjs::hidden(
    #       div(id="tests_3_div",
    #         fluidRow(
    #           splitLayout(
    #             cell_width="33%",
    #             card(
    #               card_header(h5(strong("Test 1"))),
    #               card_body(
    #                 mod_tests_data_ui(ns(paste0(scenarios_n,"_tests_data_1_3t")))
    #               )),
    #             card(
    #               card_header(h5(strong("Test 2"))),
    #               card_body(
    #                 mod_tests_data_ui(ns(paste0(scenarios_n,"_tests_data_2_3t")))
    #               )),
    #             card(
    #               card_header(h5(strong("Test 3"))),
    #               card_body(
    #                 mod_tests_data_ui(ns(paste0(scenarios_n,"_tests_data_3_3t")))
    #               ))
    #         )
    #         )
    #       )
    # ))
    # })
    # observe({
    #       num_tests <- length(test_n_out())
    #       if (num_tests==5) {
    #         shinyjs::show(id="tests_5_div", asis=TRUE)
    #         shinyjs::hide(id="tests_3_div", asis=TRUE)
    #       } else if(num_tests==3){
    #         shinyjs::show(id ="tests_3_div", asis=TRUE)
    #         shinyjs::hide(id="tests_5_div", asis=TRUE)
    #       }
    # })




    create_scenario_list <- function(test_count, pathway_type) {
      test_list <- list()
      for (i in 1:test_count) {
        test_name <- paste0("test", i)
        module_name <- paste0(scenarios_n,  "_tests_data_", i)#, "_", test_count, "t"
        test_list[[test_name]] <- mod_tests_data_server(module_name)
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

    # pathway_list <- list()
    # pathway_list$pathway_type <- reactive({ input$pathway_type })



  })
}
