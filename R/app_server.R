#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom ggplot2 ggplot geom_point
#' @importFrom shinyjs useShinyjs show hide hidden disable runjs
#' @importFrom shinyBS bsCollapse bsCollapsePanel
#' @importFrom patientpathways make_params run_pathway make_plots make_pathway_diagram format_app_params_react make_table_params make_table_results make_prev_df
#' @importFrom DiagrammeR render_graph grVizOutput renderGrViz
#' @importFrom gt render_gt gt_output
#' @importFrom finddxtemplate html_document_find
#' @importFrom plotly ggplotly renderPlotly plotlyOutput layout
#' @importFrom shinybusy show_modal_spinner remove_modal_progress update_modal_progress show_modal_progress_line
#' @importFrom magrittr "%>%"
#'
#' @noRd
app_server <- function(input, output, session) {

  # i18n <- golem::get_golem_options(which = "translator")
  # i18n$set_translation_language("en")
  # keep track of language object as a reactive
  # i18n_r <- reactive({
  #   i18n
  # })
  # change language
  # observeEvent(input[["selected_language"]], {
  #   shiny.i18n::update_lang(session, input[["selected_language"]])
  #   i18n_r()$set_translation_language(input[["selected_language"]])
  # })
  #Disable add scenario button
  # observe({
  #   if (length(displayed_scenarios()) >= 3) {
  #     shinyjs::disable("add_scenario")
  #   }
  # })

  #Track number of Scenarios
  displayed_scenarios <- reactiveVal()
  #Count number of scenarios (up tp 3)
  observeEvent(input$add_num_scenarios, {
    num_scenarios <- input$out_num_scenarios
    if (num_scenarios >= 1 & num_scenarios <= 3) {
      displayed_scenarios(seq_len(num_scenarios))
    } else {
      showModal(modalDialog(
        title = "Invalid number of scenarios",
        "Please enter a number between 1 and 3"
      ))
    }
  })

  #UI modules#

  color_scenarios <- c("#498FA9","#FF8F28","#491E5D")
  #Generate UI elements for a scenario
  generate_scenario_ui <- function(scenario_id, color_scenarios) {
    if (scenario_id %in% displayed_scenarios()) {
      scenario_number <- as.numeric(gsub("\\D", "", scenario_id))
      bg_color <- color_scenarios[scenario_number]
      tagList(
               card(
                 card_header(h4(strong(paste("Scenario", scenario_number))), style=paste0("background-color: " , bg_color, "; color: #ffffff;")),
                 card_body(
                   mod_scenarios_data_ui(paste0("scenarios_data_", scenario_number))
                 )
               )
      )
    }
  }
  #Render scenarios
  output$scenarios_ui <- renderUI({
    scenario_list <- lapply(displayed_scenarios(), generate_scenario_ui, color_scenarios=color_scenarios)
    fluidRow(do.call(tagList, scenario_list))
  })

  #Generate UI elements for a result
  generate_result_ui <- function(result_id, color_scenarios) {
    if(!is.null(results_data())){
      if (result_id %in% displayed_scenarios()) {
        result_number <- as.numeric(gsub("\\D", "", result_id))
        bg_color <- color_scenarios[result_number]
        tagList(
          column(width=ifelse(length(displayed_scenarios())==1,12,ifelse(length(displayed_scenarios())==2,6,4)),
                 # id=paste0("results_data_", gsub("\\D", "", result_id)),
                 card(
                   card_header(h4(strong(paste("Results scenario", result_number))), style=paste0("background-color: ", bg_color, "; color: #ffffff;")),
                   card_body(
                     mod_results_data_ui(paste0("results_data_", result_number))
                   )
                 )
          )
        )
      }
    }
  }
  #Render results
  output$results_ui <- renderUI({
    results_list <- lapply(displayed_scenarios(), generate_result_ui, color_scenarios=color_scenarios)
    fluidRow(do.call(tagList, results_list))
    # fluidRow(do.call(layout_column_wrap, results_list))
  })


  output$results_general_ui <- renderUI({
    if(!is.null(results_data())){
      fluidRow(
        card(
          card_body(
            mod_results_ui("results_general")
          )
        )
      )
    }
  })

  #SERVER modules#

  pathways <- mod_pathways_data_server("pathways_data")
  advance <- mod_advance_data_server("advance_data")

  #Generate server Scenario for each displayed scenario
  observe({
    lapply(displayed_scenarios(), function(scenario_id) {
      if (scenario_id %in% displayed_scenarios()) {
        scenario_number <- as.numeric(gsub("\\D", "", scenario_id))

        assign(
          paste0("scenario", scenario_number, "_vars"),
          mod_scenarios_data_server(
            id = paste0("scenarios_data_", scenario_number),
            scenarios_n = paste0("scenario", scenario_number)
          ),
          envir = .GlobalEnv
        )
      }
    })
  })

  #Generate server Result-scenario for each displayed scenario
  observe({
    # if(!is.null(results_data())){
    lapply(displayed_scenarios(), function(result_id) {
      if (result_id %in% displayed_scenarios()) {
        result_number <- as.numeric(gsub("\\D", "", result_id))
        assign(
          paste0("results_", result_number, "_vars"),
          mod_results_data_server(
            id = paste0("results_data_", result_number),
            # event_calculate = event_calculate,
            scenarios_n = paste0("scenario", result_number),
            results_list  = results_data #get(paste0("scenario", result_number, "_vars"))
          ),
          envir = .GlobalEnv
        )
      }
    })
    # }
  })

  observe({
    assign("results_all", mod_results_server("results_general", results_list=results_data), envir=.GlobalEnv)
  })
  # event_calculate <- eventReactive(input$calculate, {
  #   input$calculate
  # })


  #RESULTS#

  #Assign inputs to a list
  results_data <-
    eventReactive(input$calculate, {

      calculate_pathways(scenario1=scenario1_vars, scenario2=scenario2_vars, scenario3=scenario3_vars, advance=advance, pathways=pathways, displayed_scenarios=displayed_scenarios)
    })

  #Show modal results
  observeEvent(input$calculate, {
    # if (!is.null(results_data())) {
      showModal(modalDialog(
        title = "Functions executed correctly!",
        # "Functions executed correctly!",
        footer = tagList(
          actionButton("results_button", "Go to Results tab", class = "btn-secondary")
        )
      ))

    # }
  })

  # observe({
  #   tmp_params <- format_app_params_react(scenario_vars=results_data()$scenario1, global_vars=results_data()$pathways, advance_vars=results_data()$advance)
  # })


  #BUTTONS#

  observeEvent(input$results_button,{
    removeModal()
    updateNavbarPage(session, "menubar", selected = "Results")
  })

  observeEvent(input$advance_toggle,{
    shinyjs::toggle("div_advance")

  })

  output$calculate_button <- renderUI({
    if(length(displayed_scenarios())>=1){
      actionButton("calculate", "Calculate pathways", width="100%")
    }
  })

  output$report_button <- renderUI({
    if(!is.null(results_data())){
      column(width=4, offset=4,
             downloadButton("report", label="Generate report", style="text-align: center; width: 100%;", icon=NULL)
      )
    }
  })

  #Generate html report
  output$report <- downloadHandler(

    filename <-  "chagaspathway_report.html",
    content = function(file) {

      #Loading bar
      show_modal_progress_line(color="#491E5D", text="Generating report...")
      milestones <- c(10, 50, 70, 90)
      for (i in milestones) {
        update_modal_progress(i/100, text = paste("Generating report...", sprintf("%02d%%", i)))
        Sys.sleep(runif(1, 1, 3))
      }

      tempReport <- file.path(tempdir(), "chagaspathway_report.Rmd")
      file.copy("chagaspathway_report.Rmd", tempReport, overwrite=TRUE)
      #Pass outputs to the report
      rmarkdown::render(tempReport,
                        output_format = html_document_find(code_folding="none"),
                        output_file=file,
                        params=list(
                          num_scenarios=input$out_num_scenarios,
                          user_name = pathways$user_name(),
                          fig_diagram_scenarios1 = if(exists("results_1_vars")) results_1_vars$fig_diagram else NULL,
                          values_box_scenarios1 = if(exists("results_1_vars")) results_1_vars$values_box else NULL,
                          table_params_scenarios1 = if(exists("results_1_vars")) results_1_vars$table_params else NULL,
                          fig_diagram_scenarios2 =  if(exists("results_2_vars")) results_2_vars$fig_diagram else NULL,
                          values_box_scenarios2 = if(exists("results_2_vars")) results_2_vars$values_box else NULL,
                          table_params_scenarios2 = if(exists("results_2_vars")) results_2_vars$table_params else NULL,
                          fig_diagram_scenarios3 = if(exists("results_3_vars")) results_3_vars$fig_diagram else NULL,
                          values_box_scenarios3 = if(exists("results_3_vars")) results_3_vars$values_box else NULL,
                          table_params_scenarios3 = if(exists("results_3_vars")) results_3_vars$table_params else NULL,
                          table_res = if(exists("results_all")) results_all$table_res else NULL,
                          plot_ppv = if(exists("results_all")) results_all$plot_ppv else NULL,
                          plot_npv = if(exists("results_all")) results_all$plot_npv else NULL,
                          plot_cpc = if(exists("results_all")) results_all$plot_cpc else NULL
                        ),
                        envir=new.env(parent = globalenv())
      )
      remove_modal_progress()

    }

  )

}
