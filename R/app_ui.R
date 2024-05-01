#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @importFrom bslib page_navbar bs_theme nav_panel bs_add_rules bs_theme card card_body card_header value_box
#' @importFrom bsicons bs_icon
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyjs useShinyjs show hide hidden disable
#' @importFrom shinyBS bsCollapse bsCollapsePanel
#' @importFrom shiny.i18n usei18n

#' @noRd
app_ui <- function(request) {
  # i18n <- golem::get_golem_options(which = "translator")
  # i18n$set_translation_language("en")

  tagList(
    # Leave this function for adding external resources
    useShinyjs(),
    golem_add_external_resources(),
    tags$style(HTML("body { margin-top: 50px !important; }")),
    page_navbar(
      theme = bs_theme(
        preset = "shiny",
        bg = "white",
        fg = "#414d52",
        primary = "#81969F",
        secondary = "#491e5d",
        success = "#489FA9",
        info = "#354159",
        warning = "#FF8F28",
        danger = "#D44F4E",
      ),
      id="menubar",
      title="Chagas Diagnostic Algorithms",
      bg="#491e5d",
      position="fixed-top",
      nav_panel(
        "Pathways",icon = bs_icon("arrows-move"),
        h3(strong("Introduction")),
        # h3(strong(i18n$t("Introduction"))),
        # selectInput("selected_language",label="Select language", choices=i18n$get_languages()),
        p(HTML("This online applications will help you to estimate the effectiveness and cost of different diagnostic algorithms for Chagas disease. Further details are provided in the Information tab.<br>You can model one, two, or three algorithms at the same time. These algorithms must follow one of the general structures displayed below.")),
        fluidRow(
          column(12, align="center", offset=0, img(src="www/img/model-diag.png", width="100%"))#, height="400vh"
        ),
        br(),
        h3(strong("General information")),
        p("Please begin by entering information on the general context of Chagas diagnosis in your setting. Please note that other default parameters, such as the proportion of individuals who are treated and the prevalance of Chagas in your population, can be modified in the Advanced Settings tab."),
        fluidRow(
          column(width=12,
                 mod_pathways_data_ui("pathways_data")
                 )
        ),
        h3(strong("Scenario specification")),
        p("Please select how many scenarios (1-3) you would like to model. You will be asked to select which general pathway (parallel testing, serial testing with full confirmation, or serial testing with positive confirmation) you would like to model for each scenario, and to provide information on test performance and cost for each scenario."),
        fluidRow(
            card(
              card_body(
                layout_column_wrap(
                  style = "display: flex; align-items: flex-end;",
                  width = 1/3,
                numericInput("out_num_scenarios", label=HTML("<b> Select the number of scenarios (up to 3) </b>"), min=1, max=3, value=1, width="100%"),
                actionButton("add_num_scenarios", "Create scenarios", width="100%")
              )
            )
          )

        ),
        uiOutput("scenarios_ui"),
        fluidRow(
          column(width=4,
                 uiOutput("calculate_button")
                )
        )
      ),
      nav_panel(title="Results", icon=bs_icon("bar-chart-line"),
        uiOutput("results_ui"),
        uiOutput("results_general_ui"),
        uiOutput("report_button")
      ),
      nav_panel(title="Advanced settings", icon=bs_icon("gear-fill"),
        h3(strong("Advanced settings")),
        mod_advance_data_ui("advance_data")
      ),
      nav_panel(title="Info",icon=bs_icon("info-circle-fill"),
        h4(strong("Acknowledgements")),
        p("This application was built by the Impact Department and Data Science Unit at FIND, with support from DNDi."),
        h4(strong("User manual")),
        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed quis volutpat justo. Phasellus dignissim, metus vitae malesuada faucibus, odio lorem varius arcu, nec efficitur libero tortor vel mi. Maecenas euismod ligula eget erat malesuada, vel pharetra dui consequat. Integer auctor eleifend velit, vel condimentum nulla vestibulum et. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Quisque lacinia enim ac velit rhoncus, a pellentesque nulla mattis. Donec sit amet sapien quis lorem fermentum tempor. Duis posuere lectus vitae velit ultrices, eget consequat odio consequat. Vivamus ut ipsum ac neque fringilla iaculis. Sed non turpis arcu. Nulla ac consectetur risus, eget convallis velit. Sed a sapien id mauris mattis blandit. Sed lacinia ipsum sapien, eget egestas enim scelerisque sed. Sed tristique ultrices mauris, vitae sodales tortor tempus sed. Phasellus bibendum nisi at dui volutpat, a vehicula felis cursus. Sed vitae libero eu arcu rutrum elementum."))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @import bslib
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "chagaspathway"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
