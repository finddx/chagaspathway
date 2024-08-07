#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @importFrom bslib page_navbar bs_theme nav_panel bs_add_rules bs_theme card card_body card_header value_box
#' @importFrom bsicons bs_icon
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyjs useShinyjs show hide hidden disable toggle
#' @importFrom shinyBS bsCollapse bsCollapsePanel
#' @importFrom shiny.i18n usei18n update_lang

#' @noRd
app_ui <- function(request) {
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("English")

  tagList(
    # Leave this function for adding external resources
    useShinyjs(),
    shiny.i18n::usei18n(i18n),

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
        fluidRow(column(3, offset=9, selectInput("selected_language",label="Select language/Selecciona el idioma", choices=i18n$get_languages(),  selected=i18n$get_key_translation(), width="100%"))),
        h3(strong(i18n$t("Introduction"))),
        uiOutput("select_lang"),
        p(i18n$t("This online applications will help you to estimate the effectiveness and cost of different diagnostic algorithms for Chagas disease. Further details are provided in the Information tab. You can model one, two, or three algorithms at the same time. These algorithms must follow one of the general structures displayed below.")),
        fluidRow(
        layout_column_wrap(
          card(
            # card_header(h4(strong("PPV plot"))),
            full_screen=TRUE,
            card_body(
              img(src="www/img/parallel.png", width="100%"))
            ),
          card(
            # card_header(h4(strong("NPV plot"))),
            full_screen=TRUE,
            card_body(
              img(src="www/img/serial-pos.png", width="100%"))
            ),
          card(
            # card_header(h4(strong("OPC plot"))),
            full_screen=TRUE,
            card_body(
              img(src="www/img/serial-full.png", width="100%"))
            )
        )
        ),
        br(),
        h3(strong(i18n$t("General information"))),
        p(i18n$t("Please begin by entering information on the general context of Chagas diagnosis in your setting. Please note that other default parameters, such as the proportion of individuals who are treated and the prevalance of Chagas in your population, can be modified in the Advanced Settings section.")),
        fluidRow(
          column(width=12,
                 mod_pathways_data_ui("pathways_data", i18n=i18n)
                 )
        ),
        fluidRow(
          column(width=4,
                 actionButton("advance_toggle", label=div(style = "display: flex; align-items: center; justify-content: space-between;", tags$h3(tags$strong(i18n$t("Advance settings"))), span(icon("caret-down"), style = "margin-left: 5px;")), style = "text-decoration: none; border: none; background: none; padding: 0; margin: 0; cursor: pointer; outline: none; color: inherit;", onmouseover ="this.style.color = '#491E5D';", onclick = "this.style.color = '#491E5D';", onmouseout = "this.style.color = 'inherit';")
                 )
        ),
        hidden(div(
          id="div_advance",
          mod_advance_data_ui("advance_data", i18n=i18n)
          )
        ),
        h3(strong(i18n$t("Scenario specification"))),
        p(i18n$t("Please select how many scenarios (1-3) you would like to model. You will be asked to select which general pathway (parallel testing, serial testing with full confirmation, or serial testing with positive confirmation) you would like to model for each scenario, and to provide information on test performance and cost for each scenario.")),
        fluidRow(
            card(
              card_body(
                layout_column_wrap(
                  style = "display: flex; align-items: flex-end;",
                  width = 1/3,
                numericInput("out_num_scenarios", label=strong(i18n$t("Select the number of scenarios (up to 3)")), min=1, max=3, value=1, width="100%"),
                actionButton("add_num_scenarios", i18n$t("Create scenarios"), width="100%")
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
      nav_panel(value="results_tab",title=i18n$t("Results"), icon=bs_icon("bar-chart-line"),
        uiOutput("results_ui"),
        uiOutput("results_general_ui"),
        uiOutput("report_button")
      ),
      # nav_panel(title="Advanced settings", icon=bs_icon("gear-fill"),
      #   h3(strong("Advanced settings")),
      #   mod_advance_data_ui("advance_data"),
      #   fluidRow(
      #     column(width=4,
      #            uiOutput("recalculate_button")
      #     )
      #   )
      # ),
      nav_panel(title="Info",icon=bs_icon("info-circle-fill"),
                htmlOutput("user_manual")

      )
      # ,
      # nav_spacer(),
      # nav_item(
      #   align = "right",
      #     fluidRow(div(
      #              style = "display: flex; align-items: center;",
      #              tags$span("Select language/Selecciona el idioma", style = "margin-right: 10px;"),
      #              selectInput("selected_lge", NULL, choices=i18n$get_languages(),selected=i18n$get_key_translation())
      #            ))
      # )



        # h4(strong("Acknowledgements")),
        # p("This application was built by the Impact Department and Data Science Unit at FIND. We gratefully acknowledge the support and contribution of our many partners. The multicentric prospective study in Argentina is being conducted by our partners, CONICET, sponsored by the National Institute of Health, INP (National Institute of Parasitology), Fatala within ANLIS, with the support of FIND and DNDi."),
        # h4(strong("User manual")),
        # p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed quis volutpat justo. Phasellus dignissim, metus vitae malesuada faucibus, odio lorem varius arcu, nec efficitur libero tortor vel mi. Maecenas euismod ligula eget erat malesuada, vel pharetra dui consequat. Integer auctor eleifend velit, vel condimentum nulla vestibulum et. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Quisque lacinia enim ac velit rhoncus, a pellentesque nulla mattis. Donec sit amet sapien quis lorem fermentum tempor. Duis posuere lectus vitae velit ultrices, eget consequat odio consequat. Vivamus ut ipsum ac neque fringilla iaculis. Sed non turpis arcu. Nulla ac consectetur risus, eget convallis velit. Sed a sapien id mauris mattis blandit. Sed lacinia ipsum sapien, eget egestas enim scelerisque sed. Sed tristique ultrices mauris, vitae sodales tortor tempus sed. Phasellus bibendum nisi at dui volutpat, a vehicula felis cursus. Sed vitae libero eu arcu rutrum elementum."))
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
