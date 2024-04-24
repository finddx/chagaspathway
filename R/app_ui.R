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
  # i18n <- Translator$new(translation_json_path= "translations/translation.json")

  tagList(
    # Leave this function for adding external resources
    useShinyjs(),
    # shiny.i18n::usei18n(i18n),
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
      title="Chagas Pathway",
      bg="#491e5d",
      position="fixed-top",
      nav_panel(
        "Pathways",icon = bs_icon("arrows-move"),
        h3(strong("Introduction")),
        # selectInput("selected_language","",choices=i18n$get_languages(),selected=i18n$get_key_translation()),
        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed quis volutpat justo. Phasellus dignissim, metus vitae malesuada faucibus, odio lorem varius arcu, nec efficitur libero tortor vel mi. Maecenas euismod ligula eget erat malesuada, vel pharetra dui consequat. Integer auctor eleifend velit, vel condimentum nulla vestibulum et. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Quisque lacinia enim ac velit rhoncus, a pellentesque nulla mattis. Donec sit amet sapien quis lorem fermentum tempor. Duis posuere lectus vitae velit ultrices, eget consequat odio consequat. Vivamus ut ipsum ac neque fringilla iaculis. Sed non turpis arcu. Nulla ac consectetur risus, eget convallis velit. Sed a sapien id mauris mattis blandit. Sed lacinia ipsum sapien, eget egestas enim scelerisque sed. Sed tristique ultrices mauris, vitae sodales tortor tempus sed. Phasellus bibendum nisi at dui volutpat, a vehicula felis cursus. Sed vitae libero eu arcu rutrum elementum."),
        fluidRow(
          column(4, align="left", offset=0, img(src="img/model1.png")),
          column(4, align="center", offset=0, img(src="img/model2.png")),
          column(4, align="right", offset=0, img(src="img/model3.png"))

        ),
        #   div(
        #     class="col-md-12",
        #     img(src='img/model1.png'),
        #     img(src='img/model2.png', style="margin-left:50px;"),
        #     img(src='img/model3.png', style="margin-left:50px;")
        #
        # ),
        br(),
        h3(strong("Pathways data")),
        fluidRow(
          column(width=12,
                 mod_pathways_data_ui("pathways_data")
                 )
        ),
        p("This model assumes treatment is Y% effective at curing chronic chagas disease. This model also assumes that X% of patients with untreated chronic chagas develop a long-term complication. These values can be changed in [Advanced settings]"),
        h3(strong("Scenario specification")),
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
        uiOutput("scenarios"),
        # fluidRow(
        #   column(width=4,
        #          actionButton("add_scenario", "Add new scenario", width="100%")
        #          )
        # ),
        fluidRow(
        column(width=4,
               uiOutput("calculate_button")
          # actionButton("calculate", "Calculate pathways", width="100%")
        )
        )
          # column(width=4, id="scenarios_data_1",
          #        uiOutput("scenario1")
          #        ),
          # column(width=4,id="scenarios_data_2",
          #        uiOutput("scenario2")
          #        ),
          # column(width=4,id="scenarios_data_3",
          #        uiOutput("scenario3")
          #        )
          # )
      ),
      nav_panel(title="Results", icon=bs_icon("bar-chart-line"),
        uiOutput("results"),
        uiOutput("report_button")
      ),
      nav_panel(title="Advance settings", icon=bs_icon("gear-fill"),
        h3(strong("Advance settings")),
        mod_advance_data_ui("advance_data")
      ),
      nav_panel(title="Info",icon=bs_icon("info-circle-fill"),
        h4(strong("Acknowledgements")),
        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed quis volutpat justo. Phasellus dignissim, metus vitae malesuada faucibus, odio lorem varius arcu, nec efficitur libero tortor vel mi. Maecenas euismod ligula eget erat malesuada, vel pharetra dui consequat. Integer auctor eleifend velit, vel condimentum nulla vestibulum et. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Quisque lacinia enim ac velit rhoncus, a pellentesque nulla mattis. Donec sit amet sapien quis lorem fermentum tempor. Duis posuere lectus vitae velit ultrices, eget consequat odio consequat. Vivamus ut ipsum ac neque fringilla iaculis. Sed non turpis arcu. Nulla ac consectetur risus, eget convallis velit. Sed a sapien id mauris mattis blandit. Sed lacinia ipsum sapien, eget egestas enim scelerisque sed. Sed tristique ultrices mauris, vitae sodales tortor tempus sed. Phasellus bibendum nisi at dui volutpat, a vehicula felis cursus. Sed vitae libero eu arcu rutrum elementum."),
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
