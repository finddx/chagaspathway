#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @importFrom bslib page_navbar bs_theme nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyBS bsCollapse bsCollapsePanel

#' @noRd
app_ui <- function(request) {

  useShinyjs()

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      # theme = bs_theme(bootswatch = "materia")


      id = "menubar",
      title = "Chagas Pathway",
      bg = "#491e5d",
      position = "fixed-top",
      nav_panel(
        "Pathways",icon = bs_icon("arrows-move"),
        br(),
        br(),
        h2("Introduction"),
        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed quis volutpat justo. Phasellus dignissim, metus vitae malesuada faucibus, odio lorem varius arcu, nec efficitur libero tortor vel mi. Maecenas euismod ligula eget erat malesuada, vel pharetra dui consequat. Integer auctor eleifend velit, vel condimentum nulla vestibulum et. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Quisque lacinia enim ac velit rhoncus, a pellentesque nulla mattis. Donec sit amet sapien quis lorem fermentum tempor. Duis posuere lectus vitae velit ultrices, eget consequat odio consequat. Vivamus ut ipsum ac neque fringilla iaculis. Sed non turpis arcu. Nulla ac consectetur risus, eget convallis velit. Sed a sapien id mauris mattis blandit. Sed lacinia ipsum sapien, eget egestas enim scelerisque sed. Sed tristique ultrices mauris, vitae sodales tortor tempus sed. Phasellus bibendum nisi at dui volutpat, a vehicula felis cursus. Sed vitae libero eu arcu rutrum elementum."),
        # fluidRow(
          # column(width = 12,
        fluidRow(
          column(4,align="left", offset=0, img(src="img/model1.png")),
          column(4,align="center", offset=0, img(src="img/model2.png")),
          column(4,align="right", offset=0, img(src="img/model3.png"))

        ),
        #   div(
        #     class="col-md-12",
        #     img(src='img/model1.png'),
        #     img(src='img/model2.png', style="margin-left:50px;"),
        #     img(src='img/model3.png', style="margin-left:50px;")
        #
        # ),
        br(),
        fluidRow(

            column(width=4,
                   #offset=4,
                   # div(
                   #     style = "display: flex; justify-content: center;",
                   # class="col-md-12",
                   mod_user_data_ui("user_data"),
                   h2(strong("Pathways")),
                   mod_pathways_data_ui("pathways_data"),
                   h2(strong("Advance settings")),
                   uiOutput("collapse_settings"),
                   # mod_advance_data_ui("advance_data")
                   # )
            )


        ),
        # div(
        #
        # ),
        h2(strong("Scenario specification")),
        fluidRow(

          column(width=4,
                 actionButton("add_scenario", "Add a new scenario", width="100%")
                 )
        ),
        fluidRow(
          column(width=4, id="scenarios_data_1",
                 h2("Scenario 1"),
                 mod_scenarios_data_ui("scenarios_data_1")#,
                 # actionButton("calculate1", "Calculate pathways")
          ),
          # column(width=4,id="scenarios_data_2",
          #        h2("Scenario 2"),
          #        mod_scenarios_data_ui("scenarios_data_2")
          # ),
          # column(width=4,id="scenarios_data_3",
          #        h2("Scenario 3"),
          #        mod_scenarios_data_ui("scenarios_data_3")
          # ),
          # textInput("current_scenario", "", value="1")

          )
      ),
      nav_panel(title="Results",
                icon=bs_icon("bar-chart-line"),
                mod_results_ui("results")
      ),
      nav_panel(title="Info",icon=bs_icon("info-circle-fill"),
                br(),
                br(),
                h1("Acknowledgements"),
                p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed quis volutpat justo. Phasellus dignissim, metus vitae malesuada faucibus, odio lorem varius arcu, nec efficitur libero tortor vel mi. Maecenas euismod ligula eget erat malesuada, vel pharetra dui consequat. Integer auctor eleifend velit, vel condimentum nulla vestibulum et. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Quisque lacinia enim ac velit rhoncus, a pellentesque nulla mattis. Donec sit amet sapien quis lorem fermentum tempor. Duis posuere lectus vitae velit ultrices, eget consequat odio consequat. Vivamus ut ipsum ac neque fringilla iaculis. Sed non turpis arcu. Nulla ac consectetur risus, eget convallis velit. Sed a sapien id mauris mattis blandit. Sed lacinia ipsum sapien, eget egestas enim scelerisque sed. Sed tristique ultrices mauris, vitae sodales tortor tempus sed. Phasellus bibendum nisi at dui volutpat, a vehicula felis cursus. Sed vitae libero eu arcu rutrum elementum."),
                h1("User manual"),
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
