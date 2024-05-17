#' pathways_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pathways_data_ui <- function(id, i18n){
  ns <- NS(id)
  tagList(
    fluidRow(
      card(
          card_header(h4(strong(i18n$t("User data")))),
          card_body(
            column(3, textInput(ns("user_name"), label=strong(i18n$t("User name")),  width="100%"))
            )
      )
    ),
    fluidRow(
      card(
        card_header(h4(strong(i18n$t("General data")))),
        card_body(
          layout_column_wrap(
            style = "display: flex; align-items: flex-end;",
            # radioButtons(ns("population"), label=HTML("<b> Population </b>"), choices=c("General", "Women of childbearing age", "Children"), inline=TRUE, selected="General", width="100%"),
            radioButtons(ns("fixed_cost"), label=strong(i18n$t("Include health system and patient visit costs?")), choices=c("Yes", "No"), inline=TRUE, selected="Yes", width="100%"),
            numericInput(ns("ltfu"), label=HTML("<b> Loss to follow-up (0-100%) </b> </br> Expected LTFU following patient or sample referral for testing/additional visit."), min=0, max=100, value=15, width="100%"),
            numericInput(ns("link_treatment_low"), label=HTML("<b> Linkage to treatment low complexity (0-100%) </b> </br> Expected linkage to treatment if final diagnosis is made at low complexity facility."), min=0, max=100, value=85, width="100%"),
             numericInput(ns("link_treatment_high"), label=HTML("<b> Linkage to treatment high complexity (0-100%) </b> </br> Expected linkage to treatment if final diagnosis is made at high complexity facility"), min=0, max=100, value=95, width="100%")
            )
        )
      )
    )
  )
}

#' pathways_data Server Functions
#'
#' @noRd
mod_pathways_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      list(
        user_name = reactive({ input$user_name }),
        # population = reactive({ input$ population }),
        fixed_cost = reactive({ input$fixed_cost }),
        ltfu = reactive({ input$ltfu / 100 }),
        link_treatment_low = reactive({ input$link_treatment_low / 100 }),
        link_treatment_high = reactive({ input$link_treatment_high / 100 })
      )
    )

  })
}

