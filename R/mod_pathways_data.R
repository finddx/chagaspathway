#' pathways_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pathways_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      card(
          card_header(h4(strong("User data"))),
          card_body(
            column(3, textInput(ns("user_name"), label=HTML("<b> User name </b>"),  width="100%"))
            )
      )
    ),
    fluidRow(
      card(
        card_header(h4(strong("General data"))),
        card_body(
          layout_column_wrap(
            style = "display: flex; align-items: flex-end;",
            radioButtons(ns("population"), label=HTML("<b> Population </b>"), choices=c("General", "Women of childbearing age", "Children"), inline=TRUE, selected=NULL, width="100%"),
            numericInput(ns("ltfu"), label=HTML("<b> LTFU (0-100%) </b> </br> Expected LTFU following patient or sample referral from low complexity to high complexity facility."), min=0, max=100, value=NULL, width="100%"),
            numericInput(ns("link_treatment_low"), label=HTML("<b> Linkage to treatment low complexity (0-100%) </b> </br> Expected linkage to treatment if final diagnosis is made after testing at low complexity facility."), min=0, max=100, value=NULL, width="100%"),
             numericInput(ns("link_treatment_high"), label=HTML("<b> Linkage to treatment high complexity (0-100%) </b> </br> Expected linkage to treatment if final diagnosis is made after testing at high complexity facility"), min=0, max=100, value=NULL, width="100%")
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
        population = reactive({ input$ population }),
        ltfu = reactive({ input$ltfu }),
        link_treatment_low = reactive({ input$link_treatment_low }),
        link_treatment_high = reactive({ input$link_treatment_high })
      )
    )

  })
}

