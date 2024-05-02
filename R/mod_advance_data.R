#' advance_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_advance_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      card(
        card_header(h4(strong("Population and natural history parameters"))),
        card_body(
          layout_column_wrap(
            style = "display: flex; align-items: flex-end;",
            numericInput(ns("prev_chagas"), label=HTML("<b> Prevalence of Chagas in care-seeking population (0-100%) </b>"), min=0, max=100, value=5, width="100%"),
            numericInput(ns("treat_effect"), label=HTML("<b> Treatment effectiveness (0-100%) </b>"), min=0, max=100, value=50, width="100%"),
            numericInput(ns("untreated_pats"), label=HTML("<b> Percent untreated patients developing long-term morbidities (0-100%) </b>"), min=0, max=100, value=20, width="100%"),
           numericInput(ns("avg_dalys"), label=HTML("<b> Average DALYs associated with untreated Chagas </b>"), value=0.05, width="100%")
          )
        )
      )
     ),
    fluidRow(
      card(
        card_header(h4(strong("Costing parameters"))),
          card_body(
            layout_column_wrap(
              style = "display: flex; align-items: flex-end;",
              numericInput(ns("cost_visit"), label=HTML("<b> Per-visit patient costs </b>"), value=0, width="100%"),
              numericInput(ns("cost_sys_low"), label=HTML("<b> Per-visit health system costs, low complexity </b>"), value=8.53, width="100%"),
              numericInput(ns("cost_sys_high"), label=HTML("<b> Per-visit health system costs, high complexity </b>"), value=12.35, width="100%")
            )
          )
     )
    )
  )
}

#' advance_data Server Functions
#'
#' @noRd
mod_advance_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      list(
        prev_chagas = reactive({ input$prev_chagas / 100 }),
        treat_effect = reactive({ input$treat_effect / 100 }),
        untreated_pats = reactive({ input$untreated_pats / 100 }),
        avg_dalys = reactive({ input$avg_dalys }),
        cost_visit = reactive({ input$cost_visit }),
        cost_sys_low = reactive({ input$cost_sys_low }),
        cost_sys_high = reactive({ input$cost_sys_high })
      )
    )

  })
}


