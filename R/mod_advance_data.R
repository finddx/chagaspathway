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
      h4("NB"),
      column(3, textInput(ns("tbd1"), label=HTML("<b> TBD </b>"), value="", width="100%"))
     ),
    fluidRow(
      h4("Population and natural history parameters"),
      column(3, numericInput(ns("prev_chagas"), label=HTML("<b> Prevalence of Chagas in care-seeking population </b>"), min=0, max=1, value=NULL, width="100%")),
      column(3, numericInput(ns("treat_effect"), label=HTML("<b> Treatment effectiveness </b>"), min=0,  max=100, value=NULL, width="100%")),
      column(3, numericInput(ns("untreated_pats"), label=HTML("<b> Proportion of untreated patients developing long-term morbidities </b>"), min=0, max=100, value=NULL, width="100%")),
      column(3, textInput(ns("avg_dalys"), label=HTML("<b> Average DALYs associated with untreated, long-term comorbidities </b>"), value="", width="100%"))
     ),
    fluidRow(
      h4("Costing parameters"),
      column(3, textInput(ns("cost_visit"), label=HTML("<b> Per-patient cost of attending a medical visit </b>"), value="", width="100%"))
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
        prev_chagas = reactive({ input$prev_chagas }),
        treat_effect = reactive({ input$treat_effect }),
        untreated_pats = reactive({ input$untreated_pats }),
        avg_dalys = reactive({ input$avg_dalys }),
        cost_visit = reactive({ input$cost_visit })
      )
    )

  })
}


