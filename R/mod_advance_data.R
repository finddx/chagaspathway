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
      h5("NB"),
      column(3, textInput(ns("tbd1"), label=HTML("<b> TBD </b>"), value="", width="100%"))
     ),
    fluidRow(
      h5("Population and natural history parameters"),
      column(3, sliderInput(ns("treat_effect"), label=HTML("<b> Treatment effectiveness </b>"), min=0,  max=100, value=40, width="100%")),
      column(3, sliderInput(ns("untreated_pats"), label=HTML("<b> Proportion of untreated patients developing long-term morbidities </b>"), min=0, max=100, value=40, width="100%")),
      column(3, textInput(ns("avg_dalys"), label=HTML("<b> Average DALYs associated with untreated, long-term comorbidities </b>"), value="", width="100%"))
     ),
    fluidRow(
      h5("Costing parameters"),
      column(3, textInput(ns("tbd2"), label=HTML("<b> TBD </b>"), value="", width="100%"))
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
        treat_effect = reactive({ input$treat_effect }),
        untreated_pats = reactive({ input$untreated_pats }),
        avg_dalys = reactive({ input$avg_dalys })
      )
    )

  })
}


