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
      column(3, selectInput(ns("population"), label=HTML("<b> Population </b>"), choices=c("General", "Women of childbearing age", "Children"), multiple=FALSE, selected=NULL, width="100%")),
      column(3, sliderInput(ns("lftu"), label=HTML("<b> LFTU (0-100%) </b> </br> Expected LTFU following patient or sample referral from low complexity to high complexity facility."), min=0, max=100, value=1, width="100%")),
      column(3, sliderInput(ns("link_treatment_l"), label=HTML("<b> Linkage to treatment (0-100%) </b> </br> Expected linkage to treatment if final diagnosis is made after testing at low complexity facility."), min=0, max=100, value=1, width="100%")),
      column(3, sliderInput(ns("link_treatment_h"), label=HTML("<b> Linkage to treatment (0-100%) </b> </br> Expected linkage to treatment if final diagnosis is made after testing at high complexity facility"), min=0, max=100, value=1, width="100%")),
      #Slider instead numericinput?
      p("This model assumes treatment is Y% effective at curing chronic chagas disease. This model also assumes that X% of patients with untreated chronic chagas develop a long-term complication. These values can be changed in [Advanced settings]")
    )

  )
}

#' pathways_data Server Functions
#'
#' @noRd
mod_pathways_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

