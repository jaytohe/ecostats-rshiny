#' match_calls UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_match_calls_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, align="center", uiOutput(ns("time_scope_container")))
    ),
    layout_columns(
      widths = c(4, 4, 4),

      # First Column: DataTable Output
      dataTableOutput("datatable1"),

      # Second Column: Buttons and Dropdown
      div(
        actionButton("create_group", "Create new call group", class = "btn btn-primary"),
        br(), br(),
        fluidRow(
        column(8, selectInput("existing_group", "Add calls to existing group:", choices = NULL)),
        column(4, actionButton("add_to_group", "Add", class = "btn btn-secondary", style="margin-top: 25px;")),
        ),
        br(), br(),
        actionButton("show_spectrograms", "Show spectrograms", class = "btn btn-info")
      ),

      # Third Column: DataTable Output
      dataTableOutput("datatable2")
    )
  )
}

#' match_calls Server Functions
#' @importFrom dplyr select summarise
#' @noRd
mod_match_calls_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$time_scope_container <- renderUI({
      req(r$recParsedData)

      date_limits <- r$recParsedData %>%
        select(toa) %>%
        summarise(min_date = min(toa), max_date=max(toa))

      golem::print_dev(date_limits)

      return(generate_date_slider(date_limits$min_date, date_limits$max_date))

    })

  })
}

## To be copied in the UI
# mod_match_calls_ui("match_calls_1")

## To be copied in the server
# mod_match_calls_server("match_calls_1")
