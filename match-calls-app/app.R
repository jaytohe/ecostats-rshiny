#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(DT)
library(purrr)
library(lubridate)
library(dplyr)
library(magrittr)

ns <- \(x) x

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch="default"),
  tagList(
    fluidRow(
      column(12, align="center", uiOutput(ns("time_scope_container")))
    ),
    fluidRow(
      column(4, align="center", actionButton("show_spectrograms2", "Show spectrograms", class = "btn btn-info")),
      column(4, align="center", actionButton("create_group2", "Create new call group", class = "btn btn-primary")),
      column(4, align="center", div(class="btn-group",
        actionButton("add_to_group", "Add", class = "btn btn-secondary btn-spacing"),
        selectInput("existing_group", "Add calls to existing group:", choices = NULL)
      )),
    ),
    fluidRow(
      column(6,
        # First Column: DataTable Output
        div(
          id="calls_container",
          tags$label(`for`="calls_container", "Unmatched calls"),
          DTOutput("unmatched_calls")
        )
      ),
      column(1, div()),
      # Third Column: DataTable Output
      column(5, DTOutput("datatable2"))
    )
)
)

parse_rec_data <- function(recordings) {
  recordings %>%
    #which columns to select
    select(c(recording_ID, mic_ID, GPSDatetime2, measured_bearing, measured_gender, spectrogram)) %>%
    # rename columns to standard format
    rename(rec_id = recording_ID, mic_id = mic_ID, toa = GPSDatetime2, bearing = measured_bearing, sex = measured_gender) %>%
    # parse toa as date object
    mutate(toa = ymd_hms(toa)) %>%
    # order by toa ascending
    arrange(toa)
}

server <- function(input, output, session){

  r <- list(recParsedData = NULL, micData = NULL)

  rawRecData <- read.csv("recording.csv", tryLogical = F)
  rawMicData <- read.csv("mic.csv")

  r$recParsedData <- parse_rec_data(rawRecData)

  output$time_scope_container <- renderUI({
      req(r$recParsedData)
      date_limits <- r$recParsedData %>%
        select(toa) %>%
        summarise(min_date = min(toa), max_date=max(toa))

      print(date_limits)
      sliderInput(
        "selected_scope_time_range",
        "Select Time Scope: ",
        min = date_limits$min_date,
        max = date_limits$max_date,
        value = c(date_limits$min_date, date_limits$max_date),
        timeFormat = "%H:%M:%S",
        timezone = "+0000" # we assume that all times are UTC
      )

    })

  in_scoped_time_range <- reactive({
    if (is.null(input$selected_scope_time_range)) {
      return(NA)
    }
    #return(\(x) between(x,input$selected_scope_time_range[[1]], input$selected_scope_time_range[[2]]))
    #return(\(x) x %within% interval(input$selected_scope_time_range[[1]], input$selected_scope_time_range[[2]]))
    return(interval(input$selected_scope_time_range[[1]], input$selected_scope_time_range[[2]]))
  })

  frontendData <- reactive({
    out <- r$recParsedData %>%
    mutate(
      timediff = "-",
    ) %>%
    select(rec_id, mic_id, toa, timediff, sex, spectrogram)

    if(!is.na(in_scoped_time_range())) {
      out<- out %>%
        filter(toa %within% in_scoped_time_range())
    }
    return(out)
    })

  time_diffed_data <- reactive({
    if (
      # If no row is selected
      is.null(input$unmatched_calls_rows_selected)
      || length(input$unmatched_calls_rows_selected) == 0
      ){
      return(frontendData() %>%
        mutate(timediff = "-"))
    }
    else {
      # Get recording ID of selected row.
      selected_rec_id <- frontendData() %>%
        slice(input$unmatched_calls_rows_selected) %>%
        pull(rec_id)

      if (length(selected_rec_id) == 0) {
        # If the selected row does not exist in the frontend table
        # zero-out all time-differences
        # Probably a better way to do this.
        return(frontendData() %>%
                 mutate(timediff = "-"))
      }
      selected_row <- frontendData() %>%
        filter(rec_id == selected_rec_id)

      # Populate timediff col with time differences between selected row and other columns.
      frontendData() %>%
        mutate(timediff = interval(selected_row$toa, toa) %>% as.period %>% as.character)
    }
  })

  formatted_data <- reactive({
    if (!is.na(in_scoped_time_range())) {
      time_diffed_data() %>%
        mutate(toa = format(toa, "%H:%M:%S"))
    } else {
      time_diffed_data() %>%
        mutate(toa = format(toa, "%H:%M:%S"))
    }
  })
  output$unmatched_calls <- renderDT(isolate(formatted_data()),
    # Specify datatable options
    options = list(
      paging = TRUE,    ## paginate the output
      pageLength = 15,  ## number of rows to output for each page
      scrollX = TRUE,   ## enable scrolling on X axis
      scrollY = TRUE,   ## enable scrolling on Y axis
      autoWidth = TRUE, ## use smart column width handling
      scrollY =  20,
      scrollCollapse = TRUE,
      server = TRUE,   ## use client-side processing
      searching = FALSE,
      columnDefs = list(list(visible=FALSE, targets=1)) # hide rec_id
   ),
   selection = "single"
  )

  proxy = dataTableProxy('unmatched_calls')

  observeEvent(formatted_data(), {
    replaceData(proxy, formatted_data(), resetPaging = FALSE, clearSelection="none")
  }, ignoreInit = TRUE)

}


# Run the application
shinyApp(ui = ui, server = server)
