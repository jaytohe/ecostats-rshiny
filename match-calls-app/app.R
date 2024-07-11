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
library(shinyWidgets)
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

generate_date_slider <- function(min_date, max_date) {


  # Generate all timestampts from min_date to max_date with step=second.
  time_labels <- seq(min_date, max_date, by="sec")
  # Convert time labels to HH:MM:SS strings.
  time_labels <- time_labels %>% purrr::map(\(t) paste0(hour(t), ':', minute(t), ':', second(t)))

  golem::print_dev(head(time_labels))
  golem::print_dev(tail(time_labels))
  return(
    sliderTextInput(
      "selected_scope_time_range",
      "Select Time Scope: ",
      time_labels,
      selected = c(time_labels[[1]], time_labels[[length(time_labels)]])
    )
  )

}

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

  rawRecData <- read.csv("recording.csv")
  rawMicData <- read.csv("mic.csv")

  r$recParsedData <- parse_rec_data(rawRecData)

  output$time_scope_container <- renderUI({
      req(r$recParsedData)
      date_limits <- r$recParsedData %>%
        select(toa) %>%
        summarise(min_date = min(toa), max_date=max(toa))

      golem::print_dev(date_limits)

      return(generate_date_slider(date_limits$min_date, date_limits$max_date))

    })


  frontendData <- r$recParsedData %>%
    mutate(
      timediff = "-",
      toa = paste0(hour(toa), ':', minute(toa), ':', second(toa)),
      # If sex is read in as Boolean, show it as "T"
      sex = ifelse(is.na(sex), "-", ifelse(is.logical(sex), ifelse(sex, "M", "F"), sex))
    ) %>%
    select(rec_id, mic_id, toa, timediff, sex, spectrogram)

  frontend_data_reactive <- reactive({
    input$unmatched_calls_rows_selected # take a reactive dependency on row selected

    if (is.null(input$unmatched_calls_rows_selected)) {
      frontendData %>%
        mutate(timediff = "-")
    }
    else {
      # Get selected row from back-end table.
      selected_row <- frontendData %>%
        slice(input$unmatched_calls_rows_selected) %>%
        pull(rec_id) %>%
        slice(r$recParsedData, .)

      # Populate timediff col with time differences between selected row and other columns.
      r$recParsedData %>%
        mutate(timediff = interval(selected_row$toa, toa) %>% as.period %>% as.character()) %>%
        pull(timediff) %>%
        mutate(frontendData, timediff = .)
    }

  })
  output$unmatched_calls <- renderDT(isolate(frontend_data_reactive()),
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

  observe({
    replaceData(proxy, frontend_data_reactive(), resetPaging = FALSE, clearSelection="none")
  })

  # Observe changes in the selected DT row.
  observeEvent(input$unmatched_calls_rows_selected, {

  })
}


# Run the application
shinyApp(ui = ui, server = server)
