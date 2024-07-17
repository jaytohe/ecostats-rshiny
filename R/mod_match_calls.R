#' match_calls UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT DTOutput
#' @importFrom lubridate ymd_hms
mod_match_calls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
#      column(12, verbatimTextOutput(ns("debug"))), # DEBUG
      column(12, align="center",
             div(id=ns("datetime_slider_container"),
                 style="visibility: hidden;",
                 sliderInput(
                   ns("datetime_slider"),
                   "Filter time: ",
                   min = ymd_hms("2024-07-09 08:54:36", tz="UTC"),
                   max = ymd_hms("2024-07-09 23:59:20", tz="UTC"),
                   value = c(ymd_hms("2024-07-09 08:54:36", tz="UTC"), ymd_hms("2024-07-09 23:59:20", tz="UTC")),
                   timeFormat = "%H:%M:%S",
                   timezone = "+0000" # we assume that all times are UTC
                 )
             )
      )
    ),
    fluidRow(
      #actionButton(ns("show_spectrograms2"), "Show spectrograms", class = "btn btn-info")
      column(4, align="center", tags$button("Show spectrograms" ,id="show_spectrograms", class="btn btn-info")),
      column(4, align="center", actionButton(ns("create_group2"), "Create new call group", class = "btn btn-primary")),
      column(4, align="center", div(class="btn-group",
                                    actionButton(ns("add_to_group"), "Add", class = "btn btn-secondary btn-spacing"),
                                    selectInput(ns("existing_group"), "Add calls to existing group:", choices = NULL)
      )),
    ),
    fluidRow(
      column(6,
             # First Column: DataTable Output
             div(
               id = "calls_container",
               tags$label(`for`="calls_container", "Unmatched calls"),
               DTOutput(ns("unmatched_calls"))
             )
      ),
      column(1, div()),
      # Third Column: DataTable Output
      column(5, DTOutput(ns("datatable2")))
    )
  )
}

#' match_calls Server Functions
#' @import dplyr
#'
#' @importFrom DT renderDT datatable JS
#' @importFrom lubridate format_ISO8601
#' @importFrom purrr map
#' @importFrom fs path path_dir
#' @noRd
mod_match_calls_server <- function(id, q){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r <- list(recParsedData = NULL, micData = NULL)


    absolute_path <- "/home/thinkpad/Documents/Dissertation2024/vocomatcher/data/poc_spectro/"
    rawRecData <- read.csv("/home/thinkpad/Documents/Dissertation2024/vocomatcher/data/poc_spectro/recordings.csv", tryLogical = F)
    rawMicData <- read.csv("/home/thinkpad/Documents/Dissertation2024/vocomatcher/data/poc_spectro/mic.csv")

    r$recParsedData <- parse_rec_data(rawRecData)

    # This is the main view of the unmatched calls
    frontendData <- reactive({
      req(r$recParsedData)
      r$recParsedData %>%
        mutate(
          toa = format_ISO8601(toa),
          timediff = "-",
          tick = checkboxColumn(nrow(r$recParsedData)),
          spectrogram = spectroImageColumn(nrow(r$recParsedData))
          ) %>%
        select(rec_id, tick, mic_id, toa, timediff, sex, spectrogram)
      # we need to null-check because renderDT isolates the reactive frontend data on init and the observer
      # waits for changes to update the table through the datatable proxy.
      # Therefore, we cannot req(input$) because the table won't render at all on init.
    })

    output$unmatched_calls <- renderDT({
      datatable(
        isolate(frontendData()),
        # Specify datatable options
        options = list(
          paging = TRUE,    ## paginate the output
          pageLength = 15,  ## number of rows to output for each page
          scrollX = TRUE,   ## enable scrolling on X axis
          scrollY = TRUE,   ## enable scrolling on Y axis
          autoWidth = TRUE, ## use smart column width handling
          scrollY =  20,
          scrollCollapse = TRUE,
          searching = TRUE,
          columnDefs = list(
            list(visible=FALSE, targets=1)
            ), # hide rec_id
          select = list(style = "single", items = "row") # configure single row selection.
        ),
        escape = FALSE, # allow html in data.
        selection = "none", # disable DT own's select implementation.
        extensions = c('Select', 'Buttons'), # Enable Select extension.
        callback = JS("onTableLoadFinish(table)") # call custom js function of ours on end.
      )
    },
    server = FALSE)  ## use client-side processing

    observeEvent(frontendData(), {

      # Calculate min and max date values
      date_limits <- r$recParsedData %>%
        select(toa) %>%
        summarise(min_date = min(toa), max_date=max(toa))

      # Update the slider with read-in date values
      updateSliderInput(
        session,
        "datetime_slider",
        min = date_limits$min_date,
        max = date_limits$max_date,
        value = c(date_limits$min_date, date_limits$max_date),
        timeFormat = "%H:%M:%S",
        timezone = "+0000" # we assume that all times are UTC
      )

      #Notify the client to display the slider input
      session$sendCustomMessage("showDateTimeSlider", list())
    })


    ## Hack: Listen on the server-side for changes on the datetime slider
    ## On any change, notify the client via custom function such that the table gets filtered.
    ## Ideally filtering should be handely completely on the client side.
    ## However 1) crosstalk's filter_slider did not work with DT (an empty input box was shown in the UI - no slider. Bug?)
    ## 2) Filtering the reactive frontendData() using dplyr and rendering it on the client side is not supported without
    ## server = TRUE. We need server=FALSE to utilize the Select extension such that the checkbox column is not selectable.
    observeEvent(input$datetime_slider, {
      session$sendCustomMessage(
        "filterTableByDate",
        # Send as JSON the user selected range to filter the dates on.
        list(from = format_ISO8601(input$datetime_slider[[1]]),
             to = format_ISO8601(input$datetime_slider[[2]]))
        )

    })

    observe({
      req(input$unmatched_calls_rows_current)
      req(frontendData())

      # array of base64 images and the frontend row index they correspond to
      encodedImages <- list()
      idx <- 1
      ## For each row that is visible on the current page of the table
      for (i in input$unmatched_calls_rows_current) {
          # Get row from backend using frontend row id.
          backendRecID <- frontendData() %>%
            slice(i) %>%
            pull(rec_id)

          # Filter backend datatable by record ID
          backendRow <- filter(r$recParsedData, rec_id == backendRecID)

          #Get relative path of spectrogram from backend
          spectroRelativePath <- backendRow$spectrogram

          # Get the absolute path to the image according to csv file path provided by shinyFiles
          spectroAbsPath <- path("/home/thinkpad/Documents/Dissertation2024/vocomatcher/data/poc_spectro/recordings.csv") %>% # path(csvMetaData()$datapath) %>%
            path_dir() %>%
            path(spectroRelativePath) %>%
            as.character

          ## Read the input image file and base64 encode it
          b64data <- encode_image(spectroAbsPath)

          #i minus one since array indexing in javascript begins at zero (like any other normal language!)
          encodedImages[[idx]] <- list(rowId = i-1, src=b64data)

          idx = idx + 1;
      }
      # Send JSON array to client of format [ {rowId: <num>, src: <base64string>} ]
      session$sendCustomMessage("updateTableSpectrogramImages", encodedImages)
    })



#    proxy = dataTableProxy('unmatched_calls')

#    observeEvent(on_row_select_data(), {
#      replaceData(proxy, on_row_select_data(), resetPaging = FALSE, clearSelection="none")
#    }, ignoreInit = TRUE)


  })
}

## To be copied in the UI
# mod_match_calls_ui("match_calls_1")

## To be copied in the server
# mod_match_calls_server("match_calls_1")
