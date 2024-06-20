#' file_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_file_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      fileInput(
        inputId=ns("microphones"),
        label="Upload microphones coordinates",
        #accept=c("text/csv"),
        buttonLabel="Upload..."),
      fileInput(
        inputId=ns("recordings"),
        label="Upload recordings data",
        accept=c("text/csv"),
        buttonLabel="Upload...")
      )
    )
}

#' file_upload Server Functions
#'
#' @noRd

#' @importFrom attempt attempt is_try_error try_catch
mod_file_upload_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$microphones, {
      micData <- attempt(read_csv_vroom(input$microphones$datapath))
      if(is_try_error(micData)) {
        golem::invoke_js("erroralert", list(title="Microphones CSV read error!", msg=micData))
      }
    })

    observeEvent(input$recordings, {
      recData <- attempt(read_csv_vroom(input$recordings$datapath))
      if(is_try_error(recData)) {
        golem::invoke_js("erroralert", list(title="Recordings CSV read error!", msg=recData))
      }
    })
  })
}

## To be copied in the UI
# mod_file_upload_ui("file_upload_1")

## To be copied in the server
# mod_file_upload_server("file_upload_1")
