#' file_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shinyFiles shinyFilesButton
#'
mod_file_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, shinyFilesButton(
        id=ns("microphones"),
        label="Select microphones coordinates CSV",
#        accept=c("text/csv"),
        multiple = FALSE,
        title="Select CSV file")),
      column(6, shinyFilesButton(
        id=ns("recordings"),
        label="Select recordings CSV",
#        accept=c("text/csv"),
        multiple = FALSE,
        title="Select CSV file"))
    ),
    fluidRow(
      column(4, div(
        id="file_upload_div_mics",
        tableOutput(ns("tblMics")),
      )),
      column(2, div()),
      column(6, div(
        id="file_upload_div_recs",
        tableOutput(ns("tblRecs"))
      ))
    )
    )
}

#' file_upload Server Functions
#'
#' @noRd

#' @importFrom attempt attempt is_try_error try_catch
#' @importFrom shinyFiles shinyFileChoose getVolumes parseFilePaths
#' @importFrom fs path_home
mod_file_upload_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    volumes <- c(Home = path_home(),  getVolumes()())
    shinyFileChoose(input, "microphones", session = session, roots = volumes)
    shinyFileChoose(input, "recordings", session = session, roots = volumes)

    observeEvent(input$microphones, {
      req(!is.integer(input$microphones))
      f <- parseFilePaths(volumes, input$microphones)
      micData <- attempt(read_csv_vroom(f$datapath))
        if(is_try_error(micData)) {
          golem::invoke_js("erroralert", list(title="Microphones CSV read error!", msg=micData))
        } else {
          output$tblMics <- renderTable(head(micData))
          # Add micData to global reactiveValues
          r$micData <- micData
        }
    })

    observeEvent(input$recordings, {
      req(!is.integer(input$recordings))
      f <- parseFilePaths(volumes, input$recordings)
      recData <- attempt(read_csv_vroom(f$datapath))
      if(is_try_error(recData)) {
        golem::invoke_js("erroralert", list(title="Recordings CSV read error!", msg=recData))
      } else {
        output$tblRecs <- renderTable(head(recData))
        # Add recData to global reactiveValues
        r$recData <- recData
      }
    })
  })
}

## To be copied in the UI
# mod_file_upload_ui("file_upload_1")

## To be copied in the server
# mod_file_upload_server("file_upload_1")
