#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Global reactive values to communicate data between modules.
  r <- reactiveValues(
    micData = NULL,
    recData = NULL
  )
  mod_file_upload_server("file_upload_1", r)
  mod_date_select_server("date_select_1", r)
  mod_wizard_server("vocostep", 2)
}
