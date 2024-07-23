#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = bs_theme(version = 5),
      h1("vocomatcher"),
      mod_wizard_ui("vocostep",
                    list(
                      mod_match_calls_ui("match_calls_1"),
                      mod_export_calls_ui("export_calls_1"),
                      mod_file_upload_ui("file_upload_1"),
                      mod_date_select_ui("date_select_1")
                      #mod_match_calls_ui("match_calls_1")
                     ),
                    doneButton=actionButton("done", "Submit"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "vocomatcher"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
