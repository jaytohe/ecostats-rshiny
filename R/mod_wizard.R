#' wizard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_wizard_ui <- function(id, pages, doneButton = NULL) {
  ns <- NS(id)
  stopifnot(is.list(pages))
  n <- length(pages)
  wrapped <- vector("list", n)
  for (i in seq_along(pages)) {
    # First page only has next; last page only prev + done
    lhs <- if (i > 1) prevPage(id, i)
    rhs <- if (i < n) nextPage(id, i) else doneButton
    wrapped[[i]] <- wrapPage(paste0("page_", i), pages[[i]], lhs, rhs)
  }

  # Create tabsetPanel
  # https://github.com/rstudio/shiny/issues/2927
  wrapped$id <- NS(id, "wizard")
  wrapped$type <- "hidden"
  v <- do.call("tabsetPanel", wrapped)
  #golem::print_dev(v)
  return(v)
}

#' wizard Server Functions
#'
#' @noRd
mod_wizard_server <- function(id, n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    changePage <- function(from, to) {
      observeEvent(input[[paste0("go_", from, "_", to)]], {
        updateTabsetPanel(session, "wizard", selected = paste0("page_", to))
      })
    }
    ids <- seq_len(n)
    lapply(ids[-1], function(i) changePage(i, i - 1))
    lapply(ids[-n], function(i) changePage(i, i + 1))
  })
}

## To be copied in the UI
# mod_wizard_ui("wizard_1")

## To be copied in the server
# mod_wizard_server("wizard_1")
