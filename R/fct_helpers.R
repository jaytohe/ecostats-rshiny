#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom vroom vroom
#' @importFrom attempt attempt
#' @importFrom tools file_ext
read_csv_vroom <- function(datapath, ...) {

  if(tolower(tools::file_ext(datapath)) != "csv") {
    stop("Invalid file. Please upload a .csv file!")
  }
  # Save arguments to list
  args <- list(datapath, ...)
  # If only datapath argument
  if (length(args) == 1) {
    # Append default arguments
    args <- list(datapath, delim = ",", col_names = TRUE, na = c("NA", "NULL", ""))
  }
  # Attempt to call vroom with arguments.
  return(attempt(do.call(vroom, args)))

}

show_alert <- function(msg, title) {
  golem::invoke_js("erroralert", list(title=title, msg=msg))
}
