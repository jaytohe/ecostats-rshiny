#' @importFrom lubridate hour minute second
#' @importFrom purrr map
#' @importFrom shinyWidgets sliderTextInput
#' @noRd
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

#' @noRd
checkboxColumn <- function(len) {
  inputs <- character(len)
  for(i in seq_len(len)) {
    inputs[i] <- paste0("<input id=", "'checkb_" , i, "' type='checkbox'>")
  }
  return(inputs)
}
