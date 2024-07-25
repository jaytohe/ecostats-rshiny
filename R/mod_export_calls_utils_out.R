#'
#' @description
#' This function reads all the
#'
#' @importFrom dplyr select mutate bind_rows
#' @importFrom purrr map list_rbind
#' @noRd
create_capture_history <- function(call_groups) {
  ## <Session>, <ID>, <Occasion>, <Detector>, <bearing>, <toa>
  ## where ID is the group_id, Detector is the mic_id
  map(call_groups, \(call_group) call_group$backend_rows %>%
      # Occasion is hard-coded since ascr does not support multi-occasion data.
      # Session will eventually be supplied by user input; Hard-coded for now.
      mutate(Session = "demo", ID = call_group$group_id, Occasion = as.integer(1), Detector = mic_id, toa = as.numeric(toa)) %>%
      select(Session, ID, Occasion, Detector, bearing, toa) # abide to secr/ascr naming convention
  ) %>%
    list_rbind()
}
