#'
#' @description
#' This function reads all the
#'
#' @importFrom dplyr select mutate bind_rows
#' @importFrom purrr walk
#' @noRd
create_capture_history <- function(call_groups) {
  capt_hist <- data.frame()
  ## <Session>, <ID>, <Occasion>, <Detector>, <bearing>, <toa>
  ## where ID is the group_id, Detector is the mic_id

  walk(call_groups, function(call_group) {
    tmp_df <- call_group$backend_rows %>%
      # Occasion is hard-coded since ascr does not support multi-occasion data.
      # Session will eventually be supplied by user input; Hard-coded for now.
      mutate(Session = "demo", ID = call_group$group_id, Occasion = as.integer(1), Detector = mic_id, toa = as.numeric(toa)) %>%
      select(Session, ID, Occasion, Detector, bearing, toa) # abide to secr/ascr naming convention

    golem::print_dev(tmp_df)
    # append rows to main dataframe
    capt_hist <<- bind_rows(capt_hist, tmp_df)
  })
  return(capt_hist)
}
