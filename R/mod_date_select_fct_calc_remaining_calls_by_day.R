#' Remaining/Unmatched calls calculation Function
#'
#' @description
#' This function returns a tibble of the number of unmatched calls per day.
#'
#' @details
#' This function uses the datetime column of the passed-in recordings tibble to group calls by day
#' and count how many calls exist per day. This allows to quickly pick a day of calls they
#' would like to work on.
#'
#' @param recordings A tibble containing the recorded calls read from a csv.
#' @returns number of calls aggregated by day
#' @noRd
#'
#' @importFrom dplyr mutate group_by summarise n
#' @importFrom lubridate stamp
#'
calc_remaining_calls_by_day <- function(recordings) {
  recordings %>%
    mutate(Date = stamp("31/12/1990")(toa)) %>%
    group_by(Date) %>%
    summarise(Calls = n())
}
