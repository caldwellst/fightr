#' Get data frame with all session and attendance data
#'
#' `get_full_df()` pulls together the separate `sessions`, `attendance`, and
#' `attendees` data frames into a single data frame with 1 row for each attendee
#' attending a session. Useful to get a single combined database of all attendance
#' data to later parse for dashboard visualizations or other analysis.
#'
#' @param pd Parsed database list coming out of [parse_single_database()] or
#'     [parse_multiple_databases()] with `sessions`, `attendance`,  and `attendees`
#'     data frames.
#'
#' @return A data frame.
#'
#' @export
get_full_df <- function(pd) {
  assert_pd(pd)

  # Join all data to attendance data frames
  # Gets 1 data frame with 1 row per session and attendee
  dplyr::left_join(pd[["attendance"]],
                   pd[["attendees"]]) %>%
    dplyr::left_join(pd[["sessions"]])
}
