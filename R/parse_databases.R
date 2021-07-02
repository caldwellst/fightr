#' Load multiple FFP session databases
#'
#' `parse_multiple_session_databases()` loads in multiple Excel files containing data
#' exported from the FFP session database and combines them together. This can be used
#' to combine databases from different country offices or even from different years.
#' Where there are overlapping `attendee_id` rows from multiple databases (such
#' as data from multiple years), the data are automatically combined. Total columns
#' are summed, first session is the earliest, latest session the latest, and other
#' columns take the observation from the last database provided in the `paths` variable.
#'
#' @param paths File paths to FFP databases
#'
#' @inherit parse_session_database return
#'
#' @export
parse_multiple_session_databases <- function(paths) {
  databases <- purrr::map(paths, parse_session_database)
  sessions <- purrr::map_df(databases, ~.x[["sessions"]])
  attendance <- purrr::map_df(databases, ~.x[["attendance"]])
  attendees <- purrr::map_df(databases, ~.x[["attendees"]]) %>%
    dplyr::group_by(.data[["attendee_id"]]) %>%
    dplyr::summarize(dplyr::across(dplyr::any_of(c("total_sessions",
                                                   "total_mins",
                                                   "total_hrs")),
                                   sum,
                                   na.rm = TRUE),
                     dplyr::across(dplyr::any_of("first_session"),
                                   min,
                                   na.rm = TRUE),
                     dplyr::across(dplyr::any_of("last_session"),
                                   max,
                                   na.rm = TRUE),
                     dplyr::across(-dplyr::all_of(c("total_sessions",
                                             "total_mins",
                                             "total_hrs",
                                             "first_session",
                                             "last_session")),
                            ~.x[dplyr::n()]),
                     .groups = "drop")

  list(sessions = sessions,
       attendance = attendance,
       attendees = attendees)
}

#' Load multiple FFP headcount databases
#'
#' `parse_multiple_headcount_databases()` loads in multiple Excel files containing data
#' exported from the FFP headcount database and combines them together. This can be used
#' to combine databases from different country offices or even from different years.
#'
#' @inheritParams parse_multiple_session_databases
#'
#' @inherit parse_headcount_database return
#'
#' @export
parse_multiple_headcount_databases <- function(paths) {
  purrr::map_dfr(paths, parse_headcount_database)
}


