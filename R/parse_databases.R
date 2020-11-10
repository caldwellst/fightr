#' @export
parse_multiple_databases <- function(...) {
  args <- list(...)
  databases <- purrr::map(args, parse_single_database)
  sessions <- purrr::map_df(databases, ~.x[["sessions"]])
  attendance <- purrr::map_df(databases, ~.x[["attendance"]])
  attendees <- purrr::map_df(databases, ~.x[["attendees"]])
  list(sessions = sessions,
       attendance = attendance,
       attendees = attendees)
}
