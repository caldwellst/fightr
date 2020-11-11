#' Load multiple FFP databases
#'
#' @param paths File paths to FFP databases
#'
#' @export
parse_multiple_databases <- function(paths) {
  databases <- purrr::map(paths, parse_single_database)
  sessions <- purrr::map_df(databases, ~.x[["sessions"]])
  attendance <- purrr::map_df(databases, ~.x[["attendance"]])
  attendees <- purrr::map_df(databases, ~.x[["attendees"]])
  list(sessions = sessions,
       attendance = attendance,
       attendees = attendees)
}
