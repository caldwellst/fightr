#' @noRd
convert_names <- function(x, table = c("sessions", "attendance", "attendees")) {
  table <- rlang::arg_match(table)
  df <- dplyr::filter(fightr::database_names,
                      .data[["table"]] == table)
  df[["col_codes"]][match(x, df[["col_names"]])]
}
