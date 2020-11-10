#' @noRd
convert_names <- function(x, table = c("sessions", "attendance", "attendees")) {
  table <- rlang::arg_match(table)
  df <- dplyr::filter(fightr::database_names,
                      .data[["table"]] == table)
  df[["col_codes"]][match(x, df[["col_names"]])]
}

#' @noRd
convert_table <- function(df, table = c("sessions", "attendance", "attendees")) {
  table <- rlang::arg_match(table)
  x <- names(df)
  new_x <- convert_names(x, table)
  keep_cols <- !is.na(new_x)
  df <- df[,keep_cols]
  new_x <- new_x[keep_cols]
  names(df) <- new_x
  df
}
