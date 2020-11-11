#' @noRd
convert_names <- function(x, table = c("sessions", "attendance", "attendees")) {
  table <- rlang::arg_match(table)
  df <- dplyr::filter(fightr::database_names,
                      .data[["table"]] == table)
  df[["col_codes"]][match(x, df[["col_names"]])]
}

#' @noRd
get_col_types <- function(x, table = c("sessions", "attendance", "attendees")) {
  table <- rlang::arg_match(table)
  df <- dplyr::filter(fightr::database_names,
                      .data[["table"]] == table)
  df[["type"]][match(x, df[["col_names"]])]
}

#' @noRd
convert_table_type <- function(df, types) {
  conversions <- list("d" = as.numeric,
                      "cd" = readr::parse_number,
                      "D" = function(x) as.Date(as.numeric(x), origin = "1899-12-30"),
                      "t" = function(x) chron::as.times(as.numeric(x))
  )
  for (i in unique(types)) {
    inds <- which(i == types)
    df <- dplyr::mutate(df,
                        dplyr::across(inds,
                                      conversions[[match(i, names(conversions))]]))
  }
  df
}

#' @noRd
convert_table <- function(df, table = c("sessions", "attendance", "attendees")) {
  table <- rlang::arg_match(table)
  x <- names(df)
  new_x <- convert_names(x, table)
  types <- get_col_types(x, table)
  keep_cols <- !is.na(new_x)
  df <- df[,keep_cols]
  new_x <- new_x[keep_cols]
  types <- types[keep_cols]
  names(df) <- new_x
  convert_table_type(df, types)
}
