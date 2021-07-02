#' Parse Excel export of headcount data from the Fight for Peace database
#'
#' `parse_headcount_database()` parses the Fight for Peace headcount database export (from Excel)
#' format into a single frame.
#'
#' @inheritParams parse_session_database
#'
#' @return A data frame.
#'
#' @export
parse_headcount_database <- function(path) {
  raw_df <- suppressMessages(readxl::read_excel(path,
                                                col_types = "text"))

  parse_headcount_df(raw_df)
}


#' @noRd
parse_headcount_df <- function(df) {
  proj_ind <- max(which(df[[1]] == "Project"))
  end_ind <- max(which(!is.na(df[[1]])))
  female_ind <- which(unlist(df[proj_ind,]) == "Female count")
  age_ind <- which(unlist(df[proj_ind,]) == "Age range")
  headcount_df <- df[proj_ind:end_ind,
                     c(1:female_ind, age_ind)]

  headcount_df <- janitor::row_to_names(headcount_df, 1)

  headcount_df <- convert_table(headcount_df, table = "headcount")
  if ("date" %in% names(headcount_df)) {
    year <- lubridate::year(headcount_df[["date"]])
    month <- lubridate::month(headcount_df[["date"]])
    quarter <- get_quarter(headcount_df[["date"]])
    session_df <- tibble::add_column(headcount_df,
                                     year = year,
                                     month = month,
                                     quarter = quarter,
                                     .after = "date")
  }
  headcount_df
}
