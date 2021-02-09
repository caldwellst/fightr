#' Check that parsed dataframe list is passed correctly
#'
#' @param pd Parsed dataframe list to check
assert_pd <- function(pd) {
  nm_check <- c("sessions", "attendance", "attendees")
  if (!is.list(pd) && all(nm_check %in% names(pd))) {
    stop("`pd` must be a list with 3 data frames produced by `parse_single_database()` or `parse_multiple_databases()`.",
         call. = FALSE)
  }

  df_check <- sapply(pd, is.data.frame)
  if (!all(df_check)) {
    stop(sprintf("Check `pd` components which are not data frames: %s",
                 paste(names(df_check)[!df_check]),
                 collapse = ", "),
         call. = FALSE)
  }
}

#' Check data frame and columns
#'
#' @param df A data frame to check
#' @param cols Character vector of columns to check for
assert_df <- function(df, cols) {
  assert_df_type(df)
  assert_df_cols(df, cols)
}

#' Check that a data frame is a data frame
#'
#' @inheritParams assert_df
assert_df_type <- function(df) {
  if (!is.data.frame(df)) {
    stop(sprintf("`df` must be a data frame, not a %s.",
                 class(df)[1]),
         call. = FALSE)
  }
}

#' Check that columns are in a data frame
#'
#' @inheritParams assert_df
assert_df_cols <- function(df, cols) {
  nm_chk <- cols %in% names(df)
  if (!all(nm_chk)) {
    stop(sprintf("%s column(s) missing from `df`.",
                 paste(cols[!nm_chk], collapse = ", ")),
         call. = FALSE)
  }
}
