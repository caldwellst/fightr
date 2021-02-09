#' Get quarter from date vector
#'
#' `get_quarter()` takes a date vector and gets quarter from it in the format
#' `YYYY QD`, such as `2020 Q1`.
#'
#' @param x Date vector.
#'
#' @return Character vector.
get_quarter <- function(x) {
  assert_date(x)
  qtr_x <- lubridate::quarter(x, with_year = TRUE)
  stringr::str_replace(qtr_x, "\\.", " Q")
}

#' @inheritParams get_quarter
assert_date <- function(x) {
  if (!lubridate::is.Date(x)) {
    stop(sprintf("`date` column must be a date class, not %s.",
                 class(x)),
         call. = FALSE)
  }
}
