#' Names of columns in data frames parsed from the FFP databases.
#'
#' Contains all possible names found in FFP parsed databases from the UK
#' and Jamaica. The file is used to standardize R data frame names so that data
#' from each database can be pulled into a single data frame.
#'
#' @format A data frame with `r nrow(database_names)` rows and `r ncol(database_names)` variables:
#' \describe{
#'   \item{table}{Table that the name is found in. Either "session", "attendance", and "attendees".}
#'   \item{col_names}{Original names found in the FFP databases.}
#'   \item{col_codes}{Corresponding column codes for use when parsing FFP database files.}
#' }
#'
"database_names"
