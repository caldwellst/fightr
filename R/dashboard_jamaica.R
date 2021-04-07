#' Create files necessary for FFP Jamaica's Power BI dashboard
#'
#' `get_pbi_dashboard_jamaica()` takes in the parsed databases coming from
#' [parse_single_database()] or [parse_multiple_databases()] and produces the
#' data frames necessary for use in FFP Jamaica's Power BI dashboards.
#' The function exports a list of data frames that will go into the dashboards in R,
#' and also exports them in Excel to a specified file path.
#'
#' The data frames filter data based on a specific set of attributes. Specifically:
#'
#' * Open Access filters data for sessions where `project == "Open Access"`
#' * Employability filters data for sessions where `project == "Employment"`
#' * Pathways filters data for sessions where `activity_group == "Pathways"`
#' * Special Project filters data for sessions where `project == "Special Project"`
#' * Youth leadership filters data for sessions where `project == "Youth Leadership"`
#'
#' Relevant columns are then selected for each of these activity groups, written
#' to Excel, and exported as a list.
#'
#' @inherit get_pbi_dashboard_london params
#'
#' @export
get_pbi_dashboard_jamaica <- function(pd,
                                     file = NULL,
                                     ...) {
  # Join all data to attendance data frames
  df <- get_full_df(pd)

  # Dashboard data frames

  df <- jamaica_pbi_df(df)

  # Export to Excel
  if (!is.null(file)) {
    openxlsx::write.xlsx(df,
                         file,
                         ...)
  }

  df
}

#' Get data for the Jamaica PBI dashboard
#'
#' `jamaica_pbi_df()` creates a data frame with the columns and values necessary
#' for providing to the Open Access dashboard in Power BI. This is generated on
#' all rows within the Jamaica data provided.
#'
#' @inherit open_access_df params return

#'
#' @export
jamaica_pbi_df <- function(df) {
  nms <- c("Community" = "location",
           "Attendee ID" = "attendee_id",
           "Session ID" = "session_id",
           "Activity group" = "activity_group",
           "Activity Type-PROJ" = "project",
           "Activity" = "activity",
           "Title" = "title",
           "Date of Activity" = "date",
           "Date recoded" = "quarter",
           "Location" = "location",
           "duration (hrs)" = "duration_hrs",
           "Total (participants attended)" = "total_participants",
           "Gender" = "gender",
           "dob" = "dob",
           "age" = "age_years")

  assert_df(df, nms)

  df %>%
    dplyr::select(dplyr::any_of(nms)) %>%
    dplyr::mutate("Value" := "Participant",
                  "Lead partner" := NA_character_)
}
