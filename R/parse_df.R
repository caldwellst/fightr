#' Parse Excel export of session data from the Fight for Peace database
#'
#' `parse_session_database()` parses the Fight for Peace session database export (from Excel)
#' format into three separate frames, one for attendees, one for sessions, and
#' one linking attendees to sessions attended.
#'
#' @param path File path to Excel file
#'
#' @return List with three named components:
#'
#'     sessions: data frame of session information
#'     attendance: data frame of attendee and session attended
#'     attendees: data frame of attendee details
#'
#' @export
parse_session_database <- function(path) {
  raw_df <- suppressMessages(readxl::read_excel(path,
                                                col_types = "text"))

  session_df <- parse_session_df(raw_df)
  attendance_df <- parse_attendance_df(raw_df)
  attendee_df <- parse_attendee_df(raw_df)

  list("sessions" = session_df,
       "attendance" = attendance_df,
       "attendees" = attendee_df)
}

#' @noRd
parse_session_df <- function(df) {
  proj_ind <- which(df[[4]] == "Project")
  sess_ind <- which(df[[4]] == "Session ID")
  attendance_ind <- which(df[[4]] == "Total (participants attended)")
  session_df <- df[c(proj_ind:sess_ind, attendance_ind),
                   !is.na(unlist(df[proj_ind,]))]

  session_df <- session_df %>%
    tidyr::pivot_longer(-1) %>%
    tidyr::pivot_wider(names_from = 1) %>%
    dplyr::select(-"name")

  session_df <- convert_table(session_df, table = "sessions")
  if ("date" %in% names(session_df)) {
    year <- lubridate::year(session_df[["date"]])
    month <- lubridate::month(session_df[["date"]])
    quarter <- get_quarter(session_df[["date"]])
    session_df <- tibble::add_column(session_df,
                                     year = year,
                                     month = month,
                                     quarter = quarter,
                                     .after = "date")
  }
  session_df
}

#' @noRd
parse_attendance_df <- function(df) {
  sess_ind <- which(df[[4]] == "Session ID")
  attendance_ind <- which(df[[4]] == "Total (participants attended)")
  attendee_ind <- which(df[[4]] == "Attendee ID")

  sess_attend_df <- df[c(sess_ind, (attendee_ind + 1):(attendance_ind - 1)),
                       !is.na(unlist(df[sess_ind,]))]
  sess_attend_df[1,1] <- "Attendance ID"

  sess_attend_df %>%
    janitor::row_to_names(1) %>%
    tidyr::pivot_longer(-"Attendance ID",
                        names_to = "Session ID") %>%
    dplyr::filter(!is.na(.data[["value"]])) %>%
    dplyr::select(-"value") %>%
    convert_table(table = "attendance")
}

#' @noRd
parse_attendee_df <- function(df) {
  attendee_ind <- which(df[[4]] == "Attendee ID")
  attendance_ind <- which(df[[4]] == "Total (participants attended)")

  attendee_df <- df[(attendee_ind - 1):(attendance_ind - 1),4:ncol(df)]
  attendee_df <- attendee_df[,!is.na(unlist(attendee_df[1,])) | !is.na(unlist(attendee_df[2,]))]

  names(attendee_df) <- dplyr::case_when(
    !is.na(unlist(attendee_df[2,])) ~ unlist(attendee_df[2,]),
    !is.na(unlist(attendee_df[1,])) ~ unlist(attendee_df[1,])
  ) %>% unname

  attendee_df <- attendee_df[-c(1:2),] %>%
    convert_table(table = "attendees")

  if ("dob" %in% names(attendee_df)) {
    age <- lubridate::as.period(lubridate::interval(start = attendee_df[["dob"]],
                                                    end = Sys.time()))
    age_years <- lubridate::year(age)
    age_bins <- cut(age_years,
                    breaks = c(6, 14, 17, 25, Inf),
                    labels = c("7-14", "15-17","18-25", "Over 25"))
    attendee_df <- tibble::add_column(attendee_df,
                                      age_years = age_years,
                                      age_bins = age_bins,
                                      .after = "dob")
  }
  attendee_df
}
