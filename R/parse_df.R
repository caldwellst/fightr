#' Parse Excel export from the Fight for Peace database
#'
#' `parse_ffp_database()` parses the Fight for Peace database export (from Excel)
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
parse_ffp_database <- function(path) {
  raw_df <- suppressMessages(readxl::read_excel(path))

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

  session_df %>%
    tidyr::pivot_longer(-1) %>%
    tidyr::pivot_wider(names_from = 1) %>%
    dplyr::rename("project" = "Project",
                  "activity_group" := "Activity group",
                  "activity" := "Activity",
                  "title" = "Title",
                  "location_id" = "Location ID",
                  "date" = "Date",
                  "start_time" = "Start time",
                  "duration_hrs" = "Duration (hrs)",
                  "status" = "Status",
                  "session_id" = "Session ID",
                  "total_attendance" = "Total (participants attended)")
    dplyr::mutate(dplyr::across(c("location_id", "duration_hrs", "date", "start_time", "session_id", "total_attended"),
                                as.numeric),
                  "data" := as.Date(.data[["date"]], origin = "1899-12-30"),
                  "year" := lubridate::year(.data[["date"]]),
                  "month" := lubridate::month(.data[["month"]]),
                  "start_time" = chron::as.times(.data[["start_time"]])) %>%
    dplyr::select(-"name")
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
    dplyr::select(-"value")
}

#' @noRd
parse_attendee_df <- function(df) {
  attendee_ind <- which(df[[4]] == "Attendee ID")
  attendance_ind <- which(df[[4]] == "Total (participants attended)")

  attendee_df <- df[(attendee_ind - 1):attendance_ind,4:ncol(df)]
  attendee_df <- attendee_df[,!is.na(unlist(attendee_df[1,])) | !is.na(unlist(attendee_df[2,]))]

  names(attendee_df) <- dplyr::case_when(
    !is.na(unlist(attendee_df[1,])) ~ unlist(attendee_df[1,]),
    !is.na(unlist(attendee_df[2,])) ~ unlist(attendee_df[2,])
  ) %>% unname

  attendee_df[-c(1:2),] %>%
    dplyr::rename("attendee_id" = "Attendee ID",
                  "sessions_attended" = "Total (sessions attended)",
                  "time_attended_mins" = "Total (mins)",
                  "time_attended_hrs" = "Total (hours)",
                  )
    dplyr::mutate(dplyr::across(c("")),
                  dplyr::across(c("DOB", "Registered", "Added to Upshot", "First session", "Last session"),
                                as.Date,
                                origin = "1899-12-30"))
}
