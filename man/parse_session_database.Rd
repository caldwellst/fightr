% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_df.R
\name{parse_session_database}
\alias{parse_session_database}
\title{Parse Excel export of session data from the Fight for Peace database}
\usage{
parse_session_database(path)
}
\arguments{
\item{path}{File path to Excel file}
}
\value{
List with three named components:\preformatted{sessions: data frame of session information
attendance: data frame of attendee and session attended
attendees: data frame of attendee details
}
}
\description{
\code{parse_session_database()} parses the Fight for Peace session database export (from Excel)
format into three separate frames, one for attendees, one for sessions, and
one linking attendees to sessions attended.
}
