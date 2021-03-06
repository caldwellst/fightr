% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dashboard_london.R
\name{employability_df}
\alias{employability_df}
\title{Get data for the Employability dashboard}
\usage{
employability_df(df)
}
\arguments{
\item{df}{Full data frame of attendance and session data produced by \code{\link[=get_full_session_df]{get_full_session_df()}}.}
}
\value{
A data frame.
}
\description{
\code{employability_df()} creates a data frame with the columns and values necessary
for providing to the Employability dashboard in Power BI. This is generated by
filtering for \code{project == "Employment"} and selecting relevant columns.
}
