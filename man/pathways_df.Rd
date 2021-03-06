% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dashboard_london.R
\name{pathways_df}
\alias{pathways_df}
\title{Get data for the Pathways dashboard}
\usage{
pathways_df(df)
}
\arguments{
\item{df}{Full data frame of attendance and session data produced by \code{\link[=get_full_session_df]{get_full_session_df()}}.}
}
\value{
A data frame.
}
\description{
\code{pathways_df()} creates a data frame with the columns and values necessary
for providing to the Pathways dashboard in Power BI. This is generated by
filtering for \code{activity_group == "Pathways"} and selecting relevant columns.
}
