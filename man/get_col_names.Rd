% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_col_names.R
\name{get_col_names}
\alias{get_col_names}
\title{get_col_names}
\usage{
get_col_names(df, start_col, end_col)
}
\arguments{
\item{df}{dataframe}

\item{start_col}{first column to start at}

\item{end_col}{column to stop at}
}
\value{
column names between a range
}
\description{
get_col_names
}
\examples{
get_col_names(mtcars, cyl, wt)
}
