% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_cleaning.R
\name{handle_NA}
\alias{handle_NA}
\title{Function replace NAs with 0 for integers and replace NAs with "missing" in character columns}
\usage{
handle_NA(data, set_num = 0, set_logical = FALSE,
  set_char = "missing", set_array)
}
\arguments{
\item{set_num}{replace NULL values in numeric type by set_num}

\item{set_logical}{replace NULL values in logical type by set_num}

\item{set_char}{replace NULL values in character type by set_num}
}
\value{
data without NULL values
}
\description{
Function replace NAs with 0 for integers and replace NAs with "missing" in character columns
}
