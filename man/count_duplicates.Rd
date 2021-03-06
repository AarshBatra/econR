% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dailyLogicChecks.R
\name{count_duplicates}
\alias{count_duplicates}
\title{Count duplicates for a given (single or combination) of column(s).}
\usage{
count_duplicates(df, uniq_identifier_col)
}
\arguments{
\item{df}{dataset(\code{tibble}/\code{data.frame}) object from which the
\code{uniq_identifier_col} is chosen.}

\item{uniq_identifier_col}{a character vector of column name(s) that uniquely
identifies the dataset. In here, \code{tidyselect}
can be used to select columns. See examples below.}
}
\value{
a tibble containing the the variable(s) contained in
\code{uniq_identifier_col} and their corresponding duplicate count and
percentage (of total duplicates in data). If there is more than one variable in
\code{uniq_identifier_col}, the tibble shows the duplicate count (and
percentages) for those joint variables.
}
\description{
Counts the total number of duplicates for a given (single or combination)
of columns. "Combination" refers to the case, in which there is more than
one column, which in combination uniquely identify the dataset. In dplyr
terminology, we are using more than one column to group our variables (
using the \code{group_by} family of functions.).
}
\examples{
count_duplicates(df = dataObj, uniq_identifier_col = c("ID"))

count_duplicates(df = dataObj, uniq_identifier_col = c("ID", "Name"))

count_duplicates(df = dataObj, uniq_identifier_col
= tidyselect::contains("abc")) # tidyselection used to select columns.

}
