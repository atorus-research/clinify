% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{get_group_starts_from_df}
\alias{get_group_starts_from_df}
\title{The function to get all group-starting rows from rowlabel column.}
\usage{
get_group_starts_from_df(df, group_starts = c("\\t"), row_label_no = 1)
}
\arguments{
\item{df}{Dataframe under analysis.}

\item{group_starts}{a list of unique indents that are used in supplied data frame}

\item{row_label_no}{Order number of rowlabel column.}
}
\value{
Vector of the strings that are group-starting rows from rowlabel column
}
\description{
Row is considered a group start if one of the specified indents is detected
at its beginning, and the previous row has a different indent.
Currently, group start at first row can not be detected by this function.
}
\examples{
t_1_1 <- data.frame(list(c('Age', '\t<18', '\t18<=65', 'Weight', '\t<150', '\t150<=250')))
group_starts <- get_group_starts_from_df(t_1_1, row_label_no=1)
}
