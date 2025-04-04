% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pagination.R
\name{clin_group_by}
\alias{clin_group_by}
\title{Configure a clintable to table by a grouping variable, which will be used as a label}
\usage{
clin_group_by(x, group_by, caption_by = NULL)
}
\arguments{
\item{x}{A clintable object}

\item{group_by}{A character vector of variable names which will be used for grouping and attached
as a label above the table headers}

\item{caption_by}{A single element string of a variable name which will be used as a caption
attached below the table body and above in the footer. Defaults to NULL.}
}
\value{
A clintable object
}
\description{
Configure a clintable to table by a grouping variable, which will be used as a label
}
\examples{
clintable(iris) |>
  clin_group_by("Species")
}
