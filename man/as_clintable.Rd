% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clintable.R
\name{as_clintable}
\alias{as_clintable}
\title{Convert a flextable into a clintable object}
\usage{
as_clintable(x, page_by = NULL, group_by = NULL)
}
\arguments{
\item{page_by}{A variable in the input dataframe to use for pagination}

\item{group_by}{A variable which will be used for grouping and attached
as a label above the table headers}

\item{ft}{A flextable object}
}
\value{
A clintable object
}
\description{
Convert a flextable into a clintable object
}
\examples{
ft <- flextable(mtcars)
as_clintable(ft)
}
