% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/col_width.R
\name{clin_col_widths}
\alias{clin_col_widths}
\title{Set column widths using percent}
\usage{
clin_col_widths(x, ...)
}
\arguments{
\item{x}{A clintable object}

\item{...}{Named parameters where the names are columns in the flextable and
the values are decimals representing the percent of total width of the table}
}
\value{
A clintable object
}
\description{
Extraction of flextable print method with special handling of clintable pages
and
}
\examples{

ct <- clintable(mtcars)

ct <- clin_alt_pages(
  ct,
  key_cols = c("mpg", "cyl", "hp"),
  col_groups = list(
    c("disp", "drat", "wt"),
    c("qsec", "vs", "am"),
    c("gear", "carb")
  )
) |>
  clin_col_widths(mpg = .2, cyl = .2, disp = .15, vs = .15)

print(ct)

}
