#' Add Padding Between Groups in a Clinical Flextable
#'
#' Adds top padding to rows in a `clintable`` based on changes
#' in a grouping variable or non-empty values. Useful for visually separating groups
#' in a table
#'
#' @param x A clintable
#' @param pad_by A string indicating the column name used to detect group changes.
#' @param size Numeric value for the base padding size (default is 9).
#' @param when Character string indicating when to apply padding:
#'   \itemize{
#'     \item `"notempty"`: Add padding when the value in `pad_by` is not empty.
#'     \item `"change"`: Add padding when the value in `pad_by` changes from the previous row.
#'   }
#' @param drop Keep or drop the padding variable used to identify padding locations
#'
#' @return A `clintable` object with modified padding.
#' @export
#'
#' @examples
#'
#' ct <- clintable(mtcars) |>
#'   clin_group_pad('gear')
#'
#' ct <- clintable(mtcars) |>
#'   clin_group_pad('gear', size = 15)
#'
clin_group_pad <- function(
  x,
  pad_by,
  size = 9,
  when = c("change", "notempty"),
  drop = FALSE
) {
  when <- match.arg(when)
  refdat <- x$body$dataset

  # Find every index the page by variable changes
  splits <- find_split_inds(refdat[[pad_by]], when)
  splits[1] <- FALSE

  x <- flextable::padding(x, i = which(splits), padding.top = size + 5)

  if (drop) {
    x <- flextable::delete_columns(x, pad_by)
  }
  x
}
