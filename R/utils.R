#' Get the pagination method of a clintable object
#'
#' @param x A clintable object
#'
#' @return A character string specifying the pagination method
#' @export
#'
#' @examples
#'
#' ct <- clintable(mtcars)
#'
#' get_pagination_method(ct)
#'
get_pagination_method <- function(x) {
  stopifnot(inherits(x, 'clintable'))

  x$clinify_config$pagination_method
}

#' Internal method to set the string
#'
#' Individual pager functions will apply the proper method label
#'
#' @param x A clintable object
#'
#' @noRd
set_pagination_method <- function(x, method) {
  x$clinify_config$pagination_method <- method
  x
}
