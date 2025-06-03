#' Assign Page Numbers to Presorted Grouped Data
#'
#' Assigns sequential page numbers to elements of a vector, grouping by 
#' unique values and allocating a specified number of rows per page. 
#' The input vector must be presorted by group.
#'
#' @param var A vector of group labels, presorted so that identical 
#'   values are contiguous.
#' @param rows Integer. The maximum number of rows per page.
#'
#' @return An integer vector of the same length as `var``, indicating 
#'   the assigned page number for each element.
#'
#' @details
#' The function splits the input vector into groups, then assigns page numbers 
#' within each group so that each page contains up to `rows`` items.
#' Page numbers increment sequentially across groups. If the input is not 
#' presorted by group, the function will throw an error.
#'
#' @examples
#' library(dplyr)
#' iris |> 
#'   mutate(
#'     page = make_grouped_pagenums(Species, 5)
#'   )
#'
#' @export
make_grouped_pagenums <- function(var, rows) {
  # Get the groups 
  fvar <- as.factor(var)
  groups <- split(var, fvar)

  # Check if the values were pre-sorted
  if (!all(var == unsplit(groups, fvar))) {
    stop("Input data should be presorted by group")
  }

  cur_page <- 1
  out <- integer()

  for (g in groups) {
    page_list <- cur_page:((cur_page - 1) + ceiling(length(g)/rows))
    page_vec <- rep(page_list, each = rows)[1:length(g)]
    out <- append(
      out, 
      page_vec
    )
    cur_page <- max(out) + 1
  }

  out
}

