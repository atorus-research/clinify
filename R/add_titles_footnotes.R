#' Add titles, footnotes, or a footnote page to a clintable or clindoc
#'
#' This function allows you to attach specified titles, footnotes, or a footnote page
#' into clintable or clindoc object. The input can be provided either as a list of character
#' vectors, or pre-built flextable.
#'
#' When using the `ls` parameter, each element of the list can contain no more than two
#' elements within each character vector. In a title, a single element will align center.
#' In a footnote, a single element will align to the left. For both titles and footnotes,
#' two elements will align split down the middle, with the left side element aligning left
#' and the right side element aligning right. In a title, a single left aligned element,
#' provide a 2 element character vector with duplicate values.
#'
#' @param x a clintable object
#' @param ls a list of character vectors, no more than 2 elements to a vector
#' @param ft A flextable object to use as the header
#'
#' @return A clintable object
#'
#' @family add_titles_footnotes
#' @rdname add_titles_footnotes
#'
#' @export
#'
#' @examples
#' clintable(mtcars) |>
#'   clin_add_titles(
#'     list(
#'       c("Left", "Right"),
#'       c("Just the middle")
#'     )
#'   ) |>
#'   clin_add_footnotes(
#'     list(
#'       c(
#'         "Here's a footnote.",
#'         format(Sys.time(), "%H:%M %A, %B %d, %Y")
#'       )
#'     )
#'   ) |>
#'   clin_add_footnote_page(
#'     list(
#'       c(
#'         "Use when you have a lot of footnotes",
#'         "And you don't want to put them on every page"
#'       )
#'     )
#'   )
#'
clin_add_titles <- function(x, ls = NULL, ft = NULL) {
  x <- add_titles_footnotes_(x, "titles", ls, ft)
  x
}

#' @family add_titles_footnotes
#' @rdname add_titles_footnotes
#' @export
clin_add_footnotes <- function(x, ls = NULL, ft = NULL) {
  x <- add_titles_footnotes_(x, "footnotes", ls, ft)
  x
}

#' @family add_titles_footnotes
#' @rdname add_titles_footnotes
#' @export
clin_add_footnote_page <- function(x, ls = NULL, ft = NULL) {
  x <- add_titles_footnotes_(x, "footnote_page", ls, ft)
  x
}

#' Single method to apply titles or footnotes
#'
#' Called by clin_add_titles and clin_add_footnotes
#' @noRd
add_titles_footnotes_ <- function(x, sect, ls = NULL, ft = NULL) {
  stopifnot(inherits(x, 'clintable') || inherits(x, 'clindoc'))

  if (all(is.null(ls), is.null(ft)) || all(!is.null(ls), !is.null(ft))) {
    stop("One of, and only one of, ls or ft must be populated")
  }

  if (!is.null(ls)) {
    ft <- new_title_footnote(ls, sect)
  }

  x$clinify_config[[sect]] <- ft
  x
}

#' Create a new title or footnote flextable
#'
#' @param x a list of character vectors, no more than 3 elements to a vector.
#' @param sect Either "titles" or "footnotes"
#'
#' @return A flextable object
#' @export
#'
#' @examples
#'
#' title <- new_title_footnote(
#'   list(
#'     # We'll add tools to automate paging
#'     c("Protocol: CDISCPILOT01", "Page {PAGE} of {NUMPAGES}"),
#'     c("Table 14-2.01"),
#'     c("Summary of Demographic and Baseline Characteristics")
#'   ),
#'   "titles"
#' )
#'
#' footnote <- new_title_footnote(
#'   list(
#'     # We'll add tools to automate paging
#'     c("Page {PAGE}", "Total Pages: {NUMPAGES}")
#'   ),
#'   "footnotes"
#' )
#'
new_title_footnote <- function(
  x,
  sect = c("titles", "footnotes", "footnote_page")
) {
  sect <- match.arg(sect)

  # Check if all lists have length <=3
  if (any(sapply(x, length) > 2)) {
    stop("All sublists must have length <= 2")
  }

  # List to hold all data frames
  dfs <- list()

  for (i in seq_along(x)) {
    elements <- x[[i]]

    # Create a vector of length 3 filled with the last element
    row <- rep(tail(elements, 1), 2)
    # Replace the first n elements of the vector with the elements of the list
    row[seq_along(elements)] <- elements

    # Create a dataframe with 1 row and 2 columns
    df <- data.frame(t(row))
    names(df) <- c("Left", "Right")

    # Add the data frame to the list
    dfs[[i]] <- df
  }

  # Combine all data frames into one
  df <- dplyr::bind_rows(dfs)

  # Convert the data frame to a flextable
  ft <- flextable::flextable(df)

  # Apply the common styling
  ft <- ft |>
    flextable::set_header_labels(Left = "", Right = "") |>
    flextable::delete_part(part = "header") |>
    flextable::delete_part(part = "footer")

  # Apply different styling based on the number of elements
  for (i in seq_along(x)) {
    elements <- x[[i]]
    if (length(elements) == 1) {
      ft <- ft |>
        flextable::merge_h(i = i, part = "body") |>
        flextable::align(
          j = 1,
          i = i,
          align = ifelse(sect == "titles", "center", "left"),
          part = "body"
        )
    } else if (length(elements) == 2) {
      ft <- ft |>
        flextable::merge_h(i = i, part = "body") |>
        flextable::align(j = 1, i = i, align = "left", part = "body") |>
        flextable::align(j = 2, i = i, align = "right", part = "body")
    }
  }

  # Return the flextable
  return(ft)
}
