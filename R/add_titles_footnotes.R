#' Add page footer
#'
#' This function adds a text, passed as argument, into .docx bottom colontitle (page footer). As a
#' result, the text is repeated on each page of the document. Takes a list of lists as an input parameter,
#' where each sublist represents a separate line (or row) of text. There is also a possibility to add a row
#' with both left- and right-aligned text. To do so - simply pass two items to a sublist instead of one.
#'
#' @param x a clintable object
#' @param ls a list of character vectors, no more than 3 elements to a vector
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
#'       c("Left", "Center", "Right"),
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
#'   )
#' 
clin_add_titles <- function(x, ls = NULL, ft = NULL) {
  x <- add_titles_footnotes_(x, "titles", ls, ft)
}

#' @family add_titles_footnotes
#' @rdname add_titles_footnotes
#' @export
clin_add_footnotes <- function(x, ls = NULL, ft = NULL) {
  x <- add_titles_footnotes_(x, "footnotes", ls, ft)
}

#' Single method to apply titles or footnotes
#'
#' Called by clin_add_titles and clin_add_footnotes
#' @noRd
add_titles_footnotes_ <- function(x, sect, ls = NULL, ft = NULL) {

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
#' #TODO:
new_title_footnote <- function(x, sect = c("titles", "footnotes")) {

  sect <- match.arg(sect)

  # Check if all lists have length <=3
  if (any(sapply(x, length) > 3)) {
    stop("All sublists must have length <= 3")
  }

  # List to hold all data frames
  dfs <- list()

  for (i in seq_along(x)) {
    elements <- x[[i]]

    # Create a vector of length 3 filled with the last element
    row <- rep(tail(elements, 1), 3)
    # Replace the first n elements of the vector with the elements of the list
    row[seq_along(elements)] <- elements

    # Create a dataframe with 1 row and 3 columns
    df <- data.frame(t(row))
    names(df) <- c("Left", "Center", "Right")

    # Add the data frame to the list
    dfs[[i]] <- df
  }

  # Combine all data frames into one
  df <- dplyr::bind_rows(dfs)

  # Convert the data frame to a flextable
  ft <- flextable::flextable(df)

  # Apply the common styling
  ft <- ft %>%
    flextable::set_header_labels(Left = "", Center = "", Right = "") %>%
    flextable::delete_part(part="header") %>%
    flextable::delete_part(part="footer")

  # Apply different styling based on the number of elements
  for (i in seq_along(x)) {
    elements <- x[[i]]
    if (length(elements) == 1) {
      ft <- ft %>%
          flextable::merge_h(i=i, part = "body") %>%
          flextable::align(j=1, i=i,
                           align = ifelse(sect == "titles",
                                          "center",
                                          "left"),
                           part = "body")
    } else if (length(elements) == 2) {
      ft <- ft %>%
          flextable::merge_h(i=i, part = "body") %>%
          flextable::align(j = 1, i=i, align = "left", part = "body") %>%
          flextable::align(j = 2, i=i, align = "right", part = "body")
    } else if (length(elements) == 3) {
      ft <- ft %>%
          flextable::merge_h(i=i, part = "body") %>%
          flextable::align(j = 1, i=i, align = "left", part = "body") %>%
          flextable::align(j = 2, i=i, align = "center", part = "body") %>%
          flextable::align(j = 3, i=i, align = "right", part = "body")
    }
  }


  # Return the flextable
  return(ft)
}
