#' Add page footer
#'
#' This function adds a text, passed as argument, into .docx bottom colontitle (page footer). As a
#' result, the text is repeated on each page of the document. Takes a list of lists as an input parameter,
#' where each sublist represents a separate line (or row) of text. There is also a possibility to add a row
#' with both left- and right-aligned text. To do so - simply pass two items to a sublist instead of one.
#' @import flextable
#' @import dplyr
#' @importFrom utils tail
#' @param list_of_lists Encoded footers.
#' @return Empty flextable object with footers assigned based on list_of_lists
#'
#' @family add_page_*
#'
#' @export
#'
#' @examples
#' add_page_footer(c(c("left aligned", "centered", "right aligned"),
#'                   c("left aligned", "right aligned")))
#' add_page_footer(NA)
#' add_page_footer(c(c("right aligned")))
#' try(add_page_footer(c(c("i", "like", "programming", 'NA'))))
add_page_footer <- function(list_of_lists) {
  # Check if all lists have length <=3
  if (any(sapply(list_of_lists, length) > 3)) {
    stop("All sublists must have length <= 3")
  }

  # List to hold all data frames
  dfs <- list()

  for (i in seq_along(list_of_lists)) {
    elements <- list_of_lists[[i]]

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
  for (i in seq_along(list_of_lists)) {
    elements <- list_of_lists[[i]]
    if (length(elements) == 1) {
      ft <- ft %>%
          flextable::merge_h(i=i, part = "body") %>%
          flextable::align(j=1, i=i, align = "left", part = "body")
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
