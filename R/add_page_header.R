#' Add page header
#'
#' Creates headers in the empty flextable object from the list of headers encoded as triplets/pairs/singles each
#' based on alignment: singles are centered, doubles are aligned to l/r corners and triplets are l/c/r divided
#' @import flextable
#' @import dplyr
#' @importFrom utils tail
#' @param list_of_lists Encoded headers.
#' @return Empty flextable object with headers assigned based on list_of_lists
#'
#' @family add_page_*
#'
#' @export
#'
#' @examples
#' add_page_header(list(c("left aligned", "centered", "right aligned"),
#'                    c("left aligned", "right aligned")))
#' add_page_header(list(c("centered")))
#' try(add_page_header(list(c("i", "like", "programming", 'NA'))))
add_page_header <- function(list_of_lists) {
  # Check if passed argument is of type 'list'
  if (typeof(list_of_lists) != 'list') {
    stop("You should pass an argument of type 'list'.")
  }
  # Check if all lists have length <=3
  if (any(sapply(list_of_lists, length) > 3)) {
    stop("All sublists must have length <= 3")
  }

  return_max_len_id <- function(x){
    len_v <- sapply(x, nchar) %>% unname()
    return(which(len_v == max(len_v)))
  }

  # List to hold all data frames
  dfs <- list()

  for (i in seq_along(list_of_lists)) {
    elements <- list_of_lists[[i]]

    # Create a vector of length 3 filled with the last element
    # need to create a smart version where longer value will be taken
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
      flextable::delete_part(part="header")

  # Apply different styling based on the number of elements
  for (i in seq_along(list_of_lists)) {
    elements <- list_of_lists[[i]]
    if (length(elements) == 1) {
      ft <- ft %>%
          flextable::merge_h(i=i, part = "body") %>%
          flextable::align(j=1, i=i, align = "center", part = "body")
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
