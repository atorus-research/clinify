#' Blank object creator for a clintable object
#'
#' @noRd
new_clinpage <- function() {
  structure(
    list(
      header = NULL,
      body = NULL,
      footer = NULL,
      col_keys = NULL,
      caption = NULL,
      blanks = NULL,
      properties = NULL
    ),
    class = c("clinpage", "flextable")
  )
}
