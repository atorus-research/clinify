#' Clintable write method
#'
#' Write a clinify table out to a docx file
#'
#' @param x a clintable object
#' @param file The file path to which the file should be written
#' @param apply_defaults Apply default styles. These styles are stored in the
#'   options clinify_header_default, clinify_footer_default, and
#'   clinify_table_default respectively. Defaults to true.
#'
#' @return Invisible
#' @export
#'
#' @examples
#' ct <- clintable(mtcars)
#'
#' ct <- clin_alt_pages(
#'   ct,
#'   key_cols = c("mpg", "cyl", "hp"),
#'   col_groups = list(
#'     c("disp", "drat", "wt"),
#'     c("qsec", "vs", "am"),
#'     c("gear", "carb")
#'   )
#' )
#' 
#' # Get document object directly
#' doc <- clindoc(ct)
#'
#' # Write out docx file
#' write_clintable(ct, file.path(tempdir(), "demo.docx"))
#'
write_clintable <- function(x, file, apply_defaults = TRUE) {
  if (inherits(x, 'clindoc')) {
    doc <- x
  } else {
    doc <- as_clindoc(x, apply_defaults)
  }

  print(doc, target = file)
}

