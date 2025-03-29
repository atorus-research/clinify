#' Replace Table Cells with Word Page Number Fields
#'
#' This helper function will find placeholder text and replace the fields of
#' the flextable object with the appropriate page number fields. This
#' allows you to current and total page fields within Word documents. 
#' Note that this is intended to be used in the defaults for 
#' clinify_titles_default or clinify_footnotes_default.
#' 
#' @param x A clintable object
#'
#' @return A clintable object
#' @export
#'
#' @examples
#' 
#' title <- new_title_footnote(
#'   list(
#'     # We'll add tools to automate paging
#'     c("Protocol: CDISCPILOT01", "Page X of Y"),
#'     c("Table 14-2.01"),
#'     c("Summary of Demographic and Baseline Characteristics")
#'   ),
#'   "titles"
#' )
#'   
#' title <- replace_with_pagenums(title)
#' 
replace_with_pagenums <- function(x) {
  pg_inds <- which(x$body$dataset == "Page X of Y", arr.ind=TRUE)
  
  if (nrow(pg_inds) > 0) {
    x <- compose(x, i = pg_inds[, 'row'], j = pg_inds[,'col'], 
    value = as_paragraph("Page ",
      as_word_field(x = "Page"),
      " of ", as_word_field(x = "NumPages")
      )
  )
  }
  x
}