#' Replace Table Cells with Word Page Number Fields
#'
#' This helper function will find placeholder text and replace the fields of
#' the flextable object with the appropriate page number fields. The function
#' will search for the text `{PAGE}` and replace with the word field for current
#' pages, and `{NUMPAGES}` for total pages. This allows you to current and total
#' page fields within Word documents.  Note that this is intended to be used in
#' the defaults for clinify_titles_default or clinify_footnotes_default.
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
#'     c("Protocol: CDISCPILOT01", "Page {PAGE} of {NUMPAGES}"),
#'     c("Table 14-2.01"),
#'     c("Summary of Demographic and Baseline Characteristics")
#'   ),
#'   "titles"
#' )
#'
#' title <- clin_replace_pagenums(title)
#'
#' footnote <- new_title_footnote(
#'   list(
#'     # We'll add tools to automate paging
#'     c("Page {PAGE}", "Total Pages: {NUMPAGES}")
#'   ),
#'   "footnotes"
#' )
#'
#' footnote <- clin_replace_pagenums(footnote)
#'
clin_replace_pagenums <- function(x) {
  pg_inds <- which(
    apply(x$body$dataset, c(1, 2), grepl, pattern = "\\{(PAGE|NUMPAGES)\\}"),
    arr.ind = TRUE
  )

  if (nrow(pg_inds) == 0) {
    return(x)
  }

  for (i in 1:nrow(pg_inds)) {
    word_field <- build_word_fields(x$body$dataset[pg_inds[i, "row"], pg_inds[i, "col"]])
    x <- compose(
      x,
      i = pg_inds[i, "row"], j = pg_inds[i, "col"],
      value = do.call(flextable::as_paragraph, word_field)
    )
  }

  x
}

# Helper to build the pagenum fields
build_word_fields <- function(text) {
  matches <- gregexpr("\\{(PAGE|NUMPAGES)\\}", text)[[1]]
  if (matches[1] == -1) {
    return(list(text))
  }

  pieces <- list()
  last_pos <- 1

  for (i in seq_along(matches)) {
    start <- matches[i]
    end <- start + attr(matches, "match.length")[i] - 1

    # Catch preceding text from begging of string or after last match
    if (start > last_pos) {
      pieces <- append(pieces, substr(text, last_pos, start - 1))
    }

    field_name <- substr(text, start + 1, end - 1)
    pieces <- append(pieces, bquote(as_word_field(x = .(field_name))))

    last_pos <- end + 1
  }

  # Catch any lagging text
  if (last_pos <= nchar(text)) {
    pieces <- append(pieces, substr(text, last_pos, nchar(text)))
  }

  return(pieces)
}
