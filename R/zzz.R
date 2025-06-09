#' @import flextable
#' @import officer
#' @importFrom htmltools browsable tags tagList div HTML
#' @importFrom dplyr bind_rows
#' @importFrom magrittr `%>%`
#' @importFrom tidyselect eval_select
#' @importFrom knitr knit_print
#' @importFrom utils head tail
NULL

.onLoad <- function(libname, pkgname) {
  sect <- clinify_docx_default()

  # Store any defaults
  op <- options()

  # Save out options to grab defaults
  options(
    clinify_docx_default = sect,
    clinify_titles_default = clinify_titles_default,
    clinify_footnotes_default = clinify_footnotes_default,
    clinify_table_default = clinify_table_default,
    clinify_caption_default = clinify_caption_default,
    clinify_grouplabel_default = clinify_grouplabel_default
  )

  # Reapply them
  options(op)
}
