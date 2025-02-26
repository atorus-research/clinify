#' @import flextable
#' @import officer
#' @importFrom htmltools browsable
#' @importFrom dplyr bind_rows
#' @importFrom magrittr `%>%`
#' @importFrom tidyselect eval_select
NULL

.onLoad <- function(libname, pkgname) {
    # Ensure that flextable and officer are loaded with clinify
    require(flextable)
    require(officer)

    doc <- clinify_docx_default()

    # Save out options to grab defaults
    options(
        clinify_default_dimensions = officer::docx_dim(doc),
        clinify_docx_default = doc,
        clinify_titles_default = clinify_titles_default,
        clinify_footnotes_default = clinify_footnotes_default,
        clinify_table_default = clinify_table_default
    )
}
