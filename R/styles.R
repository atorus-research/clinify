#' Apply Default Clinical Styling to Clintables
#'
#' These functions apply default styling to `clintable` objects used for
#' clinical tables, including titles and footnotes. The styling includes
#' removing borders, setting font properties, and adjusting table width,
#' line spacing, and padding.
#'
#' @param x A `clintable` object representing the table (title or footnote).
#' @param ... Additional arguments (currently unused).
#'
#' @return A `clintable` object with the applied styling.
#' @family Clinify Defaults
#' @rdname clinify_defaults
#' @export
#'
#' @examples
#'
#' op <- options()
#'
#' sect <- clinify_docx_default()
#'
#' # Save out options to grab defaults
#' options(
#'   clinify_docx_default = sect,
#'   clinify_titles_default = clinify_titles_default,
#'   clinify_footnotes_default = clinify_footnotes_default,
#'   clinify_table_default = clinify_table_default,
#'   clinify_caption_default = clinify_caption_default
#' )
#'
#' options(op)
clinify_titles_default <- function(x, ...) {
  # Remove all borders as heading does not need any.
  x <- flextable::border_remove(x)
  # Setup font properties.
  x <- flextable::color(x, color = "black")
  x <- flextable::fontsize(x, size = 9)
  x <- flextable::font(x, font = "Courier New")
  x <- flextable::bold(x, bold = FALSE)
  x <- flextable::italic(x, italic = FALSE)

  # Default table width
  x <- flextable::width(x, width = clin_default_table_width() / 3)
  # Setup the interval between rows.
  x <- flextable::line_spacing(x, space = 1, part = "all")
  # Setup the cell padding.
  x <- flextable::padding(x, part = "all", padding.bottom = 0, padding.top = 0)
  x <- flextable::set_table_properties(
    x,
    layout = "fixed"
  )
  # Automatically find and update a pagenum string
  x <- replace_with_pagenums(x)
  x
}

#' @family Clinify Defaults
#' @rdname clinify_defaults
#' @export
clinify_footnotes_default <- function(x, ...) {
  # Remove all borders.
  x <- flextable::border_remove(x)
  # Page footer should have single top border over the top row.
  x <- flextable::hline_top(x, part = "body", border = officer::fp_border(color = "black", width = 1))
  # Setup font properties.
  x <- flextable::color(x, color = "black")
  x <- flextable::fontsize(x, size = 9)
  x <- flextable::font(x, font = "Courier New")
  x <- flextable::bold(x, bold = FALSE)
  x <- flextable::italic(x, italic = FALSE)
  # One has to specify the width of the page header "table", which in this case
  # is landscape page width minus two times 1 in margin.
  x <- flextable::width(x, width = clin_default_table_width() / 3)
  # Setup the interval between rows.
  x <- flextable::line_spacing(x, space = 1, part = "all")
  # Setup the cell padding.
  x <- flextable::padding(x, part = "all", padding.bottom = 0, padding.top = 0)

  x <- flextable::set_table_properties(
    x,
    layout = "fixed"
  )
  # Automatically find and update a pagenum string
  x <- replace_with_pagenums(x)
  x
}

#' @family Clinify Defaults
#' @rdname clinify_defaults
#' @export
clinify_table_default <- function(x, ...) {
  # Clear all borders first and apply them just for the header
  # (as horizontal lines).
  x <- flextable::border_remove(x)
  x <- flextable::hline(x,
    part = "header",
    border = officer::fp_border(
      color = "black",
      style = "solid",
      width = 0.2
    )
  )
  # Top horizontal line for the table header.
  x <- flextable::hline_top(x, part = "header")
  x <- flextable::hline(x,
    part = "header",
    border = officer::fp_border()
  )

  # Remove blank bottoms
  blk_inds <- which(x$header$dataset == "" | x$header$dataset == " ", arr.ind = TRUE)
  x <- flextable::hline(x,
    i = blk_inds[, "row"], j = blk_inds[, "col"],
    part = "header",
    border = officer::fp_border(style = "none", width = 0)
  )
  x <- flextable::hline_bottom(x, part = "header")

  # Set font properties for the table header.
  x <- flextable::font(x, part = "all", fontname = "Courier New")

  # Set fontsize for both table header and table body.
  x <- flextable::fontsize(x, part = "all", size = 9)

  # Set table's layout.
  x <- flextable::set_table_properties(
    x,
    layout = "fixed"
  )

  # Setup the cell padding for table body.
  x <- flextable::padding(x, part = "body", padding.bottom = 0.1, padding.top = 0.1)

  # Many other options are also available.
  x <- flextable::padding(x, i = 1, part = "header", padding.top = 9)
  x <- flextable::padding(x, i = flextable::nrow_part(x, part = "header"), part = "header", padding.bottom = 9)
  x
}

#' @family Clinify Defaults
#' @rdname clinify_defaults
#' @export
clinify_caption_default <- function(x, ...) {
  # Set font properties for the table header.
  x <- flextable::font(x, part = "footer", fontname = "Courier New")
  # Set fontsize for both table header and table body.
  x <- flextable::fontsize(x, part = "footer", size = 9)
  x
}

#' @family Clinify Defaults
#' @rdname clinify_defaults
#' @export
clinify_docx_default <- function() {
  # I want these as defaults but need to carry it forward because
  # the default section strips it off
  margins <- as.list(
    append(officer::docx_dim(officer::read_docx())$margins, list(gutter = 0))
  )

  officer::prop_section(
    page_size = officer::page_size(orient = "landscape"),
    type = "continuous",
    page_margins = do.call(officer::page_mar, margins)
  )
}

#' Get the Default Table Width for Clinical Documents
#'
#' This function calculates the default table width based on the page width
#' and margins specified in the `clinify_docx_default` option.
#'
#' @return An rdocx object from the officer package
#' @export
#'
#' @examples
#' clin_default_table_width()
clin_default_table_width <- function() {
  sect <- getOption("clinify_docx_default")

  if (is.null(sect)) {
    stop("clin_default_table_width() cannot be used if the option 'clinify_docx_default' is not set.")
  }

  # Table width is page width - margins.
  sect$page_size$width - (sect$page_margins$left + sect$page_margins$right)
}
