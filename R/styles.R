
#' The function to apply default title styling
#'
#' Next styling is applied:
#' -- font colour - black \cr
#' -- all borders are removed \cr
#' -- font style - Courier New, 9pt, not bold, not italic \cr
#' -- interval between rows - 1 space \cr
#' -- padding - bottom = 0, top = 0 \cr
#'
#' One has to specify the width of the page header "table", which in this case
#' is landscape page width (11.42 in) minus two times 1 in margin.
#' ALL headers would have 3 columns underneath the hood (for left, center and
#' right-aligned text). Hence we divide by 3 to specify the width of a single
#' column
#' @import flextable
#'
#' @param x flextable object that is modified by the function.
#' @param ... any additional arguments
#'
#' @return None
#' @export
clinify_titles_default <- function(x, ...) {

  # Remove all borders as heading does not need any.
  x <- flextable::border_remove(x)
  # Setup font properties.
  x <- flextable::color(x, color = "black")
  x <- flextable::fontsize(x, size = 9)
  x <- flextable::font(x, font = "Courier New")
  x <- flextable::bold(x, bold = FALSE)
  x <- flextable::italic(x, italic = FALSE)
  # One has to specify the width of the page header "table", which in this case
  # is landscape page width (11.42 in) minus two times 1 in margin.
  # ALL headers would have 3 columns underneath the hood (for left, center and
  # right-aligned text). Hence we divide by 3 to specify the width of a single
  # column.
  x <- flextable::width(x, width = clin_default_table_width()/3)
  # Setup the interval between rows.
  x <- flextable::line_spacing(x, space = 1, part = "all")
  # Setup the cell padding.
  x <- flextable::padding(x, part = "all", padding.bottom = 0, padding.top = 0)
  x <- flextable::set_table_properties(
    x,
    layout = "fixed"
  )
}

#' The function to apply default footnote styling
#'
#' Next styling is applied:
#' -- font colour - black \cr
#' -- page footer should have single black top border over the top row that is
#'    width of 1 \cr
#' -- all borders are removed \cr
#' -- font style - Courier New, 9pt, not bold, not italic \cr
#' -- interval between rows - 1 space \cr
#' -- padding - bottom = 0, top = 0 \cr
#'
#' One has to specify the width of the page header "table", which in this case
#' is landscape page width (11.42 in) minus two times 1 in margin.
#' ALL headers would have 3 columns underneath the hood (for left, center and
#' right-aligned text). Hence we divide by 3 to specify the width of a single
#' column
#' @import flextable
#' @import officer
#'
#'
#' @param x flextable object that is modified by the function.
#' @param ... any additional arguments
#'
#' @return None
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
  x <- flextable::width(x, width = clin_default_table_width()/3)
  # Setup the interval between rows.
  x <- flextable::line_spacing(x, space = 1, part = "all")
  # Setup the cell padding.
  x <- flextable::padding(x, part = "all", padding.bottom = 0, padding.top = 0)

  x <- flextable::set_table_properties(
    x,
    layout = "fixed"
  )
}

#' The function to apply default table styling
#'
#' Headers with the same value are merged both horizontally and vertically
#' All borders are clear except for the header (bleck, solid, 0.2pt)
#' As well as top horizontal line for the table header.
#' font style - Courier New, 9pt, not bold, not italic
#' padding for table body - bottom = 0.1, top = 0.1
#' padding for table first header - top = 9
#' padding for table last header - bottom = 9
#' table's layout is set to be fixed.
#' 
#' @import flextable
#' @param x flextable object that is modified by the function.
#' @param ... any additional arguments
#'
#' @return None
#' @export
clinify_table_default <- function(x, ...) {

  # Merge cells with same name
  # (merge occurs both horizontally and vertically)
  # x <- flextable::merge_v(x, part="header")
  # x <- flextable::merge_h(x, part="header")


  # You can center-align all table headers but first, for example like this:
  # x <- align(x, align = "center", part = "header", j = 2:ncol(df))

  # Clear all borders first and apply them just for the header
  # (as horizontal lines).
  x <- flextable::border_remove(x)
  x <- flextable::hline(x,
             part = "header",
             border = officer::fp_border(
               color="black",
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
  blk_inds <- which(x$header$dataset == "", arr.ind = TRUE)
  x <- flextable::hline(x, i=blk_inds[,'row'], j=blk_inds[,'col'],
    part = "header",
    border = officer::fp_border(style="none", width=0))
  x <- flextable::hline_bottom(x, part = "header")

  # Set font properties for the table header.
  x <- flextable::font(x, part = "header", fontname = "Courier New")
  #x <- bold(x, part = "header")

  # Set font properties for the table body.
  x <- flextable::font(x, part = "body", fontname = "Courier New")

  # Set fontsize for both table header and table body.
  x <- flextable::fontsize(x, part = "all", size=9)

  # Set table's layout.
  x <- flextable::set_table_properties(
    x,
    layout = "fixed"
  )

  # Setup the cell padding for table body.
  x <- flextable::padding(x, part = "body", padding.bottom = 0.1, padding.top = 0.1)

  # Many other options are also available.
  x <- flextable::padding(x, i=1, part = "header", padding.top = 9)
  x <- flextable::padding(x, i=flextable::nrow_part(x, part="header"), part = "header", padding.bottom = 9)

}

#' Function to create a default officer docx object
#'
#' Default document attributes assume landscape A4 paper size
#'
#' @return An rdocx object from the officer package
#' @export
clinify_docx_default <- function() {

  # I want these as defaults but need to carry it forward because
  # the default section strips it off
  margins <- as.list(officer::docx_dim(officer::read_docx())$margins)

  officer::prop_section(
    page_size = page_size(orient="landscape"),
    type="continuous",
    page_margins = do.call(page_mar, margins)
  )

}

#' Helper function to get the total page width
#'
#' Using the document dimensions stored in options, calculate the 
#' width that can be used in table body, titles, and footnotes. Calculation
#' uses the page width subtracting right and left margins.
#'
#' @return An rdocx object from the officer package
#' @export
clin_default_table_width <- function() {
  sect <- getOption("clinify_docx_default")
  
  if (is.null(sect)) {
    stop("clin_col_width() cannot be used if the option 'clinify_docx_default' is not set.")
  }

  # Table width is page width - margins. 
  sect$page_size$width - (sect$page_margins$left + sect$page_margins$right)
}