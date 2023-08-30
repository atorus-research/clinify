header_style_default <- function(x, ...) {
  
  # Remove all borders as heading does not need any.
  x <- border_remove(x)
  # Setup font properties.
  x <- color(x, color = "black")
  x <- fontsize(x, size = 9)
  x <- font(x, font = "Courier New")
  x <- bold(x, bold = FALSE)
  x <- italic(x, italic = FALSE)
  # One has to specify the width of the page header "table", which in this case
  # is landscape page width (11.42 in) minus two times 1 in margin.
  # ALL headers would have 3 columns underneath the hood (for left, center and
  # right-aligned text). Hence we divide by 3 to specify the width of a single
  # column.
  x <- width(x, width = 9.92/3)
  # Setup the interval between rows.
  x <- line_spacing(x, space = 1, part = "all")
  # Setup the cell padding.
  x <- padding(x, part = "all", padding.bottom = 0, padding.top = 0)
}


footer_style_default <- function(x, ...) {
  
  # Remove all borders.
  x <- border_remove(x)
  # Page footer should have single top border over the top row.
  x <- hline_top(x, part = "body", border = fp_border(color = "black", width = 1))
  # Setup font properties.
  x <- color(x, color = "black")
  x <- fontsize(x, size = 9)
  x <- font(x, font = "Courier New")
  x <- bold(x, bold = FALSE)
  x <- italic(x, italic = FALSE)
  # One has to specify the width of the page header "table", which in this case
  # is landscape page width (11.42 in) minus two times 1 in margin.
  x <- width(x, width = 9.92/3)
  # Setup the interval between rows.
  x <- line_spacing(x, space = 1, part = "all")
  # Setup the cell padding.
  x <- padding(x, part = "all", padding.bottom = 0, padding.top = 0)
}


table_style_default <- function(x, ...) {
  
  # Merge cells with same name
  # (merge occurs both horizontally and vertically)
  x <- merge_h(x, part="header")
  x <- merge_v(x, part="header")
  
  # You can center-align all table headers but first, for example like this:
  # x <- align(x, align = "center", part = "header", j = 2:ncol(df))
  
  # Clear all borders first and apply them just for the header
  # (as horizontal lines).
  x <- border_remove(x)
  x <- hline(x,
             part = "header",
             border = fp_border(
               color="black",
               style = "solid",
               width = 0.2
             )
  )
  # Top horizontal line for the table header.
  x <- hline_top(x, part = "header")
  x <- hline_bottom(x, part = "header")
  x <- hline(x,
             part = "header",
             border = fp_border()
  )
  
  
  # Set font properties for the table header.
  x <- font(x, part = "header", fontname = "Courier New")
  #x <- bold(x, part = "header")
  
  # Set font properties for the table body.
  x <- font(x, part = "body", fontname = "Courier New")
  
  # Set fontsize for both table header and table body.
  x <- fontsize(x, part = "all", size=9)
  
  # -----> !!! <-----
  # This one requires more in-depth exploration. This ensures i.e. AEDECODES
  # can break onto next page if the table is big.
  #x <- keep_with_next(x, part = "body", value = FALSE)
  
  # Set table's layout.
  x <- set_table_properties(
    x,
    layout = "fixed"
  )
  
  # Setup the cell padding for table body.
  x <- padding(x, part = "body", padding.bottom = 0.1, padding.top = 0.1)
  
  # Many other options are also available.
  x <- padding(x, i=1, part = "header", padding.top = 9)
  x <- padding(x, i=nrow_part(x, part="header"), part = "header", padding.bottom = 9)
  
}
