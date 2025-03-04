
#' Set the column headers of the output clintable
#' 
#' This function allows you to apply column headers named arguments
#' and character vectors. Separate elements of the character vector 
#' are converted to separate levels of the output table header. 
#' The in which the headers are applied goes from top to bottom, 
#' so if you provide 3 elements for a column header, the first 
#' element is applied to the top and the second to the bottom. 
#' If one variable has three levels and other variable only have 
#' one or two, the columns with less levels to the header will bind 
#' to the bottom. So a column with two levels will apply to the 
#' second and third row, and a column with one level with apply 
#' the bottom row. Spanners are determined using cells of the same 
#' text value, where horizontal and vertical merging is performed.
#' 
#' The same result can be achieved using column labels on the 
#' input dataframe to the clintable. If labels are present,
#' header levels will be separated using the delimitter "||" within 
#' the label string. 
#' 
#' @param x A clintable object
#' @param ... Named arguments providing the column header text. 
#'   Separate levels of the header are determined using separate 
#'   elements of a character vector. 
#'
#' @return A clintable object
#'
#' @export
#' @examples
#' 
#' clintable(iris) |>
#'   clin_column_headers(
#'     Sepal.Length = c("Flowers", "Sepal", "Length"),  
#'     Sepal.Width = c("Flowers", "Sepal", "Width"),  
#'     Petal.Length = c("Petal", "Length"),  
#'     Petal.Width = c("Petal", "Width")
#'  )
#' 
clin_column_headers <- function(x, ...) {
  stopifnot(inherits(x, "clintable"))
  refdat <- x$body$dataset

  # Pull out the column widths
  args <- list(...)
  if (!all(vapply(args, is.character, TRUE))) {
    stop("All header arguments must be characters")
  }

  if (!all(names(args) %in% colnames(refdat))) {
    stop("All argument names must be columns present within the clintable columns")
  }
  
  # Get header depth
  max(vapply(args, length, 1))

  # Find how many header levels are necessary
  depth <- max(vapply(args, length, 1))

  # Create a matrix for the headers
  mheaders <- matrix(nrow=depth, ncol = ncol(refdat))
  colnames(mheaders) <- names(refdat)

  # Loop the arguments provided
  for (n in names(args)) {
    # Start at the bottom level
    i <- depth
    # Insert elements moving bottom to top
    for (h in rev(args[[n]])) {
      mheaders[i, n] <- h
      i <- i-1
    }
  }

  # Fill the characters
  mheaders <- apply(mheaders, 2, \(x) {
    # Play games with  whitespace to get cell merging to work 
    # for bottom borders
    if (all(is.na(x))) {
      x[is.na(x)] <- " "
    } else {
      x[is.na(x)] <- ""
    }
    x
  })

  typology <- as.data.frame(t(mheaders), row.names = FALSE)
  typology['col_keys'] <- colnames(refdat)

  # Apply to the clintable
  x |> 
    set_header_df(typology) |> 
    merge_v(part = "header") |>
    merge_h(part = "header")

}

#' Convert column labels into column headers
#' 
#' This function will look at the column labels, and if present
#' separate the header levels using the delimitter "||" within 
#' the label string. Header setup is done using the exported 
#' function `clin_column_headers()`. Spanners are determined 
#' using cells of the same text value, where horizontal and 
#' vertical merging is performed.
#' 
#' @param x A clintable object
#'
#' @return A clintable object
#'
#' @noRd
headers_from_labels_ <- function(x) {
  refdat <- x$body$dataset
  if (has_labels_(refdat)) {
    args <- lapply(refdat, \(x) {
      if (!is.null(attr(x, 'label'))) {
        unlist(strsplit(attr(x, 'label'),"||", fixed=TRUE))
      } else {
        ""
      }
    })
  
    args <- append(args, list(x=x), after=0)
    # Build header df from the labels
    do.call(clin_column_headers, args)
  } else {
    # Just return the object if no labels
    x
  }

}

#' Do any of the dataframe variables have labels? 
#' @noRd
has_labels_ <- function(x) {
  any(vapply(x, \(y) !is.null(attr(y, 'label')), FALSE))
}

