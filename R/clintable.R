#' Convert a flextable into a clintable object
#'
#' @details
#' There is no `coerce_character` argument here, unlike [clintable()].
#' A flextable arrives with its cell text already rendered, so the numeric
#' formatting this argument exists to avoid has already happened and coercing
#' the source data is no longer an option. The nearest equivalent is
#' `flextable::set_formatter(x, values = as.character)` before calling
#' `as_clintable()`, which rewrites every body cell from the stored data.
#' That is not the same operation: it replaces cell *content*, so any
#' chunk level work already done on the body - `flextable::compose()`,
#' `flextable::colformat_*()`, images, hyperlinks, equations - is discarded,
#' and columns keep the right alignment flextable gave them for being numeric.
#' Because that trade cannot be made safely on the user's behalf, it is left
#' to the caller. To get the coercion without the trade, build with
#' `clintable(x, coerce_character = TRUE)` instead.
#'
#' @param x A flextable object
#' @param page_by A variable in the input dataframe to use for pagination
#' @param group_by A variable which will be used for grouping and attached
#'   as a label above the table headers
#'
#' @return A clintable object
#' @export
#'
#' @examples
#'
#' ft <- flextable::flextable(mtcars)
#' as_clintable(ft)
#'
as_clintable <- function(x, page_by = NULL, group_by = NULL) {
  stopifnot(inherits(x, "flextable"))

  x$clinify_config$page_by <- page_by
  x$clinify_config$group_by <- group_by

  if (is.null(page_by) & is.null(group_by)) {
    x$clinify_config$pagination_method <- "default"
  } else {
    x$clinify_config$pagination_method <- "custom"
  }

  # It helps to have padding settings in here so they can be overriden
  # both other functions like clin_group_pad()
  # Setup the cell padding for table body.
  x <- flextable::padding(
    x,
    part = "body",
    padding.bottom = 0.1,
    padding.top = 0.1
  )
  x <- default_header_pad_(x)

  class(x) <- c("clintable", "flextable")
  x
}

#' Create a new clintable object
#'
#' A clintable object directly inherits from a flextable object. This function
#' will pass all necessary parameters `flextable::flextable()` and conver the
#' object to a `clintable`
#'
#' @param x A data frame
#' @param page_by A variable in the input dataframe to use for pagination
#' @param group_by A character vector of variable names which will be used for grouping and attached
#'   as a label above the table headers
#' @param use_labels Use variable labels as column headers. Nested levels can be
#'   achieved using the string "||" as a delimitter. Horizontally adjacent cells
#'   using identical words will be merged, which can be adjusted afterwards using
#'   the `merge` argument of `clin_column_headers()`.
#' @param coerce_character Coerce every column of `x` to character before the
#'   flextable is built, so pre-formatted values render exactly as supplied.
#'   Defaults to `FALSE`, which leaves flextable's numeric formatting in place.
#' @param ... Parameters to pass to `flextable::flextable()`
#'
#' @details
#' # Rendering values verbatim
#'
#' flextable bakes cell text in when the table is built, and it formats a
#' `double` column as a whole with `format(x, trim = TRUE, scientific = FALSE,
#' big.mark = ",")`. Because that decision is column wide, a clinical summary
#' column holding a count in one row and a statistic in another - necessarily a
#' `double` - is reformatted against its neighbours: `c(86, 75.2)` renders the
#' count as `"86.0"`, `c(1234, 12.5)` renders it as `"1,234.0"`, and
#' `c(1234567.891, 2)` is rounded to seven significant digits as
#' `"1,234,568"`. Values that were already formatted upstream are therefore
#' silently changed, and nothing errors to say so.
#'
#' `coerce_character = TRUE` runs `as.character()` over every column first, so
#' each value carries into the table as its own string and no column wide
#' decision is made. It replaces the `lapply(x, as.character)` line that
#' otherwise has to be written ahead of every table. Column `label` attributes
#' survive the coercion, so `use_labels` still finds them. Factors coerce to
#' their levels rather than their integer codes.
#'
#' Two side effects are worth knowing about. Numeric columns lose the right
#' alignment flextable's default theme gives them, since alignment follows
#' column type; use `clin_table_align()` or `flextable::align()` to put it
#' back. And flextable's formula selectors compare against the coerced values,
#' so `bold(i = ~ n > 5)` becomes a string comparison and quietly selects
#' different rows.
#'
#' # NA is left as NA
#'
#' `as.character(NA)` is `NA_character_`, and flextable's default `na_str` is
#' `""`, so an `NA` still renders as a blank cell. `NA` is deliberately not
#' replaced with `""`, which is safe in a body column but changes the meaning
#' of a pagination variable.
#'
#' `clin_page_by()` splits where the page variable changes, as does
#' `clin_group_by()` by default, and that comparison is `x != lag(x)`. It is
#' `NA` wherever either side is `NA`, and those rows are dropped rather than
#' treated as splits. So a `page_by`, `group_by`, or `caption_by` column that
#' is padded - carrying its value only on the first row of each block, `NA`
#' below - collapses to a single page with no group label. A variable used that
#' way needs `clin_group_by(when = "notempty")`, which tests against `""` and
#' handles `NA` just as well, and `clin_page_by()` offers no such option so its
#' page variable has to carry a value on every row.
#'
#' Padding and a change comparison do not go together whichever the pad is, but
#' they fail differently, and the `NA` failure is the quieter one: `""` padding
#' makes each padded row look like a change and splits on every one of them,
#' which is hard to miss, where `NA` padding drops the splits and leaves a
#' plausible looking single page.
#'
#' @return A clintable object
#' @export
#'
#' @examples
#' clintable(mtcars)
#'
#' # A summary column holding a count and a mean is a double, so flextable
#' # would render the count 86 as "86.0". Coercion keeps it as written.
#' summary_dat <- data.frame(
#'   row_label = c("n", "Mean"),
#'   trt_a = c(86, 75.2)
#' )
#' clintable(summary_dat, coerce_character = TRUE)
clintable <- function(
  x,
  page_by = NULL,
  group_by = NULL,
  use_labels = TRUE,
  coerce_character = FALSE,
  ...
) {
  if (!isTRUE(coerce_character) && !isFALSE(coerce_character)) {
    stop("`coerce_character` must be either TRUE or FALSE.")
  }

  if (coerce_character) {
    x <- coerce_character_(x)
  }

  ct <- as_clintable(
    # flextable reads column labels of its own accord, and would otherwise put
    # the raw label string - "||" delimiter and all - into the header even when
    # the caller asked for labels to be left alone
    flextable::flextable(x, use_labels = use_labels, ...),
    page_by = page_by,
    group_by = group_by
  )

  if (use_labels) {
    ct <- headers_from_labels_(ct)
  }
  ct
}

#' Coerce every column of a dataframe to character, keeping column labels
#'
#' The plain `x[] <- lapply(x, as.character)` drops the `label` attribute along
#' with everything else, which silently collapses label driven column headers
#' to bare column names. Labels are read back on afterwards.
#'
#' The label is fetched with `exact = TRUE` so that a column carrying haven's
#' `labels` attribute (value labels) is not partially matched as `label`.
#'
#' @param x A dataframe
#'
#' @return The dataframe with every column a character vector
#'
#' @noRd
coerce_character_ <- function(x) {
  x[] <- lapply(x, \(col) {
    label <- attr(col, "label", exact = TRUE)
    col <- as.character(col)
    if (!is.null(label)) {
      attr(col, "label") <- label
    }
    col
  })
  x
}

#' The spacing clinify starts a header block with
#'
#' Setting the column headers rebuilds the header part from scratch, which
#' drops the padding put on it at construction, so this is applied again
#' afterwards rather than only once. Being a starting point it is set before
#' anything the caller does, so a later `flextable::padding()` or
#' [clin_header_pad()] still wins.
#'
#' @param x A clintable object
#'
#' @return A clintable object
#'
#' @noRd
default_header_pad_ <- function(x) {
  if (flextable::nrow_part(x, part = "header") < 1) {
    return(x)
  }

  x <- flextable::padding(x, i = 1, part = "header", padding.top = 9)

  flextable::padding(
    x,
    i = flextable::nrow_part(x, part = "header"),
    part = "header",
    padding.bottom = 9
  )
}
