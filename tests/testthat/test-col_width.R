ct1 <- clintable(mtcars)
ct2 <- ct <- clintable(mtcars) |>
  clin_alt_pages(
    key_cols = c("mpg", "cyl", "hp"),
    col_groups = list(
      c("disp", "drat", "wt"),
      c("qsec", "vs", "am"),
      c("gear", "carb")
    )
  )

test_that("col_width argument checks", {
  # Bad input
  expect_error(clin_col_widths(1), "inherits")

  # Invalid type
  expect_error(
    clin_col_widths(ct1, drat = "a"),
    "All width arguments"
  )

  # Column doesn't exist
  expect_error(
    clin_col_widths(ct1, blah = .2),
    "The following columns"
  )

  # Single argument >1
  expect_error(
    clin_col_widths(ct1, drat = 2),
    "Width arguments"
  )

  # Args sum to >1
  expect_error(
    clin_col_widths(ct1, drat = .6, gear = .5),
    "Columns widths"
  )

  # Alternating args sum to >1
  expect_error(
    clin_col_widths(ct2, mpg = .6, gear = .5),
    "Key columns"
  )
})

test_that("Widths set properly - standard table", {
  ct3 <- clin_col_widths(ct1, mpg = .1, disp = .2)
  colwidths <- ct3$body$colwidths
  expect_equal(sum(colwidths), unname(clin_default_table_width()))
  expect_equal(colwidths[which(names(mtcars) == "mpg")], unname(.1 * clin_default_table_width()))
  expect_equal(colwidths[which(names(mtcars) == "disp")], unname(.2 * clin_default_table_width()))
})

test_that("Widths set properly - standard table", {
  ct3 <- clin_col_widths(ct2, mpg = .1, disp = .2)
  colwidths <- ct3$body$colwidths

  expect_equal(colwidths[which(names(mtcars) == "mpg")], unname(.1 * clin_default_table_width()))
  expect_equal(colwidths[which(names(mtcars) == "disp")], unname(.2 * clin_default_table_width()))

  v1 <- which((names(mtcars) %in% ct3$clinify_config$key_cols) | (names(mtcars) %in% ct3$clinify_config$col_groups[[1]]))
  v2 <- which((names(mtcars) %in% ct3$clinify_config$key_cols) | (names(mtcars) %in% ct3$clinify_config$col_groups[[2]]))
  v3 <- which((names(mtcars) %in% ct3$clinify_config$key_cols) | (names(mtcars) %in% ct3$clinify_config$col_groups[[3]]))
  expect_equal(sum(ct3$body$colwidths[v1]), unname(clin_default_table_width()))
  expect_equal(sum(ct3$body$colwidths[v2]), unname(clin_default_table_width()))
  expect_equal(sum(ct3$body$colwidths[v3]), unname(clin_default_table_width()))
})

test_that("clin_table_align records and applies page alignment", {
  ct <- clintable(head(mtcars[, 1:3], 3))

  expect_null(ct$clinify_config$table_align)
  expect_equal(clin_table_align(ct, "left")$clinify_config$table_align, "left")

  expect_error(clin_table_align(ct, "middle"), "should be one of")
  expect_error(clin_table_align(mtcars, "left"), "inherits")

  # It has to win over a styling function that rebuilds the table properties,
  # which is what an organisation's own clinify_table_default() may well do
  clobbering <- function(x, ...) {
    flextable::set_table_properties(x, layout = "fixed")
  }
  withr::local_options(clinify_table_default = clobbering)

  aligned <- clin_table_align(ct, "left")

  file <- withr::local_tempfile(fileext = ".docx")
  write_clindoc(aligned, file = file)

  unzipped <- withr::local_tempdir()
  utils::unzip(file, files = "word/document.xml", exdir = unzipped)
  xml <- paste(
    readLines(file.path(unzipped, "word", "document.xml"), warn = FALSE),
    collapse = ""
  )
  tbl_pr <- regmatches(xml, regexpr("<w:tblPr>.*?</w:tblPr>", xml))

  # Word spells a left aligned table "start"
  expect_match(tbl_pr, 'w:val="start"')

  # And the HTML preview agrees
  expect_match(
    paste(clintable_as_html(aligned), collapse = ""),
    "tabwid_left"
  )
})
