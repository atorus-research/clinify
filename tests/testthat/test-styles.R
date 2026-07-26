test_that("Error messages", {
  op <- options(clinify_docx_default = NULL)

  expect_error(
    clin_default_table_width(),
    "clin_default_table_width"
  )

  options(op)
})

test_that("Styles apply", {
  sect <- clinify_docx_default()

  # Save out options to grab defaults
  op <- options(
    clinify_docx_default = sect,
    clinify_titles_default = clinify_titles_default,
    clinify_footnotes_default = clinify_footnotes_default,
    clinify_table_default = clinify_table_default,
    clinify_caption_default = clinify_caption_default,
    clinify_grouplabel_default = clinify_grouplabel_default
  )

  dat <- mtcars
  dat["page"] <- c(
    rep(1, 10),
    rep(2, 10),
    rep(3, 10),
    c(4, 4)
  )
  dat2 <- rbind(dat, dat)
  dat2["groups1"] <- c(
    rep("a", 32),
    rep("b", 32)
  )
  dat2["groups2"] <- c(
    rep("1", 16),
    rep("2", 16),
    rep("1", 16),
    rep("2", 16)
  )
  dat2["captions"] <- c(
    rep("Caption 1", 16),
    rep("Caption 2", 16),
    rep("Caption 3", 16),
    rep("Caption 4", 16)
  )

  ct <- clintable(dat2) |>
    clin_page_by("page") |>
    clin_group_by(c("groups1", "groups2"), caption_by = "captions") |>
    clin_alt_pages(
      key_cols = c("mpg", "cyl", "hp"),
      col_groups = list(
        c("disp", "drat", "wt"),
        c("qsec", "vs", "am"),
        c("gear", "carb")
      )
    ) |>
    clin_add_titles(
      list(
        c("Left", "Right"),
        c("Just the middle")
      )
    ) |>
    clin_add_footnotes(
      list(
        c(
          "Here's a footnote.",
          "10:55 Wednesday, March 26, 2025"
        )
      )
    )

  html_out <- knit_print.clintable(ct)

  expect_snapshot(gsub("cl-[0-9a-f]{8}", "x", as.character(html_out)))
})

test_that("Default styling leaves the caller's table properties alone", {
  # Fixing the layout used to go through set_table_properties(), which rebuilds
  # the whole property list, so anything the user had set was quietly replaced
  # with a flextable default (#98)
  ct <- clintable(head(mtcars[, 1:3], 3)) |>
    flextable::set_table_properties(
      align = "left",
      width = 0.5,
      word_title = "A table",
      word_description = "For screen readers"
    )

  for (styler in list(
    clinify_table_default,
    clinify_titles_default,
    clinify_footnotes_default
  )) {
    styled <- styler(ct)

    # The layout is the only thing clinify is after
    expect_equal(styled$properties$layout, "fixed")

    # Everything else survives
    expect_equal(styled$properties$align, "left")
    expect_equal(styled$properties$width, 0.5)
    expect_equal(styled$properties$word_title, "A table")
    expect_equal(styled$properties$word_description, "For screen readers")
  }

  # A table that says nothing about its properties still comes out on
  # flextable's defaults
  plain <- clinify_table_default(clintable(head(mtcars[, 1:3], 3)))
  expect_equal(
    plain$properties$align,
    flextable::get_flextable_defaults()$table_align
  )
  expect_equal(plain$properties$layout, "fixed")
})

test_that("Table alignment reaches the Word document", {
  spans <- function(align) {
    ct <- clintable(head(mtcars[, 1:3], 3)) |>
      flextable::set_table_properties(align = align)

    file <- withr::local_tempfile(fileext = ".docx")
    write_clindoc(ct, file = file)

    unzipped <- withr::local_tempdir()
    utils::unzip(file, files = "word/document.xml", exdir = unzipped)
    xml <- paste(
      readLines(file.path(unzipped, "word", "document.xml"), warn = FALSE),
      collapse = ""
    )

    # The table's own justification is the one inside tblPr
    tbl_pr <- regmatches(xml, regexpr("<w:tblPr>.*?</w:tblPr>", xml))
    regmatches(tbl_pr, regexpr('w:val="[a-z]+"', tbl_pr))
  }

  # Word writes "start" for a left aligned table
  expect_equal(spans("left"), 'w:val="start"')
  expect_equal(spans("center"), 'w:val="center"')
})
