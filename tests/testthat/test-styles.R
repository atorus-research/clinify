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
    clinify_caption_default = clinify_caption_default
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
        c("Left", "Center", "Right"),
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
