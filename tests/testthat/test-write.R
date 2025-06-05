test_that("Single table write", {
  temp_file1 <- withr::local_tempfile(fileext = ".docx")
  temp_file2 <- withr::local_tempfile(fileext = ".docx")

  ct <- clintable(mtcars) |>
    clin_auto_page("gear")
  doc <- clindoc(ct)

  write_clintable(ct, file = temp_file2)
  x1 <- officer::read_docx(temp_file2)

  write_clintable(doc, file = temp_file2)
  x2 <- officer::read_docx(temp_file2)

  expect_snapshot(x1)
  expect_snapshot(x2)
})

test_that("Alt pages write", {
  temp_file <- withr::local_tempfile(fileext = ".docx")

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
    clin_column_headers(
      mpg = "Miles/(US) gallon",
      cyl = c("Number of cylinders"),
      disp = c("Displacement\n(cu.in.)"),
      hp = c("Gross horsepower"),
      drat = c("Span multiple pages", "Rear axle ratio"),
      wt = c("Span multiple pages", "Weight (1000 lbs)"),
      qsec = c("Span multiple pages", "1/4 mile time"),
      vs = c("Span multiple pages", "Engine\n(0 = V-shaped, 1 = straight)"),
      am = c("Span multiple pages", "Transmission\n(0 = automatic, 1 = manual)"),
      gear = c("Some Spanner", "Number of forward gears"),
      carb = c("Some Spanner", "Number of carburetors")
    ) |>
    clin_col_widths(mpg = .2, cyl = .2, disp = .15, vs = .15) |>
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
    ) |>
    clin_add_footnote_page(
      list(
        c("One very long footnote full of text"),
        c("Two very long footnote full of text"),
        c("Three very long footnote full of text"),
        c("Four very long footnote full of text"),
        c("Five very long footnote full of text")
      )
    )

  write_clintable(ct, file = temp_file)
  x <- officer::read_docx(temp_file)

  expect_snapshot(x)
})
