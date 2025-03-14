test_that("Single table write", {
  temp_file <- withr::local_tempfile(fileext = ".docx")

  ct <- clintable(mtcars)

  write_clintable(ct, file = temp_file)

  x <- officer::read_docx(temp_file)

  expect_snapshot(x)
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
    rep("a", 32)
  )

  dat2["groups2"] <- c(
    rep("1", 16),
    rep("2", 16)
  )

  # Create a basic table
  ct <- clintable(dat2,
    page_by = "page",
    group_by = c("groups1", "groups2")
  ) |>
    clin_alt_pages(
      key_cols = c("mpg", "cyl", "hp"),
      col_groups = list(
        c("disp", "drat", "wt"),
        c("qsec", "vs", "am"),
        c("gear", "carb")
      )
    ) |>
    clin_col_widths(mpg = .2, cyl = .2, disp = .15, vs = .15) |>
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
          format("x")
        )
      )
    )

  write_clintable(ct, file = temp_file)
  x <- officer::read_docx(temp_file)

  expect_snapshot(x)
})
