test_that("Errors checking", {
  # Proper target object
  expect_error(clin_column_headers(1), "inherits")

  # Proper argument types
  expect_error(clin_column_headers(clintable(iris), drat = 1), "All header arguments")


  # Proper column names
  expect_error(clin_column_headers(clintable(iris), blah = "blah"), "All argument names")

  # Header levels have to hold something
  expect_error(
    clin_column_headers(clintable(iris), Species = character(0)),
    "at least one level"
  )

  # Nothing to merge without a header
  expect_error(
    clin_column_headers(
      flextable::delete_part(clintable(iris), part = "header"),
      merge = FALSE
    ),
    "no header rows"
  )
})

test_that("merge can be adjusted without restating the header text", {
  # Labels are the other way headers get built, and until now they had no way
  # to opt out of the merging (#95)
  dat <- data.frame(a = "1", b = "2", c = "3")
  attr(dat$a, "label") <- "Spanner||Baseline"
  attr(dat$b, "label") <- "Spanner||Baseline"
  attr(dat$c, "label") <- "Other||Baseline"

  ct <- clintable(dat)
  labeled_text <- ct$header$dataset

  # Default merging collapses the shared bottom level
  expect_equal(ct$header$spans$rows[2, ], c(3, 0, 0))

  # Keeping the spanner but not the leaves
  ct2 <- clin_column_headers(ct, merge = 1)
  expect_equal(ct2$header$spans$rows[1, ], c(2, 0, 1))
  expect_equal(ct2$header$spans$rows[2, ], c(1, 1, 1))

  # The header text itself is untouched
  expect_equal(ct2$header$dataset, labeled_text)

  # Merging can be dropped entirely, and re-applied afterwards. Coming back
  # this way has to land on exactly what building the header produced
  ct3 <- clin_column_headers(ct2, merge = FALSE)
  expect_equal(ct3$header$spans$rows, matrix(1, nrow = 2, ncol = 3))
  expect_identical(clin_column_headers(ct3, merge = TRUE), ct)

  # Merging the whole header is cleared out first, including merges the user
  # applied themselves
  by_hand <- flextable::merge_at(ct3, i = 1:2, j = 1, part = "header")
  expect_equal(by_hand$header$spans$columns[, 1], c(2, 0))
  expect_equal(
    clin_column_headers(by_hand, merge = FALSE)$header$spans$columns,
    matrix(1, nrow = 2, ncol = 3)
  )

  # Only the header is ever touched. Body merges have to come through every
  # merge value intact
  bodied <- flextable::merge_at(ct, i = 1, j = 2:3, part = "body")
  expect_equal(bodied$body$spans$rows[1, ], c(1, 2, 0))

  for (m in list(TRUE, FALSE, 1, "spanners", "none")) {
    adjusted <- clin_column_headers(bodied, merge = m)
    expect_equal(adjusted$body$spans, bodied$body$spans, info = format(m))
    expect_equal(adjusted$body$dataset, bodied$body$dataset, info = format(m))
  }
})

test_that("merge argument is validated", {
  ct <- clintable(iris)
  headers <- function(...) {
    clin_column_headers(
      ct,
      Sepal.Length = c("Flowers", "Sepal", "Length"),
      Sepal.Width = c("Flowers", "Sepal", "Width"),
      Petal.Length = c("Petal", "Length"),
      Petal.Width = c("Petal", "Width"),
      Species = "",
      ...
    )
  }

  # Only keywords, logicals and numbers are row selectors
  expect_error(headers(merge = "1"), "must be TRUE, FALSE")
  expect_error(headers(merge = c("all", "none")), "must be TRUE, FALSE")
  expect_error(headers(merge = list(1)), "must be TRUE, FALSE")
  expect_error(headers(merge = factor(3)), "must be TRUE, FALSE")

  # NULL is not a stand in for the default
  expect_error(headers(merge = NULL), "cannot be NULL")

  # Missings and empties are never intentional
  expect_error(headers(merge = NA), "cannot be empty or contain missing")
  expect_error(headers(merge = c(1, NA)), "cannot be empty or contain missing")
  expect_error(headers(merge = integer(0)), "cannot be empty or contain missing")

  # Logical vectors have to line up with the header rows
  expect_error(headers(merge = c(TRUE, FALSE)), "must be length 1 or length 3")

  # Row numbers have to be usable row numbers
  expect_error(headers(merge = 1.5), "must be whole numbers")
  expect_error(headers(merge = c(-1, 2)), "cannot mix positive and negative")
  expect_error(headers(merge = 0), "cannot be 0")
  expect_error(headers(merge = 4), "must be between 1 and 3")
  expect_error(headers(merge = -4), "must be between 1 and 3")

  # `merge` follows the dots, so a near miss on the name becomes header text
  expect_error(headers(mer = FALSE), "Did you mean `merge`", fixed = TRUE)
  expect_error(headers(merges = FALSE), "Did you mean `merge`", fixed = TRUE)

  # Including when the value looks like a keyword, which lands on the unknown
  # column name error rather than the type error
  expect_error(headers(Merge = "spanners"), "Did you mean `merge`", fixed = TRUE)
  expect_error(headers(merg = "all"), "Did you mean `merge`", fixed = TRUE)

  # But a real column of the data is never a misspelling of `merge`
  expect_error(
    clin_column_headers(clintable(iris), Species = 1),
    "^All header arguments must be characters$"
  )
  merged <- data.frame(m = "1", merged = "2")
  expect_error(
    clin_column_headers(clintable(merged, use_labels = FALSE), merged = 1),
    "^All header arguments must be characters$"
  )
})

test_that("Default merge behavior is unchanged by the merge argument", {
  ct <- clintable(iris)
  headers <- function(...) {
    clin_column_headers(
      ct,
      Sepal.Length = c("Flowers", "Sepal", "Length"),
      Sepal.Width = c("Flowers", "Sepal", "Width"),
      Petal.Length = c("Petal", "Length"),
      Petal.Width = c("Petal", "Width"),
      Species = "",
      ...
    )
  }

  # Every way of saying "merge everything" has to land in the same place,
  # including the pre-existing behavior of merging the whole header part
  reference <- headers() |>
    flextable::merge_none(part = "header") |>
    flextable::merge_h(part = "header")

  expect_equal(headers(), reference)
  expect_equal(headers(merge = TRUE), reference)
  expect_equal(headers(merge = "all"), reference)
  expect_equal(headers(merge = 1:3), reference)
  expect_equal(headers(merge = c(TRUE, TRUE, TRUE)), reference)
})

test_that("merge argument controls which header rows merge", {
  ct <- clintable(iris)

  # The bottom row repeats a label across adjacent columns, which is what makes
  # the merge value observable at all
  headers <- function(...) {
    clin_column_headers(
      ct,
      Sepal.Length = c("Flowers", "Sepal", "Value"),
      Sepal.Width = c("Flowers", "Sepal", "Value"),
      Petal.Length = c("Petal", "Value"),
      Petal.Width = c("Petal", "Value"),
      Species = "",
      ...
    )
  }

  unmerged <- matrix(1, nrow = 3, ncol = 5)

  # Merging everything collapses that bottom row, so the assertions below
  # distinguish "all rows" from "all rows but the last"
  expect_equal(headers()$header$spans$rows[3, ], c(4, 0, 0, 0, 1))

  # Nothing merges
  expect_equal(headers(merge = FALSE)$header$spans$rows, unmerged)
  expect_equal(headers(merge = "none")$header$spans$rows, unmerged)

  # Only the top row merges, three equivalent ways of asking
  top_only <- unmerged
  top_only[1, ] <- c(2, 0, 2, 0, 1)
  expect_equal(headers(merge = 1)$header$spans$rows, top_only)
  expect_equal(headers(merge = -(2:3))$header$spans$rows, top_only)
  expect_equal(
    headers(merge = c(TRUE, FALSE, FALSE))$header$spans$rows,
    top_only
  )

  # Everything but the bottom row, which is how repeated leaf labels are kept
  # separate (#95)
  no_leaf <- unmerged
  no_leaf[1:2, ] <- c(2, 2, 0, 0, 2, 2, 0, 0, 1, 1)
  expect_equal(headers(merge = 1:2)$header$spans$rows, no_leaf)
  expect_equal(headers(merge = -3)$header$spans$rows, no_leaf)
  expect_equal(headers(merge = c(TRUE, TRUE, FALSE))$header$spans$rows, no_leaf)
  expect_equal(headers(merge = "spanners")$header$spans$rows, no_leaf)

  # Single level headers are still row 1
  flat <- function(...) {
    clin_column_headers(
      ct,
      Sepal.Length = "Sepal",
      Sepal.Width = "Sepal",
      Petal.Length = "Petal",
      Petal.Width = "Petal",
      Species = "Species",
      ...
    )
  }
  expect_equal(
    flat(merge = FALSE)$header$spans$rows,
    matrix(1, nrow = 1, ncol = 5)
  )

  # A single row header is all leaves, so there are no spanners to merge
  expect_equal(
    flat(merge = "spanners")$header$spans$rows,
    matrix(1, nrow = 1, ncol = 5)
  )
  expect_equal(
    flat(merge = 1)$header$spans$rows,
    matrix(c(2, 0, 2, 0, 1), nrow = 1)
  )
})

test_that("A one column table takes a multi level header", {
  # Every level asked for has to reach the table, and merging is resolved
  # against the header rows the table really ends up with
  one_col <- data.frame(a = 1:3)

  for (m in list(TRUE, FALSE, 1, -1, "all", "none", "spanners")) {
    ct <- clin_column_headers(
      clintable(one_col),
      a = c("Top", "Bottom"),
      merge = m
    )
    expect_equal(nrow(ct$header$dataset), 2)
    expect_equal(unname(unlist(ct$header$dataset)), c("Top", "Bottom"))

    # One column has nothing to merge with
    expect_equal(ct$header$spans$rows, matrix(1L, nrow = 2, ncol = 1))
    expect_equal(ct$header$spans$columns, matrix(1L, nrow = 2, ncol = 1))
  }

  # Deeper headers land the same way
  deep <- clin_column_headers(clintable(one_col), a = c("A", "B", "C"))
  expect_equal(unname(unlist(deep$header$dataset)), c("A", "B", "C"))

  # Same through the labels path
  labeled <- data.frame(a = 1:3)
  attr(labeled$a, "label") <- "Top||Bottom"
  ct <- clintable(labeled)
  expect_equal(unname(unlist(ct$header$dataset)), c("Top", "Bottom"))

  # And it still renders
  expect_no_error(clintable_as_html(ct))
  expect_no_error(write_clindoc(ct, file = withr::local_tempfile(fileext = ".docx")))

  # Row numbers are held to the header rows that are really there
  expect_error(
    clin_column_headers(clintable(one_col), a = c("Top", "Bottom"), merge = 3),
    "must be between 1 and 2"
  )

  # Asking for the same row twice is harmless. The columns left out of the call
  # come through blank, and blanks merge with each other
  ct <- clintable(iris)
  expect_equal(
    clin_column_headers(
      ct,
      Sepal.Length = c("Flowers", "Length"),
      Sepal.Width = c("Flowers", "Width"),
      merge = c(1, 1)
    )$header$spans$rows[1, ],
    c(2, 0, 3, 0, 0)
  )
})

test_that("Repeated leaf labels can be kept separate (#95)", {
  # The shape that motivated the issue - adjacent columns legitimately share
  # a bottom level label underneath their own spanners
  dat <- data.frame(
    lbl = "Row label",
    a_base = "1", a_post = "2",
    b_base = "3", b_post = "4"
  )

  ct <- clintable(dat, use_labels = FALSE) |>
    clin_column_headers(
      lbl = "",
      a_base = c("Treatment A", "Baseline"),
      a_post = c("Treatment A", "Post"),
      b_base = c("Treatment B", "Baseline"),
      b_post = c("Treatment B", "Post"),
      merge = 1
    )

  # Spanners merged on the top row, leaves untouched on the bottom
  expect_equal(
    ct$header$spans$rows,
    matrix(
      c(
        1, 2, 0, 2, 0,
        1, 1, 1, 1, 1
      ),
      nrow = 2,
      byrow = TRUE
    )
  )

  # Same layout, but now the shared leaf label really is adjacent
  ct2 <- clintable(dat, use_labels = FALSE) |>
    clin_column_headers(
      lbl = "",
      a_base = c("Treatment A", "Baseline"),
      a_post = c("Treatment A", "Baseline"),
      b_base = c("Treatment B", "Baseline"),
      b_post = c("Treatment B", "Baseline"),
      merge = 1
    )

  # All four "Baseline" cells stay separate, which is what the default
  # merge would have collapsed into one cell
  expect_equal(ct2$header$spans$rows[2, ], c(1, 1, 1, 1, 1))
  expect_equal(ct2$header$spans$rows[1, ], c(1, 2, 0, 2, 0))

  default_merge <- clintable(dat, use_labels = FALSE) |>
    clin_column_headers(
      lbl = "",
      a_base = c("Treatment A", "Baseline"),
      a_post = c("Treatment A", "Baseline"),
      b_base = c("Treatment B", "Baseline"),
      b_post = c("Treatment B", "Baseline")
    )
  expect_equal(default_merge$header$spans$rows[2, ], c(1, 4, 0, 0, 0))
})

test_that("The merge choice carries through to the rendered output", {
  # Skipping the merge also skips flextable's own merge bookkeeping, so the
  # object has to be proven renderable rather than just inspectable
  dat <- data.frame(
    lbl = "Row label",
    a_base = "1", a_post = "2",
    b_base = "3", b_post = "4"
  )

  headers <- function(...) {
    clintable(dat, use_labels = FALSE) |>
      clin_column_headers(
        lbl = "",
        a_base = c("Treatment", "Baseline"),
        a_post = c("Treatment", "Baseline"),
        b_base = c("Treatment", "Baseline"),
        b_post = c("Treatment", "Baseline"),
        ...
      )
  }

  # Word is the first class output, so count the spans officer really wrote
  grid_spans <- function(ct) {
    file <- withr::local_tempfile(fileext = ".docx")
    write_clindoc(ct, file = file)

    unzipped <- withr::local_tempdir()
    utils::unzip(file, files = "word/document.xml", exdir = unzipped)
    xml <- readLines(
      file.path(unzipped, "word", "document.xml"),
      warn = FALSE
    )
    sum(lengths(regmatches(xml, gregexpr("gridSpan", xml, fixed = TRUE))))
  }

  # Both header rows collapse by default, so Word gets one span per row
  expect_equal(grid_spans(headers()), 2)

  # Only the spanner row collapses, leaving one
  expect_equal(grid_spans(headers(merge = "spanners")), 1)

  # With merging off every header cell stands alone, so there are none at all
  expect_equal(grid_spans(headers(merge = FALSE)), 0)

  # The HTML preview path has to survive it too
  expect_no_error(clintable_as_html(headers(merge = FALSE)))
  expect_no_error(clintable_as_html(headers(merge = "spanners")))
})

test_that("Unmerged header rows survive slicing", {
  # Slicing repairs spanners split over column page breaks, and that repair
  # must not re-merge cells the user asked to keep separate
  dat <- as.data.frame(matrix(1:12, nrow = 2))
  names(dat) <- c("key", paste0("v", 1:5))

  ct <- clintable(dat, use_labels = FALSE) |>
    clin_alt_pages(
      key_cols = "key",
      # The last page holds a single column, which is where the spanner loses
      # the cell carrying its merge count
      col_groups = list(c("v1", "v2"), c("v3", "v4"), "v5")
    ) |>
    clin_column_headers(
      key = c("", "Key"),
      v1 = c("Spanner", "Baseline"),
      v2 = c("Spanner", "Baseline"),
      v3 = c("Spanner", "Baseline"),
      v4 = c("Spanner", "Baseline"),
      v5 = c("Spanner", "Baseline"),
      merge = 1
    )

  ct2 <- prep_pagination_(ct)
  pages <- ct2$clinify_config$pagination_idx
  expect_length(pages, 3)

  for (p in pages) {
    sliced <- slice_clintable(ct2, p$rows, p$cols)
    width <- length(p$cols)

    # The spanner is cut by the column break and gets repaired
    expect_equal(
      sliced$header$spans$rows[1, ],
      c(1L, width - 1L, rep(0L, width - 2L)),
      info = paste("cols:", paste(p$cols, collapse = ","))
    )
    # The bottom row was never merged, so every cell stays on its own
    expect_equal(
      sliced$header$spans$rows[2, ],
      rep(1L, width),
      info = paste("cols:", paste(p$cols, collapse = ","))
    )
  }
})

test_that("A column named merge can still be given a header", {
  dat <- data.frame(merge = "a", other = "b")

  # Through the dots the argument is claimed by the merge parameter, so the
  # user gets pointed at the way out
  # Every value is refused, so the collision can never be silently swallowed
  for (m in list("Merged", "none", "all", "spanners", 1, TRUE, FALSE, -1)) {
    expect_error(
      clin_column_headers(clintable(dat, use_labels = FALSE), merge = m),
      "cannot be used on a clintable that has a column named `merge`",
      fixed = TRUE
    )
  }

  # Leaving it out still works, and that column just goes unheaded
  expect_no_error(
    clin_column_headers(clintable(dat, use_labels = FALSE), other = "Other")
  )

  # Labels sidestep the collision entirely
  attr(dat$merge, "label") <- "Spanner||Merged"
  attr(dat$other, "label") <- "Spanner||Other"
  ct <- clintable(dat)

  expect_equal(unname(unlist(ct$header$dataset[2, ])), c("Merged", "Other"))
  expect_equal(ct$header$spans$rows[1, ], c(2, 0))
})

test_that("Headers apply as expected", {
  ct <- clintable(iris)

  ct2 <- ct |>
    clin_column_headers(
      Sepal.Length = c("Flowers", "Sepal", "Length"),
      Sepal.Width = c("Flowers", "Sepal", "Width"),
      Petal.Length = c("Petal", "Length"),
      Petal.Width = c("Petal", "Width"),
      Species = ""
    )

  # These snapshots capture the major factors of interest
  expect_snapshot(ct2$header$dataset) # Dup values applied in right spots
  expect_snapshot(ct2$header$spans) # Blank column spans don't merge with horizontals

  # Use iris
  refdat <- iris
  attr(refdat$Sepal.Length, "label") <- "Flower||Sepal||Length"
  attr(refdat$Sepal.Width, "label") <- "Flower||Sepal||Width"
  attr(refdat$Petal.Length, "label") <- "Flower||Petal||Length"
  attr(refdat$Petal.Width, "label") <- "Flower||Petal||Width"

  ct3 <- clintable(refdat)
  has_labels_(ct3$body$dataset)
  ct3 <- headers_from_labels_(ct3)
  expect_snapshot(ct3$header$dataset) # Dup values applied in right spots
  expect_snapshot(ct3$header$spans) # Blank column spans don't merge with horizontals

  # Test labels but also only use a single level
  refdat <- iris
  attr(refdat$Sepal.Length, "label") <- "Sepal Length"
  attr(refdat$Sepal.Width, "label") <- "Sepal Width"
  attr(refdat$Petal.Length, "label") <- "Petal Length"
  attr(refdat$Petal.Width, "label") <- "Petal Width"

  ct3 <- clintable(refdat)
  has_labels_(ct3$body$dataset)
  ct3 <- headers_from_labels_(ct3)
  expect_snapshot(ct3$header$dataset) # Dup values applied in right spots
  expect_snapshot(ct3$header$spans) # Blank column spans don't merge with horizontals
})

test_that("Overflowing page headers update appropriately", {
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

  ct <- clintable(dat2) |>
    clin_page_by("page") |>
    clin_group_by(c("groups1", "groups2")) |>
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
    )

  ct2 <- prep_pagination_(ct)

  pages <- ct2$clinify_config$pagination_idx
  p1_ind <- pages[[1]]
  p2_ind <- pages[[2]]
  p3_ind <- pages[[3]]

  p1 <- slice_clintable(ct2, p1_ind$rows, p1_ind$cols)
  p2 <- slice_clintable(ct2, p2_ind$rows, p2_ind$cols)
  p3 <- slice_clintable(ct2, p3_ind$rows, p3_ind$cols)

  expect_snapshot(p1$header$spans$rows)
  expect_snapshot(p1$header$dataset)
  expect_snapshot(p2$header$spans$rows)
  expect_snapshot(p2$header$dataset)
  expect_snapshot(p3$header$spans$rows)
  expect_snapshot(p3$header$dataset)
})
