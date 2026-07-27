# The bottom border of every header cell, as Word receives it. A merged run
# reaches Word as one cell carrying a gridSpan, so the run's border is repeated
# across the columns it covers - that puts the result back on the table's own
# column numbering, which is what the tables this replaces were written against
docx_header_rules <- function(ct, table = 1) {
  file <- withr::local_tempfile(fileext = ".docx")
  write_clindoc(ct, file = file)

  unzipped <- withr::local_tempdir()
  utils::unzip(file, files = "word/document.xml", exdir = unzipped)
  xml <- xml2::read_xml(file.path(unzipped, "word", "document.xml"))

  rows <- xml2::xml_find_all(
    xml2::xml_find_all(xml, "//w:tbl")[[table]],
    "./w:tr"
  )

  # Only the repeating rows are the header
  header <- Filter(
    function(row) {
      !inherits(
        xml2::xml_find_first(row, "./w:trPr/w:tblHeader"),
        "xml_missing"
      )
    },
    rows
  )

  lapply(header, function(row) {
    cells <- xml2::xml_find_all(row, "./w:tc")

    unlist(lapply(cells, function(cell) {
      span <- xml2::xml_attr(
        xml2::xml_find_first(cell, "./w:tcPr/w:gridSpan"),
        "val"
      )
      span <- if (is.na(span)) 1 else as.integer(span)

      border <- xml2::xml_find_first(cell, "./w:tcPr/w:tcBorders/w:bottom")
      pen <- if (inherits(border, "xml_missing")) {
        "none"
      } else {
        xml2::xml_attr(border, "val")
      }

      rep(pen, span)
    }))
  })
}

# A house style of the kind an organisation installs: every border cleared and
# only the one rule under the column labels drawn back. This is what the CDISC
# pilot tables use, and why they had to hand roll the spanner rule
house_style_ <- function(x, ...) {
  x <- flextable::border_remove(x)
  flextable::hline_bottom(
    x,
    part = "header",
    border = officer::fp_border(color = "black", width = 1)
  )
}

# The two arm spanner shape these tables are built around: a stub with nothing
# over it, two arms of two columns each, and a trailing p-value column that is
# not part of any arm
spanned_table_ <- function() {
  dat <- data.frame(
    stub = c("Male", "Female"),
    a_lo = c("5 (10%)", "7 (14%)"),
    a_hi = c("2 (4%)", "3 (6%)"),
    b_lo = c("6 (12%)", "8 (16%)"),
    b_hi = c("1 (2%)", "4 (8%)"),
    pval = c("0.501", "0.628")
  )

  clintable(dat, use_labels = FALSE) |>
    clin_column_headers(
      stub = "",
      a_lo = c("Drug A (N=50)", "Low"),
      a_hi = c("Drug A (N=50)", "High"),
      b_lo = c("Drug B (N=50)", "Low"),
      b_hi = c("Drug B (N=50)", "High"),
      pval = c("", "p-value")
    )
}

test_that("clin_spanner_rule records what it was given", {
  ct <- spanned_table_()

  expect_null(ct$clinify_config$spanner_rule)

  # TRUE is the 1pt solid black rule these tables conventionally use, which is
  # what the pilot wrote out as fp_border(color = "black", width = 1)
  cfg <- clin_spanner_rule(ct)$clinify_config$spanner_rule
  expect_s3_class(cfg$border, "fp_border")
  expect_equal(cfg$border$width, 1)
  expect_equal(cfg$border$style, "solid")
  expect_equal(cfg$border$color, "black")
  expect_null(cfg$rows)

  # A pen of the caller's own is kept as it stands
  dashed <- officer::fp_border(color = "grey", width = 0.5, style = "dashed")
  expect_equal(
    clin_spanner_rule(ct, border = dashed)$clinify_config$spanner_rule$border,
    dashed
  )

  # FALSE is a border that draws nothing, which is how flextable takes one away
  off <- clin_spanner_rule(ct, border = FALSE)$clinify_config$spanner_rule
  expect_equal(off$border$style, "none")
  expect_equal(off$border$width, 0)

  expect_equal(
    clin_spanner_rule(ct, rows = c(2, 1))$clinify_config$spanner_rule$rows,
    c(2L, 1L)
  )
})

test_that("clin_spanner_rule is validated", {
  ct <- spanned_table_()

  expect_error(clin_spanner_rule(mtcars), "inherits")
  expect_error(clin_spanner_rule(ct, border = "black"), "must be TRUE, FALSE")
  expect_error(clin_spanner_rule(ct, border = NA), "must be TRUE, FALSE")
  expect_error(clin_spanner_rule(ct, border = 1), "must be TRUE, FALSE")
  expect_error(clin_spanner_rule(ct, rows = 0), "whole header row numbers")
  expect_error(clin_spanner_rule(ct, rows = -1), "whole header row numbers")
  expect_error(clin_spanner_rule(ct, rows = 1.5), "whole header row numbers")
  expect_error(clin_spanner_rule(ct, rows = "1"), "whole header row numbers")
  expect_error(clin_spanner_rule(ct, rows = NA), "whole header row numbers")
  expect_error(clin_spanner_rule(ct, rows = integer(0)), "whole header row")
})

test_that("Rows are resolved against the header the table turns out to have", {
  # Which rows a header has is not settled when the verb is called - it can be
  # piped before the headers are built - so the request is resolved as the
  # table renders
  expect_equal(spanner_rule_rows_(NULL, 3), 1:2)
  expect_equal(spanner_rule_rows_(NULL, 1), integer(0))
  expect_equal(spanner_rule_rows_(2, 3), 2)

  # The bottom row is never one of them, so a two row header has one row to rule
  expect_error(spanner_rule_rows_(2, 2), "between 1 and 1")
  expect_error(spanner_rule_rows_(1, 1), "single row deep")
})

test_that("A single spanner is found and the stub is left out of it", {
  # The stub's header cell is blank, which is why the pilot had to write its
  # column numbers out by hand rather than ruling the whole row
  spans <- rbind(
    c(1, 2, 0),
    c(1, 1, 1)
  )
  text <- rbind(
    c("", "Drug A (N=50)", "Drug A (N=50)"),
    c("", "Low", "High")
  )

  expect_equal(
    spanner_runs_(spans, text, 1),
    list(list(row = 1, cols = 2:3))
  )
})

test_that("Each spanner is ruled over its own columns, and nothing else is", {
  # Two arms and a trailing p-value column, which is the shape of the pilot's
  # shift tables. The p-value column stands on its own, so it is not ruled
  a <- "Drug A (N=50)"
  b <- "Drug B (N=50)"

  spans <- rbind(
    c(1, 2, 0, 2, 0, 1),
    c(1, 1, 1, 1, 1, 1)
  )
  text <- rbind(
    c("", a, a, b, b, ""),
    c("", "Low", "High", "Low", "High", "p-value")
  )

  expect_equal(
    spanner_runs_(spans, text, 1),
    list(
      list(row = 1, cols = 2:3),
      list(row = 1, cols = 4:5)
    )
  )
})

test_that("A merged run of blank cells is not a spanner", {
  # clinify fills the header levels a column does not use, so the space over a
  # stub and a shift column merges into a run of its own. Ruling it would draw
  # a line under nothing
  spans <- rbind(
    c(2, 0, 2, 0, 1),
    c(1, 1, 1, 1, 1)
  )
  text <- rbind(
    c("", "", "Drug A (N=50)", "Drug A (N=50)", ""),
    c("", "Shift", "Normal", "High", "p-value")
  )

  expect_equal(
    spanner_runs_(spans, text, 1),
    list(list(row = 1, cols = 3:4))
  )

  # A column that has something in it lower down is padded with a space rather
  # than left empty, so that case has to be caught too
  text[1, 1:2] <- " "
  expect_equal(
    spanner_runs_(spans, text, 1),
    list(list(row = 1, cols = 3:4))
  )
})

test_that("A merged run in the bottom header row is a label, not a spanner", {
  # The bottom row holds the individual column labels, and the rule under it is
  # the full width one the styling function draws
  spans <- rbind(
    c(1, 2, 0),
    c(1, 2, 0)
  )
  text <- rbind(
    c("", "Drug A (N=50)", "Drug A (N=50)"),
    c("", "Change from Baseline", "Change from Baseline")
  )

  expect_equal(
    spanner_runs_(spans, text, spanner_rule_rows_(NULL, 2)),
    list(list(row = 1, cols = 2:3))
  )
})

test_that("Every row of a deeper header is ruled unless rows says otherwise", {
  # A three row header can carry a spanner over a spanner, and both want a rule
  spans <- rbind(
    c(1, 4, 0, 0, 0),
    c(1, 2, 0, 2, 0),
    c(1, 1, 1, 1, 1)
  )
  text <- rbind(
    c("", "Treatment", "Treatment", "Treatment", "Treatment"),
    c("", "Drug A", "Drug A", "Drug B", "Drug B"),
    c("", "Low", "High", "Low", "High")
  )

  expect_equal(
    spanner_runs_(spans, text, spanner_rule_rows_(NULL, 3)),
    list(
      list(row = 1, cols = 2:5),
      list(row = 2, cols = 2:3),
      list(row = 2, cols = 4:5)
    )
  )

  expect_equal(
    spanner_runs_(spans, text, spanner_rule_rows_(1, 3)),
    list(list(row = 1, cols = 2:5))
  )
})

test_that("A header with nothing merged has no spanners to rule", {
  # merge = "none" leaves every header cell standing alone, and an unmerged
  # cell reads as a run one column wide
  dat <- data.frame(stub = "Male", a_lo = "5", a_hi = "2")

  ct <- clintable(dat, use_labels = FALSE) |>
    clin_column_headers(
      stub = "",
      a_lo = c("Drug A (N=50)", "Low"),
      a_hi = c("Drug A (N=50)", "High"),
      merge = "none"
    )

  expect_equal(unique(as.vector(ct$header$spans$rows)), 1L)
  expect_equal(
    spanner_runs_(
      ct$header$spans$rows,
      as.matrix(ct$header$dataset),
      spanner_rule_rows_(NULL, 2)
    ),
    list()
  )

  # And a header only one row deep has no row above its bottom one at all, so
  # asking for a rule leaves the table exactly as it was
  plain <- clin_spanner_rule(clintable(head(mtcars[, 1:2], 2)))
  expect_identical(apply_spanner_rule_(plain), plain)
})

test_that("The rule survives a house style that clears the borders", {
  # This is the case the feature exists for. The pilot's own
  # clinify_table_default() opens with border_remove(), so a rule applied when
  # the verb was called would be wiped before the table reached Word
  ct <- spanned_table_()

  withr::local_options(clinify_table_default = house_style_)

  bare <- docx_header_rules(ct)
  ruled <- docx_header_rules(clin_spanner_rule(ct))

  # Nothing under the spanners to start with, only the rule under the labels
  expect_equal(bare[[1]], rep("none", 6))
  expect_equal(bare[[2]], rep("single", 6))

  # Then a rule over each arm's columns, and nowhere else
  expect_equal(
    ruled[[1]],
    c("none", "single", "single", "single", "single", "none")
  )
  expect_equal(ruled[[2]], rep("single", 6))
})

test_that("The rule survives the stock style, which draws one of its own", {
  # clinify_table_default() already rules the non blank cells of a spanner row,
  # so the interesting part here is that the verb neither doubles it up nor
  # loses it, and that FALSE is what takes it away
  ct <- spanned_table_()

  stock <- docx_header_rules(ct)
  expect_equal(
    stock[[1]],
    c("none", "single", "single", "single", "single", "none")
  )

  expect_equal(docx_header_rules(clin_spanner_rule(ct))[[1]], stock[[1]])

  # FALSE draws no rule whatever the style around it did
  expect_equal(
    docx_header_rules(clin_spanner_rule(ct, border = FALSE))[[1]],
    rep("none", 6)
  )
})

test_that("A pen of the caller's own reaches Word as it was given", {
  # The dashed variant the pilot needs. Applying it before the styling function
  # is not enough - the stock default draws its own solid rule over the top,
  # which is the second half of why this is deferred
  ct <- spanned_table_()
  dashed <- officer::fp_border(color = "black", width = 1, style = "dashed")

  by_hand <- flextable::hline(
    ct,
    i = 1,
    j = 2:5,
    border = dashed,
    part = "header"
  )
  expect_equal(
    docx_header_rules(by_hand)[[1]],
    c("none", "single", "single", "single", "single", "none")
  )

  expect_equal(
    docx_header_rules(clin_spanner_rule(ct, border = dashed))[[1]],
    c("none", "dashed", "dashed", "dashed", "dashed", "none")
  )

  # And through a house style as well
  withr::local_options(clinify_table_default = house_style_)
  expect_equal(
    docx_header_rules(clin_spanner_rule(ct, border = dashed))[[1]],
    c("none", "dashed", "dashed", "dashed", "dashed", "none")
  )
})

test_that("A spanner cut by a column page break is ruled on both halves", {
  # clin_alt_pages() can split an arm across pages, leaving part of its spanner
  # on each. The rule is drawn before the table is sliced, so each page ends up
  # with the piece of it that page carries
  dat <- data.frame(
    stub = c("Male", "Female"),
    a_lo = "1",
    a_hi = "2",
    b_lo = "3",
    b_hi = "4"
  )

  ct <- clintable(dat, use_labels = FALSE) |>
    clin_column_headers(
      stub = "",
      a_lo = c("Drug A (N=50)", "Low"),
      a_hi = c("Drug A (N=50)", "High"),
      b_lo = c("Drug B (N=50)", "Low"),
      b_hi = c("Drug B (N=50)", "High")
    ) |>
    clin_alt_pages(
      key_cols = "stub",
      col_groups = list(c("a_lo", "a_hi", "b_lo"), "b_hi")
    ) |>
    clin_spanner_rule()

  withr::local_options(clinify_table_default = house_style_)

  # Drug A whole and the first column of Drug B, with the stub left out
  expect_equal(
    docx_header_rules(ct, table = 1)[[1]],
    c("none", "single", "single", "single")
  )

  # And the rest of Drug B on its own, still without the stub
  expect_equal(
    docx_header_rules(ct, table = 2)[[1]],
    c("none", "single")
  )
})

test_that("Leaving the verb alone leaves the table alone", {
  ct <- spanned_table_()

  # The render path is untouched by a table that never asked for a rule
  styled <- getOption("clinify_table_default")(ct)
  expect_identical(apply_spanner_rule_(styled), styled)

  withr::local_options(clinify_table_default = house_style_)
  expect_equal(docx_header_rules(ct)[[1]], rep("none", 6))
})

test_that("The HTML preview survives a configured spanner rule", {
  ct <- clin_spanner_rule(spanned_table_())
  expect_no_error(clintable_as_html(ct))

  paged <- spanned_table_() |>
    clin_alt_pages(
      key_cols = "stub",
      col_groups = list(c("a_lo", "a_hi"), c("b_lo", "b_hi", "pval"))
    ) |>
    clin_spanner_rule(border = officer::fp_border(style = "dashed"))
  expect_no_error(clintable_as_html(paged))
})

test_that("A second call refines the first rather than replacing it", {
  # `border` has a default, so a later call naming only `rows` used to reset the
  # pen back to it (#119)
  base <- clintable(head(mtcars[, 1:3], 2)) |>
    clin_column_headers(
      mpg = c("Spanner", "x"),
      cyl = c("Spanner", "y"),
      disp = c("", "z")
    )

  dashed <- officer::fp_border(style = "dashed", width = 2)
  refined <- clin_spanner_rule(clin_spanner_rule(base, dashed), rows = 1)

  cfg <- refined$clinify_config$spanner_rule
  expect_equal(cfg$border$style, "dashed")
  expect_equal(cfg$border$width, 2)
  expect_equal(cfg$rows, 1L)

  # A bare first call still means "rule the spanners", and border = FALSE still
  # means do not - "no rule" is carried as a zero width pen rather than a FALSE
  bare <- clin_spanner_rule(base)$clinify_config$spanner_rule$border
  expect_gt(bare$width, 0)

  none <- clin_spanner_rule(base, border = FALSE)$clinify_config$spanner_rule$border
  expect_equal(none$width, 0)
  expect_equal(none$style, "none")
})
