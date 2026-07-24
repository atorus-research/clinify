library(Tplyr)
suppressPackageStartupMessages(library(dplyr))
library(flextable)

# Prep a dataframe to style
t <- tplyr_table(tplyr_adae, TRTA) %>%
  set_pop_data(tplyr_adsl) %>%
  set_pop_treat_var(TRT01A) %>%
  add_layer(
    group_count("All subjects")
  ) %>%
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD)) %>%
      set_nest_count(TRUE)
  )

example_df <- t %>% Tplyr::build()

example_df <- example_df[1:4]

test_that("A subset table produces as expected", {
  typology <- data.frame(
    col_keys = names(example_df),
    top = c("", "", "Xanomeline", "Xanomeline"),
    bottom = c("", "Placebo\n(N=86)", "High Dose\n(N=84)", "Low Dose\n(N=84)"),
    stringsAsFactors = FALSE
  )

  t_1 <- flextable(example_df, col_keys = names(example_df))
  t_1 <- set_header_df(t_1, mapping = typology, key = "col_keys")
  t_1 <- merge_at(t_1, i = 1, j = 3:4, part = "header")
  t_1 <- font(t_1, fontname = "COURIER NEW", part = "all")
  t_1 <- autofit(fit_to_width(t_1, 10))
  t_1 <- align(
    t_1,
    j = 2:4,
    align = "center",
    part = "all"
  )
  t_1 <- align(
    t_1,
    j = 2:4,
    align = "center",
    part = "header"
  )

  # Test table 2
  example_df_sub <- example_df[1:5, c(1, 3, 4)]

  typology2 <- data.frame(
    col_keys = names(example_df),
    top = c("", "", "Xanomeline", "Xanomeline"),
    bottom = c("", "Placebo\n(N=86)", "High Dose\n(N=84)", "Low Dose\n(N=84)"),
    stringsAsFactors = FALSE
  )[c(1, 3, 4), ]

  t_2 <- flextable(example_df_sub, col_keys = names(example_df_sub))
  t_2 <- set_header_df(t_2, mapping = typology2, key = "col_keys")
  t_2 <- merge_at(t_2, i = 1, j = 2:3, part = "header")
  t_2 <- font(t_2, fontname = "COURIER NEW", part = "all")
  t_2 <- autofit(fit_to_width(t_2, 10))
  t_2 <- align(
    t_2,
    j = 2:3,
    align = "center",
    part = "all"
  )

  test_table <- t_1
  base_table <- t_2
  comp_table <- slice_clintable(t_1, 1:5, c(1, 3:4))

  # flextable::autofit() derives rowheights and colwidths from font-metric
  # estimates, which vary across platforms (e.g. one body row's height differs
  # on r-devel-linux-x86_64-fedora-gcc) and differ between the 3-column subset
  # and the 4-column source table anyway. slice_clintable() carries the parent
  # table's dimensions forward verbatim, so compare the structural content of
  # each table part while ignoring those non-portable cosmetic dimensions.
  expect_equal_tabpart <- function(object, expected) {
    object$rowheights <- expected$rowheights <- NULL
    object$colwidths <- expected$colwidths <- NULL
    testthat::expect_equal(object, expected)
  }

  # Check element by element because there's a lot going on here
  expect_equal_tabpart(base_table$header, comp_table$header)
  testthat::expect_equal(base_table$blanks, comp_table$blanks)
  testthat::expect_equal(base_table$caption, comp_table$caption)
  testthat::expect_equal(base_table$col_keys, comp_table$col_keys)
  expect_equal_tabpart(base_table$footer, comp_table$footer)
  testthat::expect_equal(base_table$properties, comp_table$properties)
  expect_equal_tabpart(base_table$body, comp_table$body)
})

test_that("Basic table subset works", {
  x <- flextable(mtcars)
  # Specific slicing because of the formatting of character strings,
  # which isn't my top priority here.
  y <- slice_clintable(x, 20:32, c(1:3, 5:8))
  z <- flextable(mtcars[20:32, c(1:3, 5:8)])

  # Check element by element because there's a lot going on here
  testthat::expect_equal(z$header, y$header)
  testthat::expect_equal(z$blanks, y$blanks)
  testthat::expect_equal(z$caption, y$caption)
  testthat::expect_equal(z$col_keys, y$col_keys)
  testthat::expect_equal(z$footer, y$footer)
  testthat::expect_equal(z$properties, y$properties)
  # This fails because as.character() sees integers and isn't adding decimals the same
  testthat::expect_equal(z$body, y$body, ignore_attr = TRUE)
})

test_that("Subsetting merges keeps the merges the subset should have", {
  # Two blank cells merged together, then a spanner over four
  v <- c(2L, 0L, 4L, 0L, 0L, 0L)

  # Keeping everything changes nothing
  expect_equal(slice_span_vec(v, 1:6), v)

  # A merge that loses some of its cells narrows to what is left
  expect_equal(slice_span_vec(v, 1:3), c(2L, 0L, 1L))
  expect_equal(slice_span_vec(v, 1:4), c(2L, 0L, 2L, 0L))

  # A merge that loses the cell carrying its count keeps the count on the
  # first cell that survived, rather than leaving it stranded
  expect_equal(slice_span_vec(v, 3:6), c(4L, 0L, 0L, 0L))
  expect_equal(slice_span_vec(v, 4:6), c(3L, 0L, 0L))
  expect_equal(slice_span_vec(v, c(2, 4)), c(1L, 1L))

  # Cells of one merge come back together when what sat between them is gone
  expect_equal(slice_span_vec(v, c(1, 2, 5, 6)), c(2L, 0L, 2L, 0L))

  # Down to a single cell there is nothing left to merge
  expect_equal(slice_span_vec(v, 6), 1L)
  expect_equal(slice_span_vec(v, 1), 1L)

  # Nothing to keep
  expect_equal(slice_span_vec(v, integer(0)), integer(0))

  # Cells that were never merged are never merged for us, whatever they hold.
  # This is what keeps clin_column_headers(merge = ) from being undone by
  # pagination
  expect_equal(slice_span_vec(rep(1L, 6), c(1, 2, 3)), rep(1L, 3))
  expect_equal(slice_span_vec(rep(1L, 6), c(1, 4, 6)), rep(1L, 3))
})

test_that("Subset merges are always well formed", {
  # A merge count has to be followed by exactly that many fewer cells, and the
  # counts have to add up to the width of the table
  well_formed <- function(v) {
    i <- 1L
    while (i <= length(v)) {
      if (is.na(v[i]) || v[i] < 1 || i + v[i] - 1L > length(v)) {
        return(FALSE)
      }
      if (v[i] > 1L && any(v[(i + 1L):(i + v[i] - 1L)] != 0)) {
        return(FALSE)
      }
      i <- i + v[i]
    }
    TRUE
  }

  # Every arrangement of merges across a row, for every width up to 5
  span_rows <- function(n) {
    if (n == 0) {
      return(list(integer(0)))
    }
    out <- list()
    for (first in 1:n) {
      for (rest in span_rows(n - first)) {
        out[[length(out) + 1]] <- as.integer(c(first, rep(0L, first - 1L), rest))
      }
    }
    out
  }

  checked <- 0
  for (n in 1:5) {
    for (v in span_rows(n)) {
      for (mask in 1:(2^n - 1)) {
        keep <- which(bitwAnd(mask, 2^(seq_len(n) - 1)) > 0)
        out <- slice_span_vec(v, keep)
        checked <- checked + 1
        expect_length(out, length(keep))
        expect_true(
          well_formed(out),
          label = paste0(
            "spans c(", paste(v, collapse = ","), ") kept ",
            paste(keep, collapse = ","), " gave c(",
            paste(out, collapse = ","), ")"
          )
        )
      }
    }
  }
  expect_gt(checked, 500)
})

test_that("Both directions of merging are subset", {
  spans <- list(
    # rows 1 and 2 each carry a spanner over the first two columns
    rows = matrix(
      c(
        2L, 0L, 1L,
        2L, 0L, 1L,
        1L, 1L, 1L
      ),
      nrow = 3,
      byrow = TRUE
    ),
    # the last column is merged down its first two rows
    columns = matrix(
      c(
        1L, 1L, 2L,
        1L, 1L, 0L,
        1L, 1L, 1L
      ),
      nrow = 3,
      byrow = TRUE
    )
  )

  # Dropping a column narrows the horizontal merges, on every row
  kept_cols <- slice_spans(spans, 1:3, c(1, 3))
  expect_equal(kept_cols$rows[1, ], c(1L, 1L))
  expect_equal(kept_cols$rows[2, ], c(1L, 1L))
  expect_equal(kept_cols$rows[3, ], c(1L, 1L))

  # Dropping a row narrows the vertical merges
  kept_rows <- slice_spans(spans, c(1, 3), 1:3)
  expect_equal(kept_rows$columns[, 3], c(1L, 1L))
  expect_equal(kept_rows$rows[1, ], c(2L, 0L, 1L))

  # Keeping everything is a no op
  whole <- slice_spans(spans, 1:3, 1:3)
  expect_equal(whole$rows, spans$rows)
  expect_equal(whole$columns, spans$columns)
})

test_that("Every header row is repaired over a column page break", {
  # Both header rows carry a spanner that the column break cuts through
  dat <- as.data.frame(matrix(as.character(1:10), nrow = 2))
  names(dat) <- c("key", paste0("v", 1:4))

  ct <- clintable(dat, use_labels = FALSE) |>
    clin_alt_pages(
      key_cols = "key",
      col_groups = list(c("v1", "v2"), c("v3", "v4"))
    ) |>
    clin_column_headers(
      key = c("", "Key"),
      v1 = c("Top", "Second"),
      v2 = c("Top", "Second"),
      v3 = c("Top", "Second"),
      v4 = c("Top", "Second")
    )

  expect_equal(ct$header$spans$rows[1, ], c(1L, 4L, 0L, 0L, 0L))
  expect_equal(ct$header$spans$rows[2, ], c(1L, 4L, 0L, 0L, 0L))

  ct2 <- prep_pagination_(ct)

  for (p in ct2$clinify_config$pagination_idx) {
    sliced <- slice_clintable(ct2, p$rows, p$cols)

    # The lower header row used to be left holding a spanner four wide on a
    # three column page
    expect_equal(sliced$header$spans$rows[1, ], c(1L, 2L, 0L))
    expect_equal(sliced$header$spans$rows[2, ], c(1L, 2L, 0L))
  }
})

test_that("A page ending in a cut spanner slices cleanly", {
  # The last page holds one column of a spanner that started on an earlier
  # page, so the cell carrying the merge count is gone
  dat <- as.data.frame(matrix(as.character(1:8), nrow = 2))
  names(dat) <- c("key", paste0("v", 1:3))

  ct <- clintable(dat, use_labels = FALSE) |>
    clin_alt_pages(
      key_cols = "key",
      col_groups = list(c("v1", "v2"), "v3")
    ) |>
    clin_column_headers(
      key = c("", "Key"),
      v1 = c("Spanner", "a"),
      v2 = c("Spanner", "b"),
      v3 = c("Spanner", "c")
    )

  ct2 <- prep_pagination_(ct)
  pages <- ct2$clinify_config$pagination_idx

  # This used to error with "missing value where TRUE/FALSE needed"
  slices <- lapply(pages, \(p) slice_clintable(ct2, p$rows, p$cols))

  expect_equal(slices[[1]]$header$spans$rows[1, ], c(1L, 2L, 0L))
  expect_equal(slices[[2]]$header$spans$rows[1, ], c(1L, 1L))

  # And the spanner text is still on the page
  expect_equal(
    unname(unlist(slices[[2]]$header$dataset[1, ])),
    c("", "Spanner")
  )
})

test_that("Body merges survive a column page break", {
  dat <- as.data.frame(matrix(as.character(1:16), nrow = 4))
  names(dat) <- c("key", paste0("v", 1:3))

  ct <- clintable(dat, use_labels = FALSE) |>
    clin_alt_pages(
      key_cols = "key",
      col_groups = list(c("v1", "v2"), "v3")
    )

  # A full width section row, the way a section header is usually done, plus a
  # merge down a column
  ct <- flextable::merge_at(ct, i = 1, j = 1:4, part = "body")
  ct <- flextable::merge_at(ct, i = 2:3, j = 1, part = "body")

  ct2 <- prep_pagination_(ct)

  for (p in ct2$clinify_config$pagination_idx) {
    sliced <- slice_clintable(ct2, p$rows, p$cols)
    width <- length(p$cols)

    # The section row used to keep a merge four cells wide on a narrower page
    expect_equal(sliced$body$spans$rows[1, ], c(width, rep(0L, width - 1L)))

    # And the vertical merge is untouched by a column subset
    expect_equal(sliced$body$spans$columns[, 1], c(1L, 2L, 0L, 1L))
  }
})

test_that("Sliced pages always hold well formed merges", {
  # Whatever the shape of the pagination, every page has to come out with
  # merges that add up to the page it is on
  well_formed <- function(v, width) {
    if (sum(v) != width) {
      return(FALSE)
    }
    i <- 1L
    while (i <= length(v)) {
      if (is.na(v[i]) || v[i] < 1 || i + v[i] - 1L > length(v)) {
        return(FALSE)
      }
      if (v[i] > 1L && any(v[(i + 1L):(i + v[i] - 1L)] != 0)) {
        return(FALSE)
      }
      i <- i + v[i]
    }
    TRUE
  }

  check_part <- function(part, label) {
    for (i in seq_len(nrow(part$spans$rows))) {
      expect_true(
        well_formed(part$spans$rows[i, ], ncol(part$dataset)),
        label = paste(label, "row", i)
      )
    }
    for (j in seq_len(ncol(part$spans$columns))) {
      expect_true(
        well_formed(part$spans$columns[, j], nrow(part$dataset)),
        label = paste(label, "column", j)
      )
    }
  }

  dat <- as.data.frame(matrix(as.character(1:48), nrow = 8))
  names(dat) <- c("key", paste0("v", 1:5))
  dat$GRP <- rep(c("a", "b"), each = 4)

  # A spanner on every header row, a full width body row, and a merge running
  # down a column - one of each way a merge can be cut
  build <- function(col_groups) {
    ct <- clintable(dat, use_labels = FALSE) |>
      clin_group_by("GRP") |>
      clin_alt_pages(key_cols = "key", col_groups = col_groups) |>
      clin_column_headers(
        key = c("", "", "Key"),
        v1 = c("Top", "Middle", "leaf"),
        v2 = c("Top", "Middle", "leaf"),
        v3 = c("Top", "Middle", "leaf"),
        v4 = c("Top", "Middle", "leaf"),
        v5 = c("Top", "Middle", "leaf"),
        GRP = c("", "", "")
      )
    ct <- flextable::merge_at(ct, i = 1, j = 1:6, part = "body")
    flextable::merge_at(ct, i = 2:4, j = 1, part = "body")
  }

  col_group_shapes <- list(
    list(c("v1", "v2"), c("v3", "v4", "v5")),
    list("v1", "v2", "v3", "v4", "v5"),
    list(c("v1", "v2", "v3", "v4"), "v5"),
    list(c("v1", "v2", "v3", "v4", "v5"))
  )

  for (shape in col_group_shapes) {
    ct <- prep_pagination_(build(shape))
    label <- paste0("[", length(shape), " column groups]")

    for (p in ct$clinify_config$pagination_idx) {
      sliced <- slice_clintable(ct, p$rows, p$cols)
      check_part(sliced$header, paste(label, "header"))
      check_part(sliced$body, paste(label, "body"))
    }
  }
})

test_that("Merges running down a column survive a row page break", {
  dat <- data.frame(
    lbl = paste0("row", 1:8),
    v1 = as.character(1:8),
    v2 = as.character(11:18)
  )

  ct <- clintable(dat, use_labels = FALSE) |>
    clin_page_by(max_rows = 4)

  # A merge down the label column that runs straight through the page break
  ct <- flextable::merge_at(ct, i = 2:6, j = 1, part = "body")
  expect_equal(ct$body$spans$columns[, 1], c(1L, 5L, 0L, 0L, 0L, 0L, 1L, 1L))

  ct2 <- prep_pagination_(ct)
  pages <- ct2$clinify_config$pagination_idx
  expect_length(pages, 2)

  # Each page keeps only the part of the merge that is on it, and the count
  # lands on the first row of that page rather than staying behind
  expect_equal(
    slice_clintable(ct2, pages[[1]]$rows, pages[[1]]$cols)$body$spans$columns[, 1],
    c(1L, 3L, 0L, 0L)
  )
  expect_equal(
    slice_clintable(ct2, pages[[2]]$rows, pages[[2]]$cols)$body$spans$columns[, 1],
    c(2L, 0L, 1L, 1L)
  )
})
