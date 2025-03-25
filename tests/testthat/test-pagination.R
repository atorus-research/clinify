pages <- seq_along(names(mtcars))
names(pages) <- names(mtcars)

p1 <- pages[c(1, 2, 4, 3, 5, 6)]
p2 <- pages[c(1, 2, 4, 7, 8, 9)]
p3 <- pages[c(1, 2, 4, 10, 11)]

refdat <- mtcars
refdat["page"] <- c(
  rep(1, 10),
  rep(2, 10),
  rep(3, 10),
  c(4, 4)
)

refdat2 <- rbind(mtcars, mtcars)
refdat2["groups"] <- c(
  rep("a", 32),
  rep("b", 32)
)
refdat2["page"] <- c(
  rep(1, 10),
  rep(2, 10),
  rep(3, 10),
  rep(4, 10),
  rep(5, 10),
  rep(6, 10),
  rep(7, 4)
)

refdat3 <- refdat2

refdat3["groups2"] <- c(
  rep("1", 16),
  rep("2", 16),
  rep("1", 16),
  rep("2", 16)
) 

refdat3['captions'] <- c(
  rep('Caption 1', 16),
  rep('Caption 2', 16),
  rep('Caption 3', 16),
  rep('Caption 4', 16)
)

test_that("Alternating with page by", {
  ct <- clin_alt_pages(
    clintable(refdat),
    key_cols = c("mpg", "cyl", "hp"),
    col_groups = list(
      c("disp", "drat", "wt"),
      c("qsec", "vs", "am"),
      c("gear", "carb")
    )
  ) |>
    clin_page_by("page")

  ct2 <- prep_pagination_(ct)

  exp_out <- list(
    list(
      rows = 1:10,
      cols = p1,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 1:10,
      cols = p2,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 1:10,
      cols = p3,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 11:20,
      cols = p1,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 11:20,
      cols = p2,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 11:20,
      cols = p3,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 21:30,
      cols = p1,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 21:30,
      cols = p2,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 21:30,
      cols = p3,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = c(31, 32),
      cols = p1,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = c(31, 32),
      cols = p2,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = c(31, 32),
      cols = p3,
      label=NULL,
      captions=NULL
    )
  )

  expect_equal(ct2$clinify_config$pagination_idx, exp_out)
  # 'page' variable should be stripped when applying pagination
  expect_true("page" %in% ct$col_keys)
  expect_false("page" %in% ct2$col_keys)
})

test_that("Page by no alternating", {
  ct <- clintable(refdat) |>
    clin_page_by("page")

  ct2 <- prep_pagination_(ct)

  exp_out <- list(
    list(
      rows = 1:10,
      cols = pages,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 11:20,
      cols = pages,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 21:30,
      cols = pages,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = c(31, 32),
      cols = pages,
      label=NULL,
      captions=NULL
    )
  )

  expect_equal(ct2$clinify_config$pagination_idx, exp_out)
  # 'page' variable should be stripped when applying pagination
  expect_true("page" %in% ct$col_keys)
  expect_false("page" %in% ct2$col_keys)
})

test_that("Alternating no page by", {
  ct <- clin_alt_pages(
    clintable(mtcars),
    key_cols = c("mpg", "cyl", "hp"),
    col_groups = list(
      c("disp", "drat", "wt"),
      c("qsec", "vs", "am"),
      c("gear", "carb")
    )
  )

  expect_message(ct2 <- prep_pagination_(ct), "NOTE: Alternating")

  exp_out <- list(
    list(
      rows = 1:20,
      cols = p1,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 1:20,
      cols = p2,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 1:20,
      cols = p3,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 21:32,
      cols = p1,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 21:32,
      cols = p2,
      label=NULL,
      captions=NULL
    ),
    list(
      rows = 21:32,
      cols = p3,
      label=NULL,
      captions=NULL
    )
  )
})

test_that("Alternating with page by with groups", {
  ct <- clin_alt_pages(
    clintable(refdat2),
    key_cols = c("mpg", "cyl", "hp"),
    col_groups = list(
      c("disp", "drat", "wt"),
      c("qsec", "vs", "am"),
      c("gear", "carb")
    )
  ) |>
    clin_page_by("page") |>
    clin_group_by("groups")

  ct2 <- prep_pagination_(ct)

  exp_out <- list(
    list(
      rows = 1:10,
      cols = p1,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 1:10,
      cols = p2,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 1:10,
      cols = p3,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 11:20,
      cols = p1,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 11:20,
      cols = p2,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 11:20,
      cols = p3,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 21:30,
      cols = p1,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 21:30,
      cols = p2,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 21:30,
      cols = p3,
      label = "a",
      captions=NULL
    ),
    list(
      rows = c(31, 32),
      cols = p1,
      label = "a",
      captions=NULL
    ),
    list(
      rows = c(31, 32),
      cols = p2,
      label = "a",
      captions=NULL
    ),
    list(
      rows = c(31, 32),
      cols = p3,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 33:40,
      cols = p1,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 33:40,
      cols = p2,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 33:40,
      cols = p3,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 41:50,
      cols = p1,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 41:50,
      cols = p2,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 41:50,
      cols = p3,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 51:60,
      cols = p1,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 51:60,
      cols = p2,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 51:60,
      cols = p3,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 61:64,
      cols = p1,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 61:64,
      cols = p2,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 61:64,
      cols = p3,
      label = "b",
      captions=NULL
    )
  )

  expect_equal(ct2$clinify_config$pagination_idx, exp_out)
  # 'page' variable should be stripped when applying pagination
  expect_true("page" %in% ct$col_keys)
  expect_false("page" %in% ct2$col_keys)
  expect_true("groups" %in% ct$col_keys)
  expect_false("groups" %in% ct2$col_keys)
})

test_that("Page by no alternating with groups", {
  ct <- clintable(refdat2) |>
    clin_page_by("page") |>
    clin_group_by("groups")

  ct2 <- prep_pagination_(ct)

  exp_out <- list(
    list(
      rows = 1:10,
      cols = pages,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 11:20,
      cols = pages,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 21:30,
      cols = pages,
      label = "a",
      captions=NULL
    ),
    list(
      rows = c(31, 32),
      cols = pages,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 33:40,
      cols = pages,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 41:50,
      cols = pages,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 51:60,
      cols = pages,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 61:64,
      cols = pages,
      label = "b",
      captions=NULL
    )
  )

  expect_equal(ct2$clinify_config$pagination_idx, exp_out)
  # 'page' variable should be stripped when applying pagination
  expect_true("page" %in% ct$col_keys)
  expect_false("page" %in% ct2$col_keys)
  expect_true("groups" %in% ct$col_keys)
  expect_false("groups" %in% ct2$col_keys)
})

test_that("Groups no page by", {
  ct <- clintable(refdat2[-13]) |>
    clin_group_by("groups")

  ct2 <- prep_pagination_(ct)

  exp_out <- list(
    list(
      rows = 1:32,
      cols = pages,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 33:64,
      cols = pages,
      label = "b",
      captions=NULL
    )
  )

  expect_equal(ct2$clinify_config$pagination_idx, exp_out)
  # 'page' variable should be stripped when applying pagination
  expect_true("groups" %in% ct$col_keys)
  expect_false("groups" %in% ct2$col_keys)
})

test_that("Alternating pages with groups", {
  ct <- clin_alt_pages(
    clintable(refdat2[-13]),
    key_cols = c("mpg", "cyl", "hp"),
    col_groups = list(
      c("disp", "drat", "wt"),
      c("qsec", "vs", "am"),
      c("gear", "carb")
    )
  ) |>
    clin_group_by("groups")

  expect_message(ct2 <- prep_pagination_(ct), "NOTE: Alternating")

  exp_out <- list(
    list(
      rows = 1:20,
      cols = p1,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 1:20,
      cols = p2,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 1:20,
      cols = p3,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 21:32,
      cols = p1,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 21:32,
      cols = p2,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 21:32,
      cols = p3,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 33:52,
      cols = p1,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 33:52,
      cols = p2,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 33:52,
      cols = p3,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 53:64,
      cols = p1,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 53:64,
      cols = p2,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 53:64,
      cols = p3,
      label = "b",
      captions=NULL
    )
  )

  expect_equal(ct2$clinify_config$pagination_idx, exp_out)
  # 'page' variable should be stripped when applying pagination
  expect_true("groups" %in% ct$col_keys)
  expect_false("groups" %in% ct2$col_keys)
})

test_that("Multiple groups are pulled out properly", {
  ct <- clintable(refdat3) |>
    clin_page_by("page") |>
    clin_group_by(c("groups", "groups2"), caption_by="captions")

  ct2 <- prep_pagination_(ct)

  exp_out <- list(
    list(
      rows = 1:10,
      cols = pages,
      label = c("a", "1"),
      captions = "Caption 1"
    ),
    list(
      rows = 11:16,
      cols = pages,
      label = c("a", "1"),
      captions = "Caption 1"
    ),
    list(
      rows = 17:20,
      cols = pages,
      label = c("a", "2"),
      captions = "Caption 2"
    ),
    list(
      rows = 21:30,
      cols = pages,
      label = c("a", "2"),
      captions = "Caption 2"
    ),
    list(
      rows = 31:32,
      cols = pages,
      label = c("a", "2"),
      captions = "Caption 2"
    ),
    list(
      rows = 33:40,
      cols = pages,
      label = c("b", "1"),
      captions = "Caption 3"
    ),
    list(
      rows = 41:48,
      cols = pages,
      label = c("b", "1"),
      captions = "Caption 3"
    ),
    list(
      rows = 49:50,
      cols = pages,
      label = c("b", "2"),
      captions = "Caption 4"
    ),
    list(
      rows = 51:60,
      cols = pages,
      label = c("b", "2"),
      captions = "Caption 4"
    ),
    list(
      rows = 61:64,
      cols = pages,
      label = c("b", "2"),
      captions = "Caption 4"
    )
  )

  expect_equal(ct2$clinify_config$pagination_idx, exp_out)
  # 'page' variable should be stripped when applying pagination
  expect_true("groups" %in% ct$col_keys)
  expect_false("groups" %in% ct2$col_keys)
  expect_true("groups2" %in% ct$col_keys)
  expect_false("groups2" %in% ct2$col_keys)
  expect_true("page" %in% ct$col_keys)
  expect_false("page" %in% ct2$col_keys)
})

test_that("Multiple group by with alternating pages", {
  ct <- clintable(refdat3) |>
    clin_page_by("page") |>
    clin_group_by(c("groups", "groups2")) |>
    clin_alt_pages(
      key_cols = c("mpg", "cyl", "hp"),
      col_groups = list(
        c("disp", "drat", "wt"),
        c("qsec", "vs", "am"),
        c("gear", "carb")
      )
    )

  ct2 <- prep_pagination_(ct)

  exp_out <- list(
    list(
      rows = 1:10,
      cols = p1,
      label = c("a", "1"),
      captions = NULL
    ),
    list(
      rows = 1:10,
      cols = p2,
      label = c("a", "1"),
      captions = NULL
    ),
    list(
      rows = 1:10,
      cols = p3,
      label = c("a", "1"),
      captions = NULL
    ),
    list(
      rows = 11:16,
      cols = p1,
      label = c("a", "1"),
      captions = NULL
    ),
    list(
      rows = 11:16,
      cols = p2,
      label = c("a", "1"),
      captions = NULL
    ),
    list(
      rows = 11:16,
      cols = p3,
      label = c("a", "1"),
      captions = NULL
    ),
    list(
      rows = 17:20,
      cols = p1,
      label = c("a", "2"),
      captions = NULL
    ),
    list(
      rows = 17:20,
      cols = p2,
      label = c("a", "2"),
      captions = NULL
    ),
    list(
      rows = 17:20,
      cols = p3,
      label = c("a", "2"),
      captions = NULL
    ),
    list(
      rows = 21:30,
      cols = p1,
      label = c("a", "2"),
      captions = NULL
    ),
    list(
      rows = 21:30,
      cols = p2,
      label = c("a", "2"),
      captions = NULL
    ),
    list(
      rows = 21:30,
      cols = p3,
      label = c("a", "2"),
      captions = NULL
    ),
    list(
      rows = 31:32,
      cols = p1,
      label = c("a", "2"),
      captions = NULL
    ),
    list(
      rows = 31:32,
      cols = p2,
      label = c("a", "2"),
      captions = NULL
    ),
    list(
      rows = 31:32,
      cols = p3,
      label = c("a", "2"),
      captions = NULL
    ),
    list(
      rows = 33:40,
      cols = p1,
      label = c("b", "1"),
      captions = NULL
    ),
    list(
      rows = 33:40,
      cols = p2,
      label = c("b", "1"),
      captions = NULL
    ),
    list(
      rows = 33:40,
      cols = p3,
      label = c("b", "1"),
      captions = NULL
    ),
    list(
      rows = 41:48,
      cols = p1,
      label = c("b", "1"),
      captions = NULL
    ),
    list(
      rows = 41:48,
      cols = p2,
      label = c("b", "1"),
      captions = NULL
    ),
    list(
      rows = 41:48,
      cols = p3,
      label = c("b", "1"),
      captions = NULL
    ),
    list(
      rows = 49:50,
      cols = p1,
      label = c("b", "2"),
      captions = NULL
    ),
    list(
      rows = 49:50,
      cols = p2,
      label = c("b", "2"),
      captions = NULL
    ),
    list(
      rows = 49:50,
      cols = p3,
      label = c("b", "2"),
      captions = NULL
    ),
    list(
      rows = 51:60,
      cols = p1,
      label = c("b", "2"),
      captions = NULL
    ),
    list(
      rows = 51:60,
      cols = p2,
      label = c("b", "2"),
      captions = NULL
    ),
    list(
      rows = 51:60,
      cols = p3,
      label = c("b", "2"),
      captions = NULL
    ),
    list(
      rows = 61:64,
      cols = p1,
      label = c("b", "2"),
      captions = NULL
    ),
    list(
      rows = 61:64,
      cols = p2,
      label = c("b", "2"),
      captions = NULL
    ),
    list(
      rows = 61:64,
      cols = p3,
      label = c("b", "2"),
      captions = NULL
    )
  )

  expect_equal(ct2$clinify_config$pagination_idx, exp_out)
  # 'page' variable should be stripped when applying pagination
  expect_true("groups" %in% ct$col_keys)
  expect_false("groups" %in% ct2$col_keys)
  expect_true("groups2" %in% ct$col_keys)
  expect_false("groups2" %in% ct2$col_keys)
  expect_true("page" %in% ct$col_keys)
  expect_false("page" %in% ct2$col_keys)
})

test_that("Test using max rows", {
  ct <- clintable(refdat2[, -13]) |>
    clin_page_by(max_rows = 36) |>
    clin_group_by("groups") |>
    clin_alt_pages(
      key_cols = c("mpg", "cyl", "hp"),
      col_groups = list(
        c("disp", "drat", "wt"),
        c("qsec", "vs", "am"),
        c("gear", "carb")
      )
    )

  ct2 <- prep_pagination_(ct)

  exp_out <- list(
    list(
      rows = 1:32,
      cols = p1,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 1:32,
      cols = p2,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 1:32,
      cols = p3,
      label = "a",
      captions=NULL
    ),
    list(
      rows = 33:64,
      cols = p1,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 33:64,
      cols = p2,
      label = "b",
      captions=NULL
    ),
    list(
      rows = 33:64,
      cols = p3,
      label = "b",
      captions=NULL
    )
  )

  expect_equal(ct2$clinify_config$pagination_idx, exp_out)
  # 'page' variable should be stripped when applying pagination
  expect_true("groups" %in% ct$col_keys)
  expect_false("groups" %in% ct2$col_keys)
})

