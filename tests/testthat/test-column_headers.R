test_that("Errors checking", {
  # Proper target object
  expect_error(clin_column_headers(1), "inherits")

  # Proper argument types
  expect_error(clin_column_headers(clintable(iris), drat = 1), "All header arguments")


  # Proper column names
  expect_error(clin_column_headers(clintable(iris), blah = "blah"), "All argument names")
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
