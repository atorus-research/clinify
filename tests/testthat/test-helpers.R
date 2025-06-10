library(dplyr)

test_that("multiplication works", {
  expect_snapshot(
    iris |>
      arrange(Sepal.Length) |>
      mutate(
        page = make_grouped_pagenums(Species, 5)
      )
  )
})

test_that("Splits can be identified", {
  groups <- c('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c')
  nonempty <- c('a', '', '', 'b', '', '', 'c', '')

  expect_equal(which(find_split_inds(groups, "change")), c(1, 4, 7))
  expect_equal(which(find_split_inds(nonempty, "nonempty")), c(1, 4, 7))
})
