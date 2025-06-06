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
