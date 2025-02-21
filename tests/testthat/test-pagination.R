pages <- seq_along(names(mtcars))
names(pages) <- names(mtcars)
p1 <- pages[c(1,2,4, 3, 5, 6)]
p2 <- pages[c(1,2, 4, 7, 8, 9)]
p3 <- pages[c(1, 2, 4, 10, 11)]

refdat <- mtcars
refdat['page'] <- c(
  rep(1, 10),
  rep(2, 10),
  rep(3, 10),
  c(4, 4)
)

refdat2 <- rbind(refdat, refdat)
refdat2['groups'] <- c(
  rep('a', 32),
  rep('b', 32)
)

test_that("Alternating with page by",{

  ct <- clin_alt_pages(
    clintable(refdat),
    key_cols = c('mpg', 'cyl', 'hp'),
    col_groups = list(
      c('disp', 'drat', 'wt'),
      c('qsec', 'vs', 'am'),
      c('gear', 'carb')
      ) 
    ) |>
    clin_page_by('page')

  ct2 <- prep_pagination_(ct)

  exp_out <- list(
    list(
      rows = 1:10,
      cols = p1
    ), 
    list(
      rows = 1:10,
      cols = p2
    ), 
    list(
      rows = 1:10,
      cols = p3
    ), 
    list(
      rows = 11:20,
      cols = p1
    ), 
    list(
      rows = 11:20,
      cols = p2
    ), 
    list(
      rows = 11:20,
      cols = p3
    ),
    list(
      rows = 21:30,
      cols = p1
    ), 
    list(
      rows = 21:30,
      cols = p2
    ), 
    list(
      rows = 21:30,
      cols = p3
    ), 
    list(
      rows = c(31, 32),
      cols = p1
    ), 
    list(
      rows = c(31, 32),
      cols = p2
    ), 
    list(
      rows = c(31, 32),
      cols = p3
    )
  )

  expect_equal(ct2$clinify_config$pagination_idx, exp_out)
  # 'page' variable should be stripped when applying pagination
  expect_true("page" %in% ct$col_keys)
  expect_false("page" %in% ct2$col_keys)
  
})

test_that("Page by no alternating", {
  ct <- clintable(refdat)|>
    clin_page_by('page')

  ct2 <- prep_pagination_(ct)

  exp_out <- list(
    list(
      rows = 1:10,
      cols = pages
    ), 
    list(
      rows = 11:20,
      cols = pages
    ), 
    list(
      rows = 21:30,
      cols = pages
    ),
    list(
      rows = c(31, 32),
      cols = pages
    )
  )

  expect_equal(ct2$clinify_config$pagination_idx, exp_out)
  # 'page' variable should be stripped when applying pagination
  expect_true("page" %in% ct$col_keys)
  expect_false("page" %in% ct2$col_keys)
  
})
  




