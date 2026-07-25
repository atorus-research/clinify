# Pagination

A major piece of the advantage that **{clinify}** offers is control over
pagination. Clinical tables can get fairly specific in how pagination is
handled, requiring page breaks to occur in specific places, data driven
content to be pulled into different portions of the table, or
overflowing columns into multiple pages. **{clinify}** offers a few
different function to help with this process.

## `clin_auto_page()`

The most simple method of pagination is to let Word take the reins and
do most of the work for you. Word has a feature called `keep_with_next`
where you can specify groups of rows that *shouldn’t* be broken by page
breaks. {flextable} implements this using the function `keep_with_next`,
and in **{clinify}** we’ve added a simple wrapper called
[`clin_auto_page()`](https://atorus-research.github.io/clinify/reference/clin_auto_page.md).

[`clin_auto_page()`](https://atorus-research.github.io/clinify/reference/clin_auto_page.md)
works by taking a grouping variable, which will be pulled out of the
table, and using that to determine the appropriate `keep_with_next`
groupings. Note that when using this function, you’re not specifying a
variable to *insert* page breaks, but rather saying “do not split rows
within this group across pages”. It’s also important to note that the
effect of this function won’t be apparent when printing to the screen,
and only within a word document itself. `keep_with_next` is a feature of
Word, so in an HTML view there’s nothing much to see.

``` r

library(clinify)
clintable(mtcars) |>
  clin_auto_page("gear")
```

| mpg  | cyl | disp  | hp  | drat | wt    | qsec  | vs  | am  | gear | carb |
|------|-----|-------|-----|------|-------|-------|-----|-----|------|------|
| 21.0 | 6   | 160.0 | 110 | 3.90 | 2.620 | 16.46 | 0   | 1   | 4    | 4    |
| 21.0 | 6   | 160.0 | 110 | 3.90 | 2.875 | 17.02 | 0   | 1   | 4    | 4    |
| 22.8 | 4   | 108.0 | 93  | 3.85 | 2.320 | 18.61 | 1   | 1   | 4    | 1    |
| 21.4 | 6   | 258.0 | 110 | 3.08 | 3.215 | 19.44 | 1   | 0   | 3    | 1    |
| 18.7 | 8   | 360.0 | 175 | 3.15 | 3.440 | 17.02 | 0   | 0   | 3    | 2    |
| 18.1 | 6   | 225.0 | 105 | 2.76 | 3.460 | 20.22 | 1   | 0   | 3    | 1    |
| 14.3 | 8   | 360.0 | 245 | 3.21 | 3.570 | 15.84 | 0   | 0   | 3    | 4    |
| 24.4 | 4   | 146.7 | 62  | 3.69 | 3.190 | 20.00 | 1   | 0   | 4    | 2    |
| 22.8 | 4   | 140.8 | 95  | 3.92 | 3.150 | 22.90 | 1   | 0   | 4    | 2    |
| 19.2 | 6   | 167.6 | 123 | 3.92 | 3.440 | 18.30 | 1   | 0   | 4    | 4    |
| 17.8 | 6   | 167.6 | 123 | 3.92 | 3.440 | 18.90 | 1   | 0   | 4    | 4    |
| 16.4 | 8   | 275.8 | 180 | 3.07 | 4.070 | 17.40 | 0   | 0   | 3    | 3    |
| 17.3 | 8   | 275.8 | 180 | 3.07 | 3.730 | 17.60 | 0   | 0   | 3    | 3    |
| 15.2 | 8   | 275.8 | 180 | 3.07 | 3.780 | 18.00 | 0   | 0   | 3    | 3    |
| 10.4 | 8   | 472.0 | 205 | 2.93 | 5.250 | 17.98 | 0   | 0   | 3    | 4    |

If you can accomplish what you need using this function, we recommend it
because it takes the least effort on behalf of the programmer. The other
functions that `clinify` has to offer give you more specific control,
but rely more on the inputs that you provide.

[`clin_auto_page()`](https://atorus-research.github.io/clinify/reference/clin_auto_page.md)
can use two different triggers using the `when` argument to determine
the break points within paging:

- `"change"`: When the value of the the `group_var` changes, a page
  break may be inserted, and
- `"notempty"`: When the value of `group_var` is populated (i.e. not
  `NA` or `""`), a page break may be inserted.

The `when` argument is a common interface available within
[`clin_group_by()`](https://atorus-research.github.io/clinify/reference/clin_group_by.md)
and `clin_pad_by()` as well.

Additionally,
[`clin_auto_page()`](https://atorus-research.github.io/clinify/reference/clin_auto_page.md)
has the `drop` argument to specify whether the paging variable should be
dropped from the table.

``` r

clintable(mtcars) |>
  clin_auto_page("gear", when = 'notempty', drop = TRUE)
```

## `clin_page_by()`

One method of pagination for clinical tables that’s been used for a long
time is to pre-program a variable that specifies where page breaks
should occur. When default page break methods don’t suffice, this can be
one way that you ensure breaks happen in the appropriate places. The
[`clin_page_by()`](https://atorus-research.github.io/clinify/reference/clin_page_by.md)
function allows you to target a variable in the table that will be used
to drive this.

Let’s prep some data and provide an example.

``` r

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

clintable(dat2) |>
  clin_page_by("page")
```

| mpg  | cyl | disp  | hp  | drat | wt    | qsec  | vs  | am  | gear | carb | groups1 | groups2 | captions  |
|------|-----|-------|-----|------|-------|-------|-----|-----|------|------|---------|---------|-----------|
| 21.0 | 6   | 160.0 | 110 | 3.90 | 2.620 | 16.46 | 0   | 1   | 4    | 4    | a       | 1       | Caption 1 |
| 21.0 | 6   | 160.0 | 110 | 3.90 | 2.875 | 17.02 | 0   | 1   | 4    | 4    | a       | 1       | Caption 1 |
| 22.8 | 4   | 108.0 | 93  | 3.85 | 2.320 | 18.61 | 1   | 1   | 4    | 1    | a       | 1       | Caption 1 |
| 21.4 | 6   | 258.0 | 110 | 3.08 | 3.215 | 19.44 | 1   | 0   | 3    | 1    | a       | 1       | Caption 1 |
| 18.7 | 8   | 360.0 | 175 | 3.15 | 3.440 | 17.02 | 0   | 0   | 3    | 2    | a       | 1       | Caption 1 |
| 18.1 | 6   | 225.0 | 105 | 2.76 | 3.460 | 20.22 | 1   | 0   | 3    | 1    | a       | 1       | Caption 1 |
| 14.3 | 8   | 360.0 | 245 | 3.21 | 3.570 | 15.84 | 0   | 0   | 3    | 4    | a       | 1       | Caption 1 |
| 24.4 | 4   | 146.7 | 62  | 3.69 | 3.190 | 20.00 | 1   | 0   | 4    | 2    | a       | 1       | Caption 1 |
| 22.8 | 4   | 140.8 | 95  | 3.92 | 3.150 | 22.90 | 1   | 0   | 4    | 2    | a       | 1       | Caption 1 |
| 19.2 | 6   | 167.6 | 123 | 3.92 | 3.440 | 18.30 | 1   | 0   | 4    | 4    | a       | 1       | Caption 1 |

| mpg  | cyl | disp  | hp  | drat | wt    | qsec  | vs  | am  | gear | carb | groups1 | groups2 | captions  |
|------|-----|-------|-----|------|-------|-------|-----|-----|------|------|---------|---------|-----------|
| 17.8 | 6   | 167.6 | 123 | 3.92 | 3.440 | 18.90 | 1   | 0   | 4    | 4    | a       | 1       | Caption 1 |
| 16.4 | 8   | 275.8 | 180 | 3.07 | 4.070 | 17.40 | 0   | 0   | 3    | 3    | a       | 1       | Caption 1 |
| 17.3 | 8   | 275.8 | 180 | 3.07 | 3.730 | 17.60 | 0   | 0   | 3    | 3    | a       | 1       | Caption 1 |
| 15.2 | 8   | 275.8 | 180 | 3.07 | 3.780 | 18.00 | 0   | 0   | 3    | 3    | a       | 1       | Caption 1 |
| 10.4 | 8   | 472.0 | 205 | 2.93 | 5.250 | 17.98 | 0   | 0   | 3    | 4    | a       | 1       | Caption 1 |
| 10.4 | 8   | 460.0 | 215 | 3.00 | 5.424 | 17.82 | 0   | 0   | 3    | 4    | a       | 1       | Caption 1 |
| 14.7 | 8   | 440.0 | 230 | 3.23 | 5.345 | 17.42 | 0   | 0   | 3    | 4    | a       | 2       | Caption 2 |
| 32.4 | 4   | 78.7  | 66  | 4.08 | 2.200 | 19.47 | 1   | 1   | 4    | 1    | a       | 2       | Caption 2 |
| 30.4 | 4   | 75.7  | 52  | 4.93 | 1.615 | 18.52 | 1   | 1   | 4    | 2    | a       | 2       | Caption 2 |
| 33.9 | 4   | 71.1  | 65  | 4.22 | 1.835 | 19.90 | 1   | 1   | 4    | 1    | a       | 2       | Caption 2 |

| mpg  | cyl | disp  | hp  | drat | wt    | qsec  | vs  | am  | gear | carb | groups1 | groups2 | captions  |
|------|-----|-------|-----|------|-------|-------|-----|-----|------|------|---------|---------|-----------|
| 21.5 | 4   | 120.1 | 97  | 3.70 | 2.465 | 20.01 | 1   | 0   | 3    | 1    | a       | 2       | Caption 2 |
| 15.5 | 8   | 318.0 | 150 | 2.76 | 3.520 | 16.87 | 0   | 0   | 3    | 2    | a       | 2       | Caption 2 |
| 15.2 | 8   | 304.0 | 150 | 3.15 | 3.435 | 17.30 | 0   | 0   | 3    | 2    | a       | 2       | Caption 2 |
| 13.3 | 8   | 350.0 | 245 | 3.73 | 3.840 | 15.41 | 0   | 0   | 3    | 4    | a       | 2       | Caption 2 |
| 19.2 | 8   | 400.0 | 175 | 3.08 | 3.845 | 17.05 | 0   | 0   | 3    | 2    | a       | 2       | Caption 2 |
| 27.3 | 4   | 79.0  | 66  | 4.08 | 1.935 | 18.90 | 1   | 1   | 4    | 1    | a       | 2       | Caption 2 |
| 26.0 | 4   | 120.3 | 91  | 4.43 | 2.140 | 16.70 | 0   | 1   | 5    | 2    | a       | 2       | Caption 2 |
| 30.4 | 4   | 95.1  | 113 | 3.77 | 1.513 | 16.90 | 1   | 1   | 5    | 2    | a       | 2       | Caption 2 |
| 15.8 | 8   | 351.0 | 264 | 4.22 | 3.170 | 14.50 | 0   | 1   | 5    | 4    | a       | 2       | Caption 2 |
| 19.7 | 6   | 145.0 | 175 | 3.62 | 2.770 | 15.50 | 0   | 1   | 5    | 6    | a       | 2       | Caption 2 |

1

2

3

An alternative to using a paging variable, another shortcut option is to
specify the maximum rows you want to print to a specific page.

``` r

clintable(dat2) |>
  clin_page_by(max_rows = 5)
```

| mpg  | cyl | disp  | hp  | drat | wt    | qsec  | vs  | am  | gear | carb | page | groups1 | groups2 | captions  |
|------|-----|-------|-----|------|-------|-------|-----|-----|------|------|------|---------|---------|-----------|
| 21.0 | 6   | 160.0 | 110 | 3.90 | 2.620 | 16.46 | 0   | 1   | 4    | 4    | 1    | a       | 1       | Caption 1 |
| 21.0 | 6   | 160.0 | 110 | 3.90 | 2.875 | 17.02 | 0   | 1   | 4    | 4    | 1    | a       | 1       | Caption 1 |
| 22.8 | 4   | 108.0 | 93  | 3.85 | 2.320 | 18.61 | 1   | 1   | 4    | 1    | 1    | a       | 1       | Caption 1 |
| 21.4 | 6   | 258.0 | 110 | 3.08 | 3.215 | 19.44 | 1   | 0   | 3    | 1    | 1    | a       | 1       | Caption 1 |
| 18.7 | 8   | 360.0 | 175 | 3.15 | 3.440 | 17.02 | 0   | 0   | 3    | 2    | 1    | a       | 1       | Caption 1 |

| mpg  | cyl | disp  | hp  | drat | wt    | qsec  | vs  | am  | gear | carb | page | groups1 | groups2 | captions  |
|------|-----|-------|-----|------|-------|-------|-----|-----|------|------|------|---------|---------|-----------|
| 18.1 | 6   | 225.0 | 105 | 2.76 | 3.460 | 20.22 | 1   | 0   | 3    | 1    | 1    | a       | 1       | Caption 1 |
| 14.3 | 8   | 360.0 | 245 | 3.21 | 3.570 | 15.84 | 0   | 0   | 3    | 4    | 1    | a       | 1       | Caption 1 |
| 24.4 | 4   | 146.7 | 62  | 3.69 | 3.190 | 20.00 | 1   | 0   | 4    | 2    | 1    | a       | 1       | Caption 1 |
| 22.8 | 4   | 140.8 | 95  | 3.92 | 3.150 | 22.90 | 1   | 0   | 4    | 2    | 1    | a       | 1       | Caption 1 |
| 19.2 | 6   | 167.6 | 123 | 3.92 | 3.440 | 18.30 | 1   | 0   | 4    | 4    | 1    | a       | 1       | Caption 1 |

| mpg  | cyl | disp  | hp  | drat | wt    | qsec  | vs  | am  | gear | carb | page | groups1 | groups2 | captions  |
|------|-----|-------|-----|------|-------|-------|-----|-----|------|------|------|---------|---------|-----------|
| 17.8 | 6   | 167.6 | 123 | 3.92 | 3.440 | 18.90 | 1   | 0   | 4    | 4    | 2    | a       | 1       | Caption 1 |
| 16.4 | 8   | 275.8 | 180 | 3.07 | 4.070 | 17.40 | 0   | 0   | 3    | 3    | 2    | a       | 1       | Caption 1 |
| 17.3 | 8   | 275.8 | 180 | 3.07 | 3.730 | 17.60 | 0   | 0   | 3    | 3    | 2    | a       | 1       | Caption 1 |
| 15.2 | 8   | 275.8 | 180 | 3.07 | 3.780 | 18.00 | 0   | 0   | 3    | 3    | 2    | a       | 1       | Caption 1 |
| 10.4 | 8   | 472.0 | 205 | 2.93 | 5.250 | 17.98 | 0   | 0   | 3    | 4    | 2    | a       | 1       | Caption 1 |

1

2

3

## `clin_group_by()`

Another specific form of available is the function
[`clin_group_by()`](https://atorus-research.github.io/clinify/reference/clin_group_by.md).
This function takes two arguments after the `clintable`:

- `group_by`, and
- `caption_by`

The data within variables provided in `group_by` will be extracted to
lines that are printed **above** the header and in the table body.
Similarly, the `caption_by` data will be pulled into footnotes attached
below the table body but above the footer.

``` r

clintable(dat2) |>
  clin_page_by("page") |>
  clin_group_by(c("groups1", "groups2"), caption_by = "captions")
```

[TABLE]

[TABLE]

[TABLE]

1

2

3

[`clin_group_by()`](https://atorus-research.github.io/clinify/reference/clin_group_by.md)
can be used in combination with
[`clin_page_by()`](https://atorus-research.github.io/clinify/reference/clin_page_by.md),
but this is not required. When these two functions are used together,
page breaks will occur when **any of** the group, caption, or page
variables change.

Much the same as
[`clin_auto_page()`](https://atorus-research.github.io/clinify/reference/clin_auto_page.md),
the
[`clin_group_by()`](https://atorus-research.github.io/clinify/reference/clin_group_by.md)
function can identify groups using the `when` argument. In the
[`clin_group_by()`](https://atorus-research.github.io/clinify/reference/clin_group_by.md),
using `when = "notempty"` will mark a group whenever any of the
`group_by` variables or the `caption_by` variables are populated. When
using `when = "change"` groups are marked whenever any of those
variables change.

## `clin_alt_pages()`

The most complex pagination method is
[`clin_alt_pages()`](https://atorus-research.github.io/clinify/reference/clin_alt_pages.md).
This function allows you to split one table across multiple pages. This
is specifically reserved for circumstances in which you have a table
that has too many columns to fit on one page. For example, a study may
have many different treatment groups that need to be presented, or
perhaps a listing needs to present a large number of columns. This isn’t
an ideal case for a table, but it’s still a common situation.

[`clin_alt_pages()`](https://atorus-research.github.io/clinify/reference/clin_alt_pages.md)
takes two parameters after the `clintable`:

- `key_cols`: Columns frozen to each page
- `col_groups`: The sets of columns differing from page to page

``` r

clintable(dat2) |>
  clin_page_by("page") |>
  clin_alt_pages(
    key_cols = c("mpg", "cyl", "hp"),
    col_groups = list(
      c("disp", "drat", "wt"),
      c("qsec", "vs", "am"),
      c("gear", "carb")
    )
  )
```

| mpg  | cyl | hp  | disp  | drat | wt    |
|------|-----|-----|-------|------|-------|
| 21.0 | 6   | 110 | 160.0 | 3.90 | 2.620 |
| 21.0 | 6   | 110 | 160.0 | 3.90 | 2.875 |
| 22.8 | 4   | 93  | 108.0 | 3.85 | 2.320 |
| 21.4 | 6   | 110 | 258.0 | 3.08 | 3.215 |
| 18.7 | 8   | 175 | 360.0 | 3.15 | 3.440 |
| 18.1 | 6   | 105 | 225.0 | 2.76 | 3.460 |
| 14.3 | 8   | 245 | 360.0 | 3.21 | 3.570 |
| 24.4 | 4   | 62  | 146.7 | 3.69 | 3.190 |
| 22.8 | 4   | 95  | 140.8 | 3.92 | 3.150 |
| 19.2 | 6   | 123 | 167.6 | 3.92 | 3.440 |

| mpg  | cyl | hp  | qsec  | vs  | am  |
|------|-----|-----|-------|-----|-----|
| 21.0 | 6   | 110 | 16.46 | 0   | 1   |
| 21.0 | 6   | 110 | 17.02 | 0   | 1   |
| 22.8 | 4   | 93  | 18.61 | 1   | 1   |
| 21.4 | 6   | 110 | 19.44 | 1   | 0   |
| 18.7 | 8   | 175 | 17.02 | 0   | 0   |
| 18.1 | 6   | 105 | 20.22 | 1   | 0   |
| 14.3 | 8   | 245 | 15.84 | 0   | 0   |
| 24.4 | 4   | 62  | 20.00 | 1   | 0   |
| 22.8 | 4   | 95  | 22.90 | 1   | 0   |
| 19.2 | 6   | 123 | 18.30 | 1   | 0   |

| mpg  | cyl | hp  | gear | carb |
|------|-----|-----|------|------|
| 21.0 | 6   | 110 | 4    | 4    |
| 21.0 | 6   | 110 | 4    | 4    |
| 22.8 | 4   | 93  | 4    | 1    |
| 21.4 | 6   | 110 | 3    | 1    |
| 18.7 | 8   | 175 | 3    | 2    |
| 18.1 | 6   | 105 | 3    | 1    |
| 14.3 | 8   | 245 | 3    | 4    |
| 24.4 | 4   | 62  | 4    | 2    |
| 22.8 | 4   | 95  | 4    | 2    |
| 19.2 | 6   | 123 | 4    | 4    |

1

2

3

One thing to note about this function is that it should be paired with
[`clin_page_by()`](https://atorus-research.github.io/clinify/reference/clin_page_by.md).
This function works by chunking the input clintable into individual
pages and separating those pages using page breaks - so the number of
rows printed to each page is necessary for the page splits to work
effectively. If
[`clin_page_by()`](https://atorus-research.github.io/clinify/reference/clin_page_by.md)
is not provided, a warning will appear and the table will default to 20
rows per page.

As another option for creating a data driven paging variable, we’ve
added the helper function
[`make_grouped_pagenums()`](https://atorus-research.github.io/clinify/reference/make_grouped_pagenums.md).
This function could be helpful when creating tables using alternating
pages that also using
[`clin_group_by()`](https://atorus-research.github.io/clinify/reference/clin_group_by.md)
for header labels. Alternating page tables require manual management of
paging variables, since each page is technically a new table within the
word document.
[`make_grouped_pagenums()`](https://atorus-research.github.io/clinify/reference/make_grouped_pagenums.md)
allows you to use a group variable and then generate a page variable
that will appropriately change when the group changes. Consider this
example:

``` r

library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
head(ToothGrowth, 20) |>
  mutate(page = make_grouped_pagenums(dose, 7))
#>     len supp dose page
#> 1   4.2   VC  0.5    1
#> 2  11.5   VC  0.5    1
#> 3   7.3   VC  0.5    1
#> 4   5.8   VC  0.5    1
#> 5   6.4   VC  0.5    1
#> 6  10.0   VC  0.5    1
#> 7  11.2   VC  0.5    1
#> 8  11.2   VC  0.5    2
#> 9   5.2   VC  0.5    2
#> 10  7.0   VC  0.5    2
#> 11 16.5   VC  1.0    3
#> 12 16.5   VC  1.0    3
#> 13 15.2   VC  1.0    3
#> 14 17.3   VC  1.0    3
#> 15 22.5   VC  1.0    3
#> 16 17.3   VC  1.0    3
#> 17 13.6   VC  1.0    3
#> 18 14.5   VC  1.0    4
#> 19 18.8   VC  1.0    4
#> 20 15.5   VC  1.0    4
```

Note in the output that the `page` variable either every 7 rows, or when
the value of `dose` changes.
