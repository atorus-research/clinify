# Create a new title or footnote flextable

Create a new title or footnote flextable

## Usage

``` r
new_title_footnote(
  x,
  sect = c("titles", "footnotes", "footnote_page"),
  align = NULL
)
```

## Arguments

- x:

  a list of character vectors, no more than 3 elements to a vector.

- sect:

  Either "titles" or "footnotes"

- align:

  Where to place each line - `"left"`, `"center"`, `"right"`, `"split"`,
  or `NA` for the default. One value per element of `x`, or a single
  value for all of them.

## Value

A flextable object

## Examples

``` r

title <- new_title_footnote(
  list(
    # We'll add tools to automate paging
    c("Protocol: CDISCPILOT01", "Page {PAGE} of {NUMPAGES}"),
    c("Table 14-2.01"),
    c("Summary of Demographic and Baseline Characteristics")
  ),
  "titles"
)

footnote <- new_title_footnote(
  list(
    # We'll add tools to automate paging
    c("Page {PAGE}", "Total Pages: {NUMPAGES}")
  ),
  "footnotes"
)
```
