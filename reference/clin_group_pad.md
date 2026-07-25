# Add Padding Between Groups in a Clinical Flextable

Adds top padding to rows in a \`clintable“ based on changes in a
grouping variable or non-empty values. Useful for visually separating
groups in a table

## Usage

``` r
clin_group_pad(
  x,
  pad_by,
  size = 9,
  when = c("change", "notempty"),
  drop = FALSE
)
```

## Arguments

- x:

  A clintable

- pad_by:

  A string indicating the column name used to detect group changes.

- size:

  Numeric value for the base padding size (default is 9).

- when:

  Character string indicating when to apply padding:

  - `"notempty"`: Add padding when the value in `pad_by` is not empty.

  - `"change"`: Add padding when the value in `pad_by` changes from the
    previous row.

- drop:

  Keep or drop the padding variable used to identify padding locations

## Value

A `clintable` object with modified padding.

## Examples

``` r

ct <- clintable(mtcars) |>
  clin_group_pad('gear')

ct <- clintable(mtcars) |>
  clin_group_pad('gear', size = 15)
```
