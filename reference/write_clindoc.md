# Clintable write method

Write a clinify table out to a docx file

## Usage

``` r
write_clindoc(x, file)
```

## Arguments

- x:

  a clintable object

- file:

  The file path to which the file should be written

## Value

Invisible

## Examples

``` r
ct <- clintable(mtcars)

ct <- clin_alt_pages(
  ct,
  key_cols = c("mpg", "cyl", "hp"),
  col_groups = list(
    c("disp", "drat", "wt"),
    c("qsec", "vs", "am"),
    c("gear", "carb")
  )
)

# Get document object directly
doc <- clindoc(ct)
#> NOTE: Alternating pages were set, but no selection for row wise pagination was configured Defaulting to 20 rows per page.

# Write out docx file
write_clindoc(ct, file.path(tempdir(), "demo.docx"))
#> NOTE: Alternating pages were set, but no selection for row wise pagination was configured Defaulting to 20 rows per page.
```
