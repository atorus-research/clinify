# Set the column headers of the output clintable

This function allows you to apply column headers named arguments and
character vectors. Separate elements of the character vector are
converted to separate levels of the output table header. The in which
the headers are applied goes from top to bottom, so if you provide 3
elements for a column header, the first element is applied to the top
and the second to the bottom. If one variable has three levels and other
variable only have one or two, the columns with less levels to the
header will bind to the bottom. So a column with two levels will apply
to the second and third row, and a column with one level with apply the
bottom row. Spanners are determined using cells of the same text value,
where horizontally adjacent cells holding the same text are merged. Use
the `merge` argument when a header row legitimately repeats a label
across adjacent columns and those cells should be left alone - merged,
they render as one label centred over the whole run, so the repeats are
not there to read any more. That is most often wanted for the bottom
row, which holds each column's own label: six columns each labelled
`"Baseline"` come out as a single `Baseline` spanning all six unless
`merge = "spanners"` keeps that row out of it. `merge` works a row at a
time, so if a single row needs some of its repeated cells merged but not
others, leave that row out of `merge` and span the intended cells with
[`flextable::merge_at()`](https://davidgohel.github.io/flextable/reference/merge_at.html).

## Usage

``` r
clin_column_headers(x, ..., merge = TRUE)
```

## Arguments

- x:

  A clintable object

- ...:

  Named arguments providing the column header text. Separate levels of
  the header are determined using separate elements of a character
  vector.

- merge:

  Controls the automatic merging of identical, adjacent header cells,
  which is what forms spanners. `TRUE` (the default) or `"all"` merges
  every header row, `FALSE` or `"none"` merges none of them, and
  `"spanners"` merges every row except the bottom one - the row holding
  the individual column labels. Merging can also be limited to specific
  header rows, numbered from the top down, using ordinary R subscripts:
  `merge = 1:2` merges the top two rows only, `merge = -3` merges every
  row except the third, and a logical vector as long as the header is
  deep toggles each row individually. Only the header is ever merged -
  the table body is left alone.

  One thing to know: a custom
  [`clinify_table_default()`](https://atorus-research.github.io/clinify/reference/clinify_defaults.md)
  that calls
  [`flextable::merge_h()`](https://davidgohel.github.io/flextable/reference/merge_h.html)
  on the header will merge it again when the table renders, overriding
  whatever is set here.

## Value

A clintable object

## Details

The same result can be achieved using column labels on the input
dataframe to the clintable. If labels are present, header levels will be
separated using the delimitter "\|\|" within the label string. Headers
built that way can have their merging adjusted by calling
`clin_column_headers()` with no header text and only the `merge`
argument, which leaves the header text as it is. Called that way, any
merging already on the header is cleared first - including merges
applied by hand with
[`flextable::merge_at()`](https://davidgohel.github.io/flextable/reference/merge_at.html)
or
[`flextable::merge_v()`](https://davidgohel.github.io/flextable/reference/merge_v.html) -
so the rows named in `merge` end up being the only merged rows.

## Examples

``` r

clintable(iris) |>
  clin_column_headers(
    Sepal.Length = c("Flowers", "Sepal", "Length"),
    Sepal.Width = c("Flowers", "Sepal", "Width"),
    Petal.Length = c("Petal", "Length"),
    Petal.Width = c("Petal", "Width")
  )


.cl-c0e3924c{}.cl-c0d911aa{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-c0df057e{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:9pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c0df0588{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c0df0592{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:9pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c0df0593{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c0df059c{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c0df2f40{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c0df2f4a{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c0df2f4b{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c0df2f54{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c0df2f55{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c0df2f5e{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c0df2f5f{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c0df2f68{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c0df2f69{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}



Flowers
```
