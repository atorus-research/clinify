# Create a new clintable object

A clintable object directly inherits from a flextable object. This
function will pass all necessary parameters
[`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)
and conver the object to a `clintable`

## Usage

``` r
clintable(
  x,
  page_by = NULL,
  group_by = NULL,
  use_labels = TRUE,
  coerce_character = FALSE,
  ...
)
```

## Arguments

- x:

  A data frame

- page_by:

  A variable in the input dataframe to use for pagination

- group_by:

  A character vector of variable names which will be used for grouping
  and attached as a label above the table headers

- use_labels:

  Use variable labels as column headers. Nested levels can be achieved
  using the string "\|\|" as a delimitter. Horizontally adjacent cells
  using identical words will be merged, which can be adjusted afterwards
  using the `merge` argument of
  [`clin_column_headers()`](https://atorus-research.github.io/clinify/reference/clin_column_headers.md).

- coerce_character:

  Coerce every column of `x` to character before the flextable is built,
  so pre-formatted values render exactly as supplied. Defaults to
  `FALSE`, which leaves flextable's numeric formatting in place.

- ...:

  Parameters to pass to
  [`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)

## Value

A clintable object

## Rendering values verbatim

flextable bakes cell text in when the table is built, and it formats a
`double` column as a whole with
`format(x, trim = TRUE, scientific = FALSE, big.mark = ",")`. Because
that decision is column wide, a clinical summary column holding a count
in one row and a statistic in another - necessarily a `double` - is
reformatted against its neighbours: `c(86, 75.2)` renders the count as
`"86.0"`, `c(1234, 12.5)` renders it as `"1,234.0"`, and
`c(1234567.891, 2)` is rounded to seven significant digits as
`"1,234,568"`. Values that were already formatted upstream are therefore
silently changed, and nothing errors to say so.

`coerce_character = TRUE` runs
[`as.character()`](https://rdrr.io/r/base/character.html) over every
column first, so each value carries into the table as its own string and
no column wide decision is made. It replaces the
`lapply(x, as.character)` line that otherwise has to be written ahead of
every table. Column `label` attributes survive the coercion, so
`use_labels` still finds them. Factors coerce to their levels rather
than their integer codes.

Two side effects are worth knowing about. Numeric columns lose the right
alignment flextable's default theme gives them, since alignment follows
column type; use
[`clin_table_align()`](https://atorus-research.github.io/clinify/reference/clin_table_align.md)
or
[`flextable::align()`](https://davidgohel.github.io/flextable/reference/align.html)
to put it back. And flextable's formula selectors compare against the
coerced values, so `bold(i = ~ n > 5)` becomes a string comparison and
quietly selects different rows.

## NA is left as NA

`as.character(NA)` is `NA_character_`, and flextable's default `na_str`
is `""`, so an `NA` still renders as a blank cell. `NA` is deliberately
not replaced with `""`, which is safe in a body column but changes the
meaning of a pagination variable.

[`clin_page_by()`](https://atorus-research.github.io/clinify/reference/clin_page_by.md)
splits where the page variable changes, as does
[`clin_group_by()`](https://atorus-research.github.io/clinify/reference/clin_group_by.md)
by default, and that comparison is `x != lag(x)`. It is `NA` wherever
either side is `NA`, and those rows are dropped rather than treated as
splits. So a `page_by`, `group_by`, or `caption_by` column that is
padded - carrying its value only on the first row of each block, `NA`
below - collapses to a single page with no group label. A variable used
that way needs `clin_group_by(when = "notempty")`, which tests against
`""` and handles `NA` just as well, and
[`clin_page_by()`](https://atorus-research.github.io/clinify/reference/clin_page_by.md)
offers no such option so its page variable has to carry a value on every
row.

Padding and a change comparison do not go together whichever the pad is,
but they fail differently, and the `NA` failure is the quieter one: `""`
padding makes each padded row look like a change and splits on every one
of them, which is hard to miss, where `NA` padding drops the splits and
leaves a plausible looking single page.

## Examples

``` r
clintable(mtcars)


.cl-5f96136a{}.cl-5f8df2de{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-5f90bda2{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:9pt;padding-top:9pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-5f90bdac{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-5f90e03e{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5f90e048{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


mpg
```
