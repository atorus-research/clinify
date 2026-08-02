# Convert a flextable into a clintable object

Convert a flextable into a clintable object

## Usage

``` r
as_clintable(x, page_by = NULL, group_by = NULL)
```

## Arguments

- x:

  A flextable object

- page_by:

  A variable in the input dataframe to use for pagination

- group_by:

  A variable which will be used for grouping and attached as a label
  above the table headers

## Value

A clintable object

## Details

There is no `coerce_character` argument here, unlike
[`clintable()`](https://atorus-research.github.io/clinify/reference/clintable.md).
A flextable arrives with its cell text already rendered, so the numeric
formatting this argument exists to avoid has already happened and
coercing the source data is no longer an option. The nearest equivalent
is `flextable::set_formatter(x, values = as.character)` before calling
`as_clintable()`, which rewrites every body cell from the stored data.
That is not the same operation: it replaces cell *content*, so any chunk
level work already done on the body -
[`flextable::compose()`](https://davidgohel.github.io/flextable/reference/compose.html),
`flextable::colformat_*()`, images, hyperlinks, equations - is
discarded, and columns keep the right alignment flextable gave them for
being numeric. Because that trade cannot be made safely on the user's
behalf, it is left to the caller. To get the coercion without the trade,
build with `clintable(x, coerce_character = TRUE)` instead.

## Examples

``` r

ft <- flextable::flextable(mtcars)
as_clintable(ft)


.cl-bf58f2b4{}.cl-bf4e6d58{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-bf5420e0{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:9pt;padding-top:9pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-bf5420f4{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-bf544660{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bf54466a{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


mpg
```
