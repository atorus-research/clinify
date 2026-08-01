# Draw a rule beneath the spanners in a clintable's column headers

Regulatory arm spanner tables carry a thin horizontal rule directly
under each spanner label, running across only the columns that spanner
covers, so that the label reads as a heading over its own block of
columns. The columns are worked out from the header that is on the
table, so the rule follows the spanners as the layout changes instead of
having to be given as column numbers that then have to be kept in step
with it.

## Usage

``` r
clin_spanner_rule(x, border = TRUE, rows = NULL)
```

## Arguments

- x:

  A clintable object

- border:

  The pen to draw the rule with. `TRUE`, the default, draws the 1pt
  solid black rule these tables conventionally use. An
  [`officer::fp_border()`](https://davidgohel.github.io/officer/reference/fp_border.html)
  draws in whatever width, style and colour it carries, which is how to
  get a dashed or a hairline rule. `FALSE` draws no rule, which is how
  to stop a house style from underlining the spanners.

- rows:

  Header rows to rule, numbered from the top down. `NULL`, the default,
  rules every row above the bottom one, so a header of any depth has all
  of its spanners underlined.

## Value

A clintable object

## Details

A spanner is a run of header cells that has been merged together, which
is what
[`clin_column_headers()`](https://atorus-research.github.io/clinify/reference/clin_column_headers.md)
makes of adjacent cells holding the same text. Two kinds of run are
deliberately left alone:

- A run of blank cells. That is the empty space over a stub column, or
  over a trailing p-value column, rather than a spanner - clinify fills
  the header levels a column does not use, so those cells merge into a
  run of their own.

- Anything in the bottom row of the header. That row holds the
  individual column labels, so a merged run in it is a label sitting
  over two columns rather than a spanner, and the rule under the bottom
  row is the one the styling function draws across the whole table.

Called a second time, this refines what the first call set rather than
replacing it: arguments this call does not name keep their earlier
value.

The rule is drawn as the table renders, after the default styling
function has run. That is what makes it survive a house style: the stock
[`clinify_table_default()`](https://atorus-research.github.io/clinify/reference/clinify_defaults.md)
opens with
[`flextable::border_remove()`](https://davidgohel.github.io/flextable/reference/border_remove.html),
which would wipe a border applied any earlier, and a house style is free
to draw its header rules in a pen of its own.

## Examples

``` r
df <- data.frame(
  stub = c("Male", "Female"),
  a_lo = c("5 (10%)", "7 (14%)"),
  a_hi = c("2 (4%)", "3 (6%)"),
  b_lo = c("6 (12%)", "8 (16%)"),
  b_hi = c("1 (2%)", "4 (8%)")
)

ct <- clintable(df, use_labels = FALSE) |>
  clin_column_headers(
    stub = "",
    a_lo = c("Drug A (N=50)", "Low"),
    a_hi = c("Drug A (N=50)", "High"),
    b_lo = c("Drug B (N=50)", "Low"),
    b_hi = c("Drug B (N=50)", "High")
  )

# A rule under each arm spanner, over that arm's two columns only, with the
# stub left un-ruled
clin_spanner_rule(ct)


.cl-995ed588{}.cl-99580f6e{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-995b0700{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:9pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-995b0714{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:9pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-995b0715{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-995b2d7a{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-995b2d8e{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-995b2d98{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-995b2d99{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-995b2d9a{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


```
