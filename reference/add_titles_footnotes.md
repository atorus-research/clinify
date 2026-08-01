# Add titles, footnotes, or a footnote page to a clintable or clindoc

This function allows you to attach specified titles, footnotes, or a
footnote page into clintable or clindoc object. The input can be
provided either as a list of character vectors, or pre-built flextable.

## Usage

``` r
clin_add_titles(x, ls = NULL, ft = NULL, align = NULL, tokens = NULL)

clin_add_footnotes(x, ls = NULL, ft = NULL, align = NULL, tokens = NULL)

clin_add_footnote_page(x, ls = NULL, ft = NULL, align = NULL, tokens = NULL)
```

## Arguments

- x:

  a clintable object

- ls:

  a list of character vectors, no more than 2 elements to a vector, or a
  data frame spec as described above

- ft:

  A flextable object to use as the header

- align:

  Where to place each line, as a character vector holding one value per
  element of `ls` (or a single value for all of them). Values are
  `"left"`, `"center"`, `"right"`, `"split"`, or `NA` to keep the
  default for that line. Cannot be used together with `ft`, or with a
  spec that already has an `align` column.

- tokens:

  Replacements for `{NAME}` placeholders in the text, as a named list or
  character vector - `tokens = list(FILE = "programs/t14-1-01.R")` turns
  `{FILE}` into that path. Cannot be used together with `ft`.

## Value

A clintable object

## Details

When using the `ls` parameter, each element of the list can contain no
more than two elements within each character vector. In a title, a
single element will align center. In a footnote, a single element will
align to the left. For both titles and footnotes, two elements will
align split down the middle, with the left side element aligning left
and the right side element aligning right.

Use `align` to place a line somewhere other than its default. A line
holding a single element can go `"left"`, `"center"`, or `"right"`; a
line holding two elements is split down the middle by construction,
which `align` spells `"split"`. `NA` leaves a line where it would have
landed anyway.

Instead of a list, `ls` can be a data frame holding every line for a
table at once, so one object feeds the titles, the footnotes and a
footnote page together. Each of the three functions takes the rows that
belong to it and ignores the rest, and a surface with no rows is left
alone. Rows are used in the order they are given.

|  |  |
|----|----|
| column | holds |
| `type` | `"title"`, `"footnote"`, or `"footnote_page"` (plurals accepted) |
| `text1` | the line, or its left hand side |
| `text2` | the right hand side of a split line, blank or `NA` if there is none |
| `align` | as the `align` argument below, blank or `NA` for the default |

Only `type` and `text1` are required. Reading the spec in is left to
you - it is an ordinary data frame, so it can come from a spreadsheet, a
CSV, a database, or be written out by hand.

`tokens` fills in `{NAME}` placeholders, which is how a program path or
a run date gets into text that was written somewhere else. `{PAGE}` and
`{NUMPAGES}` are left alone - those become real Word page number fields
when the table renders, so do not pass them as tokens.

## Examples

``` r
clintable(mtcars) |>
  clin_add_titles(
    list(
      c("Left", "Right"),
      c("Just the middle")
    )
  ) |>
  clin_add_titles(
    list(
      c("Protocol: ABC", "Page {PAGE} of {NUMPAGES}"),
      "Table 14-2.01",
      "Summary of Demographic and Baseline Characteristics"
    ),
    # the title line stays centered, the one below it goes left
    align = c(NA, NA, "left")
  ) |>
  clin_add_footnotes(
    list(
      c(
        "Here's a footnote.",
        format(Sys.time(), "%H:%M %A, %B %d, %Y")
      )
    )
  ) |>
  clin_add_footnote_page(
    list(
      c(
        "Use when you have a lot of footnotes",
        "And you don't want to put them on every page"
      )
    )
  )

  

.cl-834903d0{}.cl-833da31e{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-83410900{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-8341091e{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-8341091f{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-83414640{width:4.125in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-83414654{width:4.125in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-8341465e{width:4.125in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Protocol: ABC
```
