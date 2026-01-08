# Apply Default Clinical Styling to Clintables

These functions apply default styling to `clintable` objects used for
clinical tables, including titles and footnotes. The styling includes
removing borders, setting font properties, and adjusting table width,
line spacing, and padding.

## Usage

``` r
clinify_titles_default(x, ...)

clinify_footnotes_default(x, ...)

clinify_table_default(x, ...)

clinify_caption_default(x, ...)

clinify_grouplabel_default(x, ...)

clinify_docx_default()
```

## Arguments

- x:

  A `clintable` object representing the table (title or footnote).

- ...:

  Additional arguments (currently unused).

## Value

A `clintable` object with the applied styling.

## Examples

``` r
op <- options()

sect <- clinify_docx_default()

# Save out options to grab defaults
options(
  clinify_docx_default = sect,
  clinify_titles_default = clinify_titles_default,
  clinify_footnotes_default = clinify_footnotes_default,
  clinify_table_default = clinify_table_default,
  clinify_caption_default = clinify_caption_default,
  clinify_grouplabel_default = clinify_grouplabel_default
)

options(op)
```
