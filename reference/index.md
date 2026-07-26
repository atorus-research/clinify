# Package index

## Table Object

Make a clintable Object

- [`clintable()`](https://atorus-research.github.io/clinify/reference/clintable.md)
  : Create a new clintable object

- [`as_clintable()`](https://atorus-research.github.io/clinify/reference/as_clintable.md)
  : Convert a flextable into a clintable object

- [`clindoc()`](https://atorus-research.github.io/clinify/reference/clindoc.md)
  [`as_clindoc()`](https://atorus-research.github.io/clinify/reference/clindoc.md)
  :

  Create a `clindoc` object

## Titles and Footnotes

Attach titles or footnotes to a clintable

- [`clin_add_titles()`](https://atorus-research.github.io/clinify/reference/add_titles_footnotes.md)
  [`clin_add_footnotes()`](https://atorus-research.github.io/clinify/reference/add_titles_footnotes.md)
  [`clin_add_footnote_page()`](https://atorus-research.github.io/clinify/reference/add_titles_footnotes.md)
  : Add titles, footnotes, or a footnote page to a clintable or clindoc
- [`new_title_footnote()`](https://atorus-research.github.io/clinify/reference/new_title_footnote.md)
  : Create a new title or footnote flextable

## Pagination

Control table pagination

- [`clin_auto_page()`](https://atorus-research.github.io/clinify/reference/clin_auto_page.md)
  : Enable Word Auto-Pagination Using Group Variable
- [`clin_alt_pages()`](https://atorus-research.github.io/clinify/reference/clin_alt_pages.md)
  : Configure alternating pages during pagination of a clintable
- [`clin_page_by()`](https://atorus-research.github.io/clinify/reference/clin_page_by.md)
  : Configure pagination using a page variable
- [`clin_group_by()`](https://atorus-research.github.io/clinify/reference/clin_group_by.md)
  : Configure a clintable to table by a grouping variable, which will be
  used as a label

## Styling

Table aesthetic helpers

- [`clin_col_widths()`](https://atorus-research.github.io/clinify/reference/clin_col_widths.md)
  : Set column widths using percent
- [`clin_table_align()`](https://atorus-research.github.io/clinify/reference/clin_table_align.md)
  : Set how a table sits across the page
- [`clin_spanner_rule()`](https://atorus-research.github.io/clinify/reference/clin_spanner_rule.md)
  : Draw a rule beneath the spanners in a clintable's column headers
- [`clin_row_height()`](https://atorus-research.github.io/clinify/reference/clin_row_height.md)
  : Set the row height of a clintable
- [`clin_header_pad()`](https://atorus-research.github.io/clinify/reference/clin_header_pad.md)
  : Set the spacing around a table's column headers
- [`clin_column_headers()`](https://atorus-research.github.io/clinify/reference/clin_column_headers.md)
  : Set the column headers of the output clintable
- [`clin_replace_pagenums()`](https://atorus-research.github.io/clinify/reference/clin_replace_pagenums.md)
  : Replace Table Cells with Word Page Number Fields
- [`clin_group_pad()`](https://atorus-research.github.io/clinify/reference/clin_group_pad.md)
  : Add Padding Between Groups in a Clinical Flextable

## Style defaults

Default style functions

- [`clinify_titles_default()`](https://atorus-research.github.io/clinify/reference/clinify_defaults.md)
  [`clinify_footnotes_default()`](https://atorus-research.github.io/clinify/reference/clinify_defaults.md)
  [`clinify_table_default()`](https://atorus-research.github.io/clinify/reference/clinify_defaults.md)
  [`clinify_caption_default()`](https://atorus-research.github.io/clinify/reference/clinify_defaults.md)
  [`clinify_grouplabel_default()`](https://atorus-research.github.io/clinify/reference/clinify_defaults.md)
  [`clinify_docx_default()`](https://atorus-research.github.io/clinify/reference/clinify_defaults.md)
  : Apply Default Clinical Styling to Clintables
- [`clin_default_table_width()`](https://atorus-research.github.io/clinify/reference/clin_default_table_width.md)
  : Get the Default Table Width for Clinical Documents

## Outputs

Table rendering functions

- [`print(`*`<clintable>`*`)`](https://atorus-research.github.io/clinify/reference/print_methods.md)
  [`knit_print(`*`<clintable>`*`)`](https://atorus-research.github.io/clinify/reference/print_methods.md)
  : Clintable print method
- [`write_clindoc()`](https://atorus-research.github.io/clinify/reference/write_clindoc.md)
  : Clintable write method

- [`make_grouped_pagenums()`](https://atorus-research.github.io/clinify/reference/make_grouped_pagenums.md)
  : Assign Page Numbers to Presorted Grouped Data
