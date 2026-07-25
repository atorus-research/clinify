# clinify 0.4.0

- `clin_column_headers()` gained a `merge` argument to control the automatic merging of identical, adjacent header cells. Use `merge = "spanners"` to keep the bottom row of the header out of it, `merge = FALSE` to turn merging off entirely, or a vector of header row numbers for finer control. This lets a header row that legitimately repeats a label across adjacent columns keep those cells separate ([#95](https://github.com/atorus-research/clinify/issues/95))
- `clin_column_headers()` can now be called with only the `merge` argument and no header text, which adjusts the merging of headers already in place - including headers built from column labels ([#95](https://github.com/atorus-research/clinify/issues/95))
- The `clinify_table_default()` in `defaults_template.R` no longer merges header cells horizontally, which would have overridden the `merge` argument of `clin_column_headers()` at render time
- Because `merge` is now a parameter of `clin_column_headers()`, a column named `merge` can no longer be given a header through `...`. Use column labels for that column instead. This is a breaking change.
- Fixed merged cells being left in an invalid state when a table is paginated column wise with `clin_alt_pages()`. Merges are now recalculated for every row of the header, for the table body, and for merges running vertically, where previously only the top header row was corrected. Symptoms included header text disappearing from later pages, cells claiming to span more columns than the page has, and `write_clindoc()` failing with "missing value where TRUE/FALSE needed" when a page ended in a split spanner
- Fixed a single column table losing all but the top level of a multi level column header
- Added `clin_header_pad()` to set the vertical space around the column headers: `above` and `below` each header row, and `rule_to_body` between the rule under the header and the first body row. The pieces are named for where the space sits because a cell's bottom border is drawn below its bottom padding, so padding under the header moves the rule toward the body rather than opening space beneath it - the space under the rule has to come from the body side. `above` and `below` apply to every header row, so a spanned header keeps the space between its levels, and `rule_to_body` is applied to the first row of every page ([#97](https://github.com/atorus-research/clinify/issues/97))
- Added `clin_row_height()` to set the row pitch of a table's body, titles, and footnotes. Regulatory outputs are specified to an exact pitch, and rendered row height is what decides how much fits on a page; {flextable} leaves rows at a nominal quarter inch with a rule of "auto". Pitches are given in points by default, and the rule defaults to `"atleast"` so a cell whose text wraps grows past the pitch instead of being clipped ([#97](https://github.com/atorus-research/clinify/issues/97))
- Title and footnote row pitch is now reachable from the `ls =` argument of `clin_add_titles()` and `clin_add_footnotes()`. Previously the only way to set it was to hand build a flextable and pass it as `ft =` ([#97](https://github.com/atorus-research/clinify/issues/97))
- Group label and caption rows, which clinify inserts as it renders, take the body pitch ([#97](https://github.com/atorus-research/clinify/issues/97))
- Whatever a table itself is configured with is now applied after the default styling function rather than before it, so a per table setting beats an organisation's house style, which beats the {flextable} default. Previously a house `clinify_table_default()` that set a row height silently overrode an explicit one ([#97](https://github.com/atorus-research/clinify/issues/97), [#98](https://github.com/atorus-research/clinify/issues/98))
- Fixed pagination discarding a table's configuration when it dropped the `page_by`, `group_by` or `caption_by` columns
- Fixed the default styling functions discarding a table's other properties when they fixed its layout. `flextable::set_table_properties()` rebuilds the whole property list, so a table's alignment on the page, its width, its Word accessibility fields and its `opts_word`/`opts_html` settings were all quietly reset at render time ([#98](https://github.com/atorus-research/clinify/issues/98))
- Added `clin_table_align()` to set how a table sits across the page. It is applied after the default styling function, so it holds even when an organisation's own `clinify_table_default()` rebuilds the table properties ([#98](https://github.com/atorus-research/clinify/issues/98))
- `clin_add_titles()`, `clin_add_footnotes()`, `clin_add_footnote_page()` and `new_title_footnote()` gained an `align` argument to place each line, so a single left aligned title no longer needs its text passed twice ([#98](https://github.com/atorus-research/clinify/issues/98))
- A title or footnote line with no elements now gives a clear error instead of failing inside `data.frame()`
- The `clinify_table_default()` in `defaults_template.R` and in `vignette("defaults")` no longer rebuild the table properties either. Organisations that copied the template will want to pick up the change

# clinify 0.3.1

- Updated for compatibility with {officer} (>= 0.7.0); the minimum required {officer} version is now 0.7.2
- Group label styling now retains the configured font size
- Fixed style application when column vectors were simplified incorrectly
- Fixed slicing when a table is subset to a single column
- Fixed handling of spanning header gaps when a spanner is split across pages
- Made internal table-slicing tests robust to platform-dependent font-metric estimates (resolves an R CMD check error on r-devel-linux-x86_64-fedora-gcc)

# clinify 0.3.0

- Incorporate performance improvements from {officer} package updates 
- Fix issue with `clin_col_widths()` where numeric precision issues could cause unexpected errors
- Group labels can be formatted and by default moved above header line ([#79](https://github.com/atorus-research/clinify/issues/79))
- Added `make_grouped_pagenums()` helper function
- Added function to create `clindoc()` objects specifically, and multiple tables can be passed to `clindoc()` ([#63](https://github.com/atorus-research/clinify/issues/63), [#80](https://github.com/atorus-research/clinify/issues/80))
- Titles and footnotes now only split into two parts to avoid line wrapping ([#69](https://github.com/atorus-research/clinify/issues/69)). This is a breaking change.
- Added `clin_group_pad()` function ([#72](https://github.com/atorus-research/clinify/issues/72))
- Updated group tracking in `clin_group_by()`, `clin_auto_page()` and `clin_group_pad()` to find groups by changed values or by populated values.
- `clin_auto_page()` now has an option to drop the `page_by` variable. Does not drop variable by default. This is a breaking change.
- Renamed `write_clintable()` to `write_clindoc()`. This is a breaking change.
- New vignette on Document Objects and Saving

# clinify 0.2.0

- Added in body captions ([#26](https://github.com/atorus-research/clinify/issues/26) and [#28](https://github.com/atorus-research/clinify/issues/28))
- Dedicated footnote pages [#29](https://github.com/atorus-research/clinify/issues/29)
- Fixed column header bugs [#61](https://github.com/atorus-research/clinify/issues/61)
- Allow return as `rdocx` object [#63](https://github.com/atorus-research/clinify/issues/63)
- Automatic page numbers [#4](https://github.com/atorus-research/clinify/issues/4)
- Added `clin_auto_page()` function for auto pagination using word's `keep_with_next` ([#16](https://github.com/atorus-research/clinify/issues/16))
- Several new vignettes

# clinify 0.1.2 

CRAN review comments, including:
- Updated description file for reference links
- Updated help documentation `dontrun{}` examples avoid unnecessary use of `dontrun{}`
- Updated help example to not write file to local directory, and ensured all tests and vignettes write to files in `tempdir()`

# clinify 0.1.1

CRAN review comment updates in DESCRIPTION file

# clinify 0.1.0 

Initial CRAN submission