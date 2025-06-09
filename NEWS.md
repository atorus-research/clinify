# clinify 0.3.0

- Incorporate performance improvements from {officer} package updates 
- Fix issue with clin_col_widths() where numeric precision issues could cause unexpected errors
- Add new helper function make_grouped_pagenums() to ease creation of a paging variable, which is required for clin_group_by() or it will just break every 20 rows
- Group labels can be formatted and by default moved above header line ([#79](https://github.com/atorus-research/clinify/issues/79))
- Fix numeric precision issue on `clin_col_widths()`
- Added `make_grouped_pagenums()` helper function
- Added function to create `clindoc()` objects specifically, and multiple tables can be passed to `clindoc()` ([#63](https://github.com/atorus-research/clinify/issues/63), [#80](https://github.com/atorus-research/clinify/issues/80))
- Titles and footnotes now only split into two parts to avoid line wrapping ([#69](https://github.com/atorus-research/clinify/issues/69)).  This is a breaking change.
- Added `clin_group_pad()` function ([#72](https://github.com/atorus-research/clinify/issues/72))
- Updated group tracking in `clin_group_by()`, `clin_auto_page()` and `clin_group_pad()` to find groups by changed values or by populated values.
- `clin_auto_page()` now has an option to drop the `page_by` variable. Does not drop variable by default. 
- New vignette on Document Objects and Saving
- Renamed `write_clintable()` to `write_clindoc()`. This is a breaking change.

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