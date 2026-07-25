# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this package is

**{clinify}** is an R package (Atorus Research) that extends **{flextable}** and **{officer}** to simplify producing regulatory clinical tables, listings, and figures, with Word (`.docx`) as the first-class output. The guiding constraint (see `README.md` "Design Philosophy"): clinify objects *inherit* from flextable/officer objects and must never break native flextable/officer behavior. Lifecycle is experimental; it is on CRAN (version `0.3.0.9000` = dev).

## Development commands

Standard devtools-based R package (`clinify.Rproj` has `PackageUseDevtools: Yes`). Run from the package root:

```r
devtools::load_all()        # load package for interactive dev (no install)
devtools::document()        # regenerate man/*.Rd + NAMESPACE from roxygen — run after changing any roxygen
devtools::test()            # run all tests
devtools::test(filter = "pagination")   # run one file: matches tests/testthat/test-pagination.R
devtools::check()           # full R CMD check (what CI runs)
lintr::lint_package()       # lint (.lintr = default linters)
devtools::build_readme()    # rebuild README.md from README.Rmd (NEVER edit README.md directly)
```

Snapshot tests dominate this suite (see below). To work with them:

```r
testthat::snapshot_review()   # review pending snapshot diffs interactively
testthat::snapshot_accept()   # accept new snapshots after an intentional change
```

CI (`.github/workflows/R-CMD-check.yaml`) runs `rcmdcheck` on Windows/macOS/Linux × R release+devel and uploads snapshots on failure. pkgdown site + test-coverage + rhub workflows also exist.

## Core architecture

### The central idea: deferred config on a flextable

A `clintable` **is a flextable** with an extra `x$clinify_config` list attached. Every user-facing `clin_*()` builder verb does almost nothing except mutate fields in `clinify_config` and return the object. No pagination, slicing, or styling is computed until a **terminal action** runs (`print()`, `knit_print()`, `write_clindoc()`, or `clindoc()`). Understanding any feature means tracing (1) which config field the `clin_*()` setter writes, and (2) where the renderer reads it. Key config fields: `pagination_method` (`"default"`/`"custom"`), `page_by`, `max_rows`, `group_by`, `caption_by`, `group_when`, `key_cols`, `col_groups`, `auto_page_var`, `titles`, `footnotes`, `footnote_page`, and the computed `pagination_idx`.

### Object model (all inherit from flextable/officer)

- **`clintable`** (`R/clintable.R`) — inherits `flextable`. Built by `clintable(df, ...)` or `as_clintable(ft)`. Holds `clinify_config`.
- **`clinpage`** (`R/clinpage.R`) — inherits `flextable`. One rendered page = a row/column subset of a clintable. Produced by `slice_clintable()`.
- **`clindoc`** (`R/clindoc.R`) — inherits `officer::rdocx`. The final document; can hold multiple clintables separated by page breaks. Built by `clindoc(...)` / `as_clindoc()`.

### Builder verbs → config (then rendered lazily)

Defined mainly in `R/pagination.R`, `R/col_width.R`, `R/column_headers.R`, `R/add_titles_footnotes.R`, `R/group_pad.R`:
`clin_page_by`, `clin_group_by`, `clin_alt_pages`, `clin_auto_page`, `clin_col_widths`, `clin_column_headers`, `clin_group_pad`, `clin_add_titles`, `clin_add_footnotes`, `clin_add_footnote_page`. Any of `clin_page_by/group_by/alt_pages` flips `pagination_method` to `"custom"`.

### Pagination engine (`R/pagination.R`) — most complex subsystem

`prep_pagination_()` is the render-time driver. It produces `pagination_idx`: a list of pages, each `list(rows, cols, label, captions)`, via `make_ind_list()`. It combines:
- **Row vectors** (`page_vecs`): `page_by_()` splits where the page variable changes (and at group starts); `max_rows_()` splits every N rows while respecting group boundaries.
- **Column vectors** (`col_vecs`): `alt_pages_()` for "alternating pages" (repeat `key_cols` on each page, then append one `col_group` per page) — this is how wide tables are split across pages; otherwise all columns.
- Group labels and captions are carried forward across rows with `zoo::na.locf`.

`clin_auto_page()` is a *separate* pagination strategy: instead of pre-slicing, it flags rows with flextable's `keep_with_next()` so Word itself avoids breaking groups across pages (`auto_page_()`).

### Slicing (`R/slice_clintable.R`) — the other complex subsystem, fragile

flextable has no native way to cut an *already-styled* table into page chunks while preserving styles/spans/borders, so `slice_clintable()` reimplements it by reaching directly into flextable's internal structures (`$body$dataset`, `complex_tabpart`, `fpstruct`, `chunkset_struct`, `$spans$rows`, `$styles$cells/pars/text`). `adjust_span_row()` repairs header spanners that get cut at a column page break; `reapply_bottom_border()` pulls the table's bottom border onto each slice. **This code is tightly coupled to flextable/officer internals** — git history shows repeated breakage on officer/flextable upgrades (DESCRIPTION pins `officer (>= 0.7.2)`). Treat flextable/officer version bumps as high-risk for slicing + snapshot tests.

### Defaults / options system (`R/styles.R`, `R/zzz.R`, `inst/defaults_template.R`)

`.onLoad` populates six options that are read at render time via `getOption(...)`:
`clinify_docx_default` (an `officer::prop_section` for page size/margins/orientation — landscape by default), and five **styling functions** `clinify_titles_default`, `clinify_footnotes_default`, `clinify_table_default`, `clinify_caption_default`, `clinify_grouplabel_default`. Organizations override defaults by assigning their own functions to these options (see `vignette("defaults")` and `inst/defaults_template.R`). `clin_default_table_width()` derives usable width from the docx section (page width − margins) and underpins the percent-based widths in `clin_col_widths()`.

### Rendering paths (where config becomes output)

- **HTML preview** — `print.clintable` / `knit_print.clintable` → `clintable_as_html()` (`R/print.R`). Renders `n` pages (default 3) with a small JS page-switcher; titles/footnotes are rendered as separate flextables and concatenated into the body HTML.
- **Word** — `write_clindoc()` (`R/write.R`) → `as_clindoc()`/`clindoc()` → `add_clintable_()` (`R/clindoc.R`), using officer's `body_append_*_context` API. Titles/footnotes become the section's `header_default`/`footer_default` so Word **repeats them on every page**; alternating pages are emitted with page breaks between slices.

### Column headers & page numbers

- Multi-level headers via `clin_column_headers()` (named character vectors, applied bottom-up) or automatically from dataframe column labels using `||` as the level delimiter (`use_labels = TRUE` default → `headers_from_labels_()`). Spanners form by merging equal-valued header cells horizontally and vertically.
- `clin_replace_pagenums()` (`R/pagenums.R`) swaps `{PAGE}`/`{NUMPAGES}` placeholders for real Word field objects; it is invoked inside the title/footnote default styling functions.

## Conventions

- **Naming**: exported user verbs are `clin_*`; overridable default-style functions are `clinify_*_default`; a **trailing underscore marks internal/non-exported helpers** (`prep_pagination_`, `page_by_`, `group_by_`, `auto_page_`, `add_clintable_`, `headers_from_labels_`, …).
- **Code style**: consistent with the **Air** formatter (`.vscode/settings.json` enables format-on-save). Match it: 2-space indent, native pipe `|>`, `\(x)` lambdas, one argument per line in multi-line calls. Fully-qualify external calls (`flextable::`, `officer::`).
- **roxygen2 with markdown** (`Roxygen: list(markdown = TRUE)`); always `devtools::document()` after edits — `man/` and `NAMESPACE` are generated, never hand-edit.
- **Tests are snapshot-heavy** (testthat edition 3). Many tests render to a `withr::local_tempfile(fileext = ".docx")`, re-read it with `officer::read_docx()`, and `expect_snapshot()` the result; others snapshot HTML or assert `prep_pagination_()` index structures directly (`test-pagination.R` is the largest and the best reference for the pagination contract). A snapshot diff after a deliberate change is expected — review then `snapshot_accept()`.
- User-facing changes go in `NEWS.md`; reference the GitHub issue number.
