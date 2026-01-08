# Create a `clindoc` object

These functions handle the conversion of a `clintable` object into a
`clindoc` object.

## Usage

``` r
clindoc(...)

as_clindoc(x)
```

## Arguments

- ...:

  `clintable` objects to be converted. Or separately, a list of
  `clintable` objects

- x:

  A `clintable` object to be converted.

## Value

a `clindoc` object, inherited from an `officer::rdocx` object

## Details

- `as_clindoc()` is intended for a single clintable object and extracts
  all necessary title and footnote information to apply to the document

- `clindoc()` can accept `clintable` objects as separate parameters or
  as a list of `clintable` objects. If a single `clintable` is passed,
  this function simply calls `as_clindoc()` internally.

When a multiple `clintable` objects are passed to `clindoc()`, titles
and footnotes should be applied directly to the `clindoc` object using
`clin_add_title()`, `clin_add_footnote()`, or
[`clin_add_footnote_page()`](https://atorus-research.github.io/clinify/reference/add_titles_footnotes.md).
Title and footnote information on the individual `clintable` objects
will be ignored.

## Examples

``` r
ct <- clintable(mtcars)

clindoc(ct)
#> rdocx document with 2 element(s)
#> 
#> * styles:
#>                 Normal              heading 1              heading 2 
#>            "paragraph"            "paragraph"            "paragraph" 
#>              heading 3 Default Paragraph Font           Normal Table 
#>            "paragraph"            "character"                "table" 
#>                No List                 strong               centered 
#>            "numbering"            "character"            "paragraph" 
#>         table_template    Light List Accent 2            Titre 1 Car 
#>                "table"                "table"            "character" 
#>            Titre 2 Car            Titre 3 Car          Image Caption 
#>            "character"            "character"            "paragraph" 
#>          Table Caption     Table Professional                  toc 1 
#>            "paragraph"                "table"            "paragraph" 
#>                  toc 2           Balloon Text    Texte de bulles Car 
#>            "paragraph"            "paragraph"            "character" 
#>           reference_id          graphic title            table title 
#>            "character"            "paragraph"            "paragraph" 
```
