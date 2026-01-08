# Configure a clintable to table by a grouping variable, which will be used as a label

Configure a clintable to table by a grouping variable, which will be
used as a label

## Usage

``` r
clin_group_by(x, group_by, caption_by = NULL, when = c("change", "notempty"))
```

## Arguments

- x:

  A clintable object

- group_by:

  A character vector of variable names which will be used for grouping
  and attached as a label above the table headers

- caption_by:

  A single element string of a variable name which will be used as a
  caption attached below the table body and above in the footer.
  Defaults to NULL.

- when:

  Character string indicating how to identify groups and captions:

  - `"change"`: Add padding when the value in `group_by` or `caption_by`
    changes from the previous row.

  - `"notempty"`: Add padding when the value in `group_by` or
    `caption_by` is not empty.

## Value

A clintable object

## Examples

``` r
clintable(iris) |>
  clin_group_by("Species")

  

.cl-f1ba33de{}.cl-f1b30eb0{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f1b62adc{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:9pt;padding-top:9pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f1b62ae6{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:9pt;padding-top:9pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f1b62ae7{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f1b64ec2{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f1b64ecc{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f1b64ed6{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f1b64ed7{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


setosa
```

Sepal.Length

Sepal.Width

Petal.Length

Petal.Width

5.1

3.5

1.4

0.2

| setosa       |             |              |             |
|--------------|-------------|--------------|-------------|
| Sepal.Length | Sepal.Width | Petal.Length | Petal.Width |
| 4.9          | 3.0         | 1.4          | 0.2         |

| setosa       |             |              |             |
|--------------|-------------|--------------|-------------|
| Sepal.Length | Sepal.Width | Petal.Length | Petal.Width |
| 4.7          | 3.2         | 1.3          | 0.2         |

1

2

3
