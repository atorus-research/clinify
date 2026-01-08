# Create a new clintable object

A clintable object directly inherits from a flextable object. This
function will pass all necessary parameters
[`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)
and conver the object to a `clintable`

## Usage

``` r
clintable(x, page_by = NULL, group_by = NULL, use_labels = TRUE, ...)
```

## Arguments

- x:

  A data frame

- page_by:

  A variable in the input dataframe to use for pagination

- group_by:

  A character vector of variable names which will be used for grouping
  and attached as a label above the table headers

- use_labels:

  Use variable labels as column headers. Nested levels can be achieved
  using the string "\|\|" as a delimitter. Horizontal and vertical
  levels using identical words will be merged.

- ...:

  Parameters to pass to
  [`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)

## Value

A clintable object

## Examples

``` r
clintable(mtcars)


.cl-f39b95c6{}.cl-f3921640{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f3955bd4{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:9pt;padding-top:9pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f3955be8{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f3957f56{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f3957f60{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


mpg
```

cyl

disp

hp

drat

wt

qsec

vs

am

gear

carb

21.0

6

160.0

110

3.90

2.620

16.46

0

1

4

4

21.0

6

160.0

110

3.90

2.875

17.02

0

1

4

4

22.8

4

108.0

93

3.85

2.320

18.61

1

1

4

1

21.4

6

258.0

110

3.08

3.215

19.44

1

0

3

1

18.7

8

360.0

175

3.15

3.440

17.02

0

0

3

2

18.1

6

225.0

105

2.76

3.460

20.22

1

0

3

1

14.3

8

360.0

245

3.21

3.570

15.84

0

0

3

4

24.4

4

146.7

62

3.69

3.190

20.00

1

0

4

2

22.8

4

140.8

95

3.92

3.150

22.90

1

0

4

2

19.2

6

167.6

123

3.92

3.440

18.30

1

0

4

4

17.8

6

167.6

123

3.92

3.440

18.90

1

0

4

4

16.4

8

275.8

180

3.07

4.070

17.40

0

0

3

3

17.3

8

275.8

180

3.07

3.730

17.60

0

0

3

3

15.2

8

275.8

180

3.07

3.780

18.00

0

0

3

3

10.4

8

472.0

205

2.93

5.250

17.98

0

0

3

4
