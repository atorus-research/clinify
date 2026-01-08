# Enable Word Auto-Pagination Using Group Variable

This function uses the applies the functionality
[`flextable::keep_with_next()`](https://davidgohel.github.io/flextable/reference/keep_with_next.html)
by automatically building the row indices using some grouping variable.
Each group identified by the variable (i.e. when the value of the
variable changes) will be set as a "keep_with_next" group in Word. Using
this functionality, Word will attempt not to break that group across
pages, enabling smoother pagination without having to do specific
calculations of page breaks.

## Usage

``` r
clin_auto_page(x, group_var, when = c("change", "notempty"), drop = FALSE)
```

## Arguments

- x:

  A clintable object

- group_var:

  A string containing a variable name of the input dataset used to
  calculate groups

- when:

  Character string indicating when to apply padding:

  - `"notempty"`: Find allowable break points when the value in
    `group_var` is not empty.

  - `"change"`: Find allowable break points when the value in
    `group_var` changes from the previous row.

- drop:

  Keep or drop the \`group_var“ variable

## Value

A clintable object

## Examples

``` r
clintable(mtcars) |>
  clin_auto_page("gear")


.cl-f0989748{}.cl-f08e77d6{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f0921026{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:9pt;padding-top:9pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f092103a{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f092103b{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f0923646{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f092365a{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


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
