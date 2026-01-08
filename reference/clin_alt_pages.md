# Configure alternating pages during pagination of a clintable

This function configures alternating pages on a clintable object.

## Usage

``` r
clin_alt_pages(x, key_cols, col_groups)
```

## Arguments

- x:

  A clintable object

- key_cols:

  A character vector of variable names

- col_groups:

  A list of character vectors of variable names

## Value

A clintable object

## Examples

``` r
ct <- clintable(mtcars)

clin_alt_pages(
  ct,
  key_cols = c("mpg", "cyl", "hp"),
  col_groups = list(
    c("disp", "drat", "wt"),
    c("qsec", "vs", "am"),
    c("gear", "carb")
  )
)
#> NOTE: Alternating pages were set, but no selection for row wise pagination was configured Defaulting to 20 rows per page.

  

.cl-f035b222{}.cl-f0287526{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f02bf08e{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:9pt;padding-top:9pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f02bf142{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f02c15b4{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f02c15be{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


mpg
```

cyl

hp

disp

drat

wt

21.0

6

110

160.0

3.90

2.620

21.0

6

110

160.0

3.90

2.875

22.8

4

93

108.0

3.85

2.320

21.4

6

110

258.0

3.08

3.215

18.7

8

175

360.0

3.15

3.440

18.1

6

105

225.0

2.76

3.460

14.3

8

245

360.0

3.21

3.570

24.4

4

62

146.7

3.69

3.190

22.8

4

95

140.8

3.92

3.150

19.2

6

123

167.6

3.92

3.440

17.8

6

123

167.6

3.92

3.440

16.4

8

180

275.8

3.07

4.070

17.3

8

180

275.8

3.07

3.730

15.2

8

180

275.8

3.07

3.780

10.4

8

205

472.0

2.93

5.250

10.4

8

215

460.0

3.00

5.424

14.7

8

230

440.0

3.23

5.345

32.4

4

66

78.7

4.08

2.200

30.4

4

52

75.7

4.93

1.615

33.9

4

65

71.1

4.22

1.835

| mpg  | cyl | hp  | qsec  | vs  | am  |
|------|-----|-----|-------|-----|-----|
| 21.0 | 6   | 110 | 16.46 | 0   | 1   |
| 21.0 | 6   | 110 | 17.02 | 0   | 1   |
| 22.8 | 4   | 93  | 18.61 | 1   | 1   |
| 21.4 | 6   | 110 | 19.44 | 1   | 0   |
| 18.7 | 8   | 175 | 17.02 | 0   | 0   |
| 18.1 | 6   | 105 | 20.22 | 1   | 0   |
| 14.3 | 8   | 245 | 15.84 | 0   | 0   |
| 24.4 | 4   | 62  | 20.00 | 1   | 0   |
| 22.8 | 4   | 95  | 22.90 | 1   | 0   |
| 19.2 | 6   | 123 | 18.30 | 1   | 0   |
| 17.8 | 6   | 123 | 18.90 | 1   | 0   |
| 16.4 | 8   | 180 | 17.40 | 0   | 0   |
| 17.3 | 8   | 180 | 17.60 | 0   | 0   |
| 15.2 | 8   | 180 | 18.00 | 0   | 0   |
| 10.4 | 8   | 205 | 17.98 | 0   | 0   |
| 10.4 | 8   | 215 | 17.82 | 0   | 0   |
| 14.7 | 8   | 230 | 17.42 | 0   | 0   |
| 32.4 | 4   | 66  | 19.47 | 1   | 1   |
| 30.4 | 4   | 52  | 18.52 | 1   | 1   |
| 33.9 | 4   | 65  | 19.90 | 1   | 1   |

| mpg  | cyl | hp  | gear | carb |
|------|-----|-----|------|------|
| 21.0 | 6   | 110 | 4    | 4    |
| 21.0 | 6   | 110 | 4    | 4    |
| 22.8 | 4   | 93  | 4    | 1    |
| 21.4 | 6   | 110 | 3    | 1    |
| 18.7 | 8   | 175 | 3    | 2    |
| 18.1 | 6   | 105 | 3    | 1    |
| 14.3 | 8   | 245 | 3    | 4    |
| 24.4 | 4   | 62  | 4    | 2    |
| 22.8 | 4   | 95  | 4    | 2    |
| 19.2 | 6   | 123 | 4    | 4    |
| 17.8 | 6   | 123 | 4    | 4    |
| 16.4 | 8   | 180 | 3    | 3    |
| 17.3 | 8   | 180 | 3    | 3    |
| 15.2 | 8   | 180 | 3    | 3    |
| 10.4 | 8   | 205 | 3    | 4    |
| 10.4 | 8   | 215 | 3    | 4    |
| 14.7 | 8   | 230 | 3    | 4    |
| 32.4 | 4   | 66  | 4    | 1    |
| 30.4 | 4   | 52  | 4    | 2    |
| 33.9 | 4   | 65  | 4    | 1    |

1

2

3
