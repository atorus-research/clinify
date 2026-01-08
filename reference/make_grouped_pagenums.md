# Assign Page Numbers to Presorted Grouped Data

Assigns sequential page numbers to elements of a vector, grouping by
unique values and allocating a specified number of rows per page. The
input vector must be presorted by group.

## Usage

``` r
make_grouped_pagenums(var, rows)
```

## Arguments

- var:

  A vector of group labels, presorted so that identical values are
  contiguous.

- rows:

  Integer. The maximum number of rows per page.

## Value

An integer vector of the same length as \`var“, indicating the assigned
page number for each element.

## Details

The function splits the input vector into groups, then assigns page
numbers within each group so that each page contains up to \`rows“
items. Page numbers increment sequentially across groups. If the input
is not presorted by group, the function will throw an error.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
iris |>
  mutate(
    page = make_grouped_pagenums(Species, 5)
  )
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species page
#> 1            5.1         3.5          1.4         0.2     setosa    1
#> 2            4.9         3.0          1.4         0.2     setosa    1
#> 3            4.7         3.2          1.3         0.2     setosa    1
#> 4            4.6         3.1          1.5         0.2     setosa    1
#> 5            5.0         3.6          1.4         0.2     setosa    1
#> 6            5.4         3.9          1.7         0.4     setosa    2
#> 7            4.6         3.4          1.4         0.3     setosa    2
#> 8            5.0         3.4          1.5         0.2     setosa    2
#> 9            4.4         2.9          1.4         0.2     setosa    2
#> 10           4.9         3.1          1.5         0.1     setosa    2
#> 11           5.4         3.7          1.5         0.2     setosa    3
#> 12           4.8         3.4          1.6         0.2     setosa    3
#> 13           4.8         3.0          1.4         0.1     setosa    3
#> 14           4.3         3.0          1.1         0.1     setosa    3
#> 15           5.8         4.0          1.2         0.2     setosa    3
#> 16           5.7         4.4          1.5         0.4     setosa    4
#> 17           5.4         3.9          1.3         0.4     setosa    4
#> 18           5.1         3.5          1.4         0.3     setosa    4
#> 19           5.7         3.8          1.7         0.3     setosa    4
#> 20           5.1         3.8          1.5         0.3     setosa    4
#> 21           5.4         3.4          1.7         0.2     setosa    5
#> 22           5.1         3.7          1.5         0.4     setosa    5
#> 23           4.6         3.6          1.0         0.2     setosa    5
#> 24           5.1         3.3          1.7         0.5     setosa    5
#> 25           4.8         3.4          1.9         0.2     setosa    5
#> 26           5.0         3.0          1.6         0.2     setosa    6
#> 27           5.0         3.4          1.6         0.4     setosa    6
#> 28           5.2         3.5          1.5         0.2     setosa    6
#> 29           5.2         3.4          1.4         0.2     setosa    6
#> 30           4.7         3.2          1.6         0.2     setosa    6
#> 31           4.8         3.1          1.6         0.2     setosa    7
#> 32           5.4         3.4          1.5         0.4     setosa    7
#> 33           5.2         4.1          1.5         0.1     setosa    7
#> 34           5.5         4.2          1.4         0.2     setosa    7
#> 35           4.9         3.1          1.5         0.2     setosa    7
#> 36           5.0         3.2          1.2         0.2     setosa    8
#> 37           5.5         3.5          1.3         0.2     setosa    8
#> 38           4.9         3.6          1.4         0.1     setosa    8
#> 39           4.4         3.0          1.3         0.2     setosa    8
#> 40           5.1         3.4          1.5         0.2     setosa    8
#> 41           5.0         3.5          1.3         0.3     setosa    9
#> 42           4.5         2.3          1.3         0.3     setosa    9
#> 43           4.4         3.2          1.3         0.2     setosa    9
#> 44           5.0         3.5          1.6         0.6     setosa    9
#> 45           5.1         3.8          1.9         0.4     setosa    9
#> 46           4.8         3.0          1.4         0.3     setosa   10
#> 47           5.1         3.8          1.6         0.2     setosa   10
#> 48           4.6         3.2          1.4         0.2     setosa   10
#> 49           5.3         3.7          1.5         0.2     setosa   10
#> 50           5.0         3.3          1.4         0.2     setosa   10
#> 51           7.0         3.2          4.7         1.4 versicolor   11
#> 52           6.4         3.2          4.5         1.5 versicolor   11
#> 53           6.9         3.1          4.9         1.5 versicolor   11
#> 54           5.5         2.3          4.0         1.3 versicolor   11
#> 55           6.5         2.8          4.6         1.5 versicolor   11
#> 56           5.7         2.8          4.5         1.3 versicolor   12
#> 57           6.3         3.3          4.7         1.6 versicolor   12
#> 58           4.9         2.4          3.3         1.0 versicolor   12
#> 59           6.6         2.9          4.6         1.3 versicolor   12
#> 60           5.2         2.7          3.9         1.4 versicolor   12
#> 61           5.0         2.0          3.5         1.0 versicolor   13
#> 62           5.9         3.0          4.2         1.5 versicolor   13
#> 63           6.0         2.2          4.0         1.0 versicolor   13
#> 64           6.1         2.9          4.7         1.4 versicolor   13
#> 65           5.6         2.9          3.6         1.3 versicolor   13
#> 66           6.7         3.1          4.4         1.4 versicolor   14
#> 67           5.6         3.0          4.5         1.5 versicolor   14
#> 68           5.8         2.7          4.1         1.0 versicolor   14
#> 69           6.2         2.2          4.5         1.5 versicolor   14
#> 70           5.6         2.5          3.9         1.1 versicolor   14
#> 71           5.9         3.2          4.8         1.8 versicolor   15
#> 72           6.1         2.8          4.0         1.3 versicolor   15
#> 73           6.3         2.5          4.9         1.5 versicolor   15
#> 74           6.1         2.8          4.7         1.2 versicolor   15
#> 75           6.4         2.9          4.3         1.3 versicolor   15
#> 76           6.6         3.0          4.4         1.4 versicolor   16
#> 77           6.8         2.8          4.8         1.4 versicolor   16
#> 78           6.7         3.0          5.0         1.7 versicolor   16
#> 79           6.0         2.9          4.5         1.5 versicolor   16
#> 80           5.7         2.6          3.5         1.0 versicolor   16
#> 81           5.5         2.4          3.8         1.1 versicolor   17
#> 82           5.5         2.4          3.7         1.0 versicolor   17
#> 83           5.8         2.7          3.9         1.2 versicolor   17
#> 84           6.0         2.7          5.1         1.6 versicolor   17
#> 85           5.4         3.0          4.5         1.5 versicolor   17
#> 86           6.0         3.4          4.5         1.6 versicolor   18
#> 87           6.7         3.1          4.7         1.5 versicolor   18
#> 88           6.3         2.3          4.4         1.3 versicolor   18
#> 89           5.6         3.0          4.1         1.3 versicolor   18
#> 90           5.5         2.5          4.0         1.3 versicolor   18
#> 91           5.5         2.6          4.4         1.2 versicolor   19
#> 92           6.1         3.0          4.6         1.4 versicolor   19
#> 93           5.8         2.6          4.0         1.2 versicolor   19
#> 94           5.0         2.3          3.3         1.0 versicolor   19
#> 95           5.6         2.7          4.2         1.3 versicolor   19
#> 96           5.7         3.0          4.2         1.2 versicolor   20
#> 97           5.7         2.9          4.2         1.3 versicolor   20
#> 98           6.2         2.9          4.3         1.3 versicolor   20
#> 99           5.1         2.5          3.0         1.1 versicolor   20
#> 100          5.7         2.8          4.1         1.3 versicolor   20
#> 101          6.3         3.3          6.0         2.5  virginica   21
#> 102          5.8         2.7          5.1         1.9  virginica   21
#> 103          7.1         3.0          5.9         2.1  virginica   21
#> 104          6.3         2.9          5.6         1.8  virginica   21
#> 105          6.5         3.0          5.8         2.2  virginica   21
#> 106          7.6         3.0          6.6         2.1  virginica   22
#> 107          4.9         2.5          4.5         1.7  virginica   22
#> 108          7.3         2.9          6.3         1.8  virginica   22
#> 109          6.7         2.5          5.8         1.8  virginica   22
#> 110          7.2         3.6          6.1         2.5  virginica   22
#> 111          6.5         3.2          5.1         2.0  virginica   23
#> 112          6.4         2.7          5.3         1.9  virginica   23
#> 113          6.8         3.0          5.5         2.1  virginica   23
#> 114          5.7         2.5          5.0         2.0  virginica   23
#> 115          5.8         2.8          5.1         2.4  virginica   23
#> 116          6.4         3.2          5.3         2.3  virginica   24
#> 117          6.5         3.0          5.5         1.8  virginica   24
#> 118          7.7         3.8          6.7         2.2  virginica   24
#> 119          7.7         2.6          6.9         2.3  virginica   24
#> 120          6.0         2.2          5.0         1.5  virginica   24
#> 121          6.9         3.2          5.7         2.3  virginica   25
#> 122          5.6         2.8          4.9         2.0  virginica   25
#> 123          7.7         2.8          6.7         2.0  virginica   25
#> 124          6.3         2.7          4.9         1.8  virginica   25
#> 125          6.7         3.3          5.7         2.1  virginica   25
#> 126          7.2         3.2          6.0         1.8  virginica   26
#> 127          6.2         2.8          4.8         1.8  virginica   26
#> 128          6.1         3.0          4.9         1.8  virginica   26
#> 129          6.4         2.8          5.6         2.1  virginica   26
#> 130          7.2         3.0          5.8         1.6  virginica   26
#> 131          7.4         2.8          6.1         1.9  virginica   27
#> 132          7.9         3.8          6.4         2.0  virginica   27
#> 133          6.4         2.8          5.6         2.2  virginica   27
#> 134          6.3         2.8          5.1         1.5  virginica   27
#> 135          6.1         2.6          5.6         1.4  virginica   27
#> 136          7.7         3.0          6.1         2.3  virginica   28
#> 137          6.3         3.4          5.6         2.4  virginica   28
#> 138          6.4         3.1          5.5         1.8  virginica   28
#> 139          6.0         3.0          4.8         1.8  virginica   28
#> 140          6.9         3.1          5.4         2.1  virginica   28
#> 141          6.7         3.1          5.6         2.4  virginica   29
#> 142          6.9         3.1          5.1         2.3  virginica   29
#> 143          5.8         2.7          5.1         1.9  virginica   29
#> 144          6.8         3.2          5.9         2.3  virginica   29
#> 145          6.7         3.3          5.7         2.5  virginica   29
#> 146          6.7         3.0          5.2         2.3  virginica   30
#> 147          6.3         2.5          5.0         1.9  virginica   30
#> 148          6.5         3.0          5.2         2.0  virginica   30
#> 149          6.2         3.4          5.4         2.3  virginica   30
#> 150          5.9         3.0          5.1         1.8  virginica   30
```
