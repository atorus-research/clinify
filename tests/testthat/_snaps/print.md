# Footnote page for no alternating pages renders properly

    Code
      as.data.frame(rvest::html_table(tab_html[[1]]))
    Output
                                           X1                                    X2
      1   One very long footnote full of text   One very long footnote full of text
      2   Two very long footnote full of text   Two very long footnote full of text
      3 Three very long footnote full of text Three very long footnote full of text
      4  Four very long footnote full of text  Four very long footnote full of text
      5  Five very long footnote full of text  Five very long footnote full of text
                                           X3
      1   One very long footnote full of text
      2   Two very long footnote full of text
      3 Three very long footnote full of text
      4  Four very long footnote full of text
      5  Five very long footnote full of text

---

    Code
      as.data.frame(rvest::html_table(tab_html[[2]]))
    Output
          mpg cyl  disp  hp drat    wt  qsec vs am gear carb
      1  21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
      2  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
      3  22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
      4  21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
      5  18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
      6  18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
      7  14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
      8  24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
      9  22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
      10 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
      11 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
      12 16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
      13 17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
      14 15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
      15 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4

# Alternating pages print 3 pages

    Code
      as.data.frame(rvest::html_table(tab_html[[1]]))
    Output
                         X1                              X2
      1                Left                          Center
      2     Just the middle                 Just the middle
      3                  a1                              a1
      4                 mpg                             cyl
      5                21.0                               6
      6                21.0                               6
      7                22.8                               4
      8                21.4                               6
      9                18.7                               8
      10               18.1                               6
      11               14.3                               8
      12               24.4                               4
      13               22.8                               4
      14               19.2                               6
      15 Here's a footnote. 11:58 Wednesday, March 05, 2025
                                      X3    X4   X5    X6
      1                            Right  <NA> <NA>  <NA>
      2                  Just the middle  <NA> <NA>  <NA>
      3                               a1    a1   a1    a1
      4                               hp  disp drat    wt
      5                              110 160.0 3.90 2.620
      6                              110 160.0 3.90 2.875
      7                               93 108.0 3.85 2.320
      8                              110 258.0 3.08 3.215
      9                              175 360.0 3.15 3.440
      10                             105 225.0 2.76 3.460
      11                             245 360.0 3.21 3.570
      12                              62 146.7 3.69 3.190
      13                              95 140.8 3.92 3.150
      14                             123 167.6 3.92 3.440
      15 11:58 Wednesday, March 05, 2025  <NA> <NA>  <NA>

---

    Code
      as.data.frame(rvest::html_table(tab_html[[2]]))
    Output
                         X1                              X2
      1                Left                          Center
      2     Just the middle                 Just the middle
      3                  a1                              a1
      4                 mpg                             cyl
      5                21.0                               6
      6                21.0                               6
      7                22.8                               4
      8                21.4                               6
      9                18.7                               8
      10               18.1                               6
      11               14.3                               8
      12               24.4                               4
      13               22.8                               4
      14               19.2                               6
      15 Here's a footnote. 11:58 Wednesday, March 05, 2025
                                      X3    X4   X5   X6
      1                            Right  <NA> <NA> <NA>
      2                  Just the middle  <NA> <NA> <NA>
      3                               a1    a1   a1   a1
      4                               hp  qsec   vs   am
      5                              110 16.46    0    1
      6                              110 17.02    0    1
      7                               93 18.61    1    1
      8                              110 19.44    1    0
      9                              175 17.02    0    0
      10                             105 20.22    1    0
      11                             245 15.84    0    0
      12                              62 20.00    1    0
      13                              95 22.90    1    0
      14                             123 18.30    1    0
      15 11:58 Wednesday, March 05, 2025  <NA> <NA> <NA>

---

    Code
      as.data.frame(rvest::html_table(tab_html[[3]]))
    Output
                         X1                              X2
      1                Left                          Center
      2     Just the middle                 Just the middle
      3                  a1                              a1
      4                 mpg                             cyl
      5                21.0                               6
      6                21.0                               6
      7                22.8                               4
      8                21.4                               6
      9                18.7                               8
      10               18.1                               6
      11               14.3                               8
      12               24.4                               4
      13               22.8                               4
      14               19.2                               6
      15 Here's a footnote. 11:58 Wednesday, March 05, 2025
                                      X3   X4   X5
      1                            Right <NA> <NA>
      2                  Just the middle <NA> <NA>
      3                               a1   a1   a1
      4                               hp gear carb
      5                              110    4    4
      6                              110    4    4
      7                               93    4    1
      8                              110    3    1
      9                              175    3    2
      10                             105    3    1
      11                             245    3    4
      12                              62    4    2
      13                              95    4    2
      14                             123    4    4
      15 11:58 Wednesday, March 05, 2025 <NA> <NA>

# Alternating pages print 3 pages with footnote page

    Code
      as.data.frame(rvest::html_table(tab_html[[1]]))
    Output
                                           X1                                    X2
      1                                  Left                                Center
      2                       Just the middle                       Just the middle
      3   One very long footnote full of text   One very long footnote full of text
      4   Two very long footnote full of text   Two very long footnote full of text
      5 Three very long footnote full of text Three very long footnote full of text
      6  Four very long footnote full of text  Four very long footnote full of text
      7  Five very long footnote full of text  Five very long footnote full of text
      8                    Here's a footnote.       10:55 Wednesday, March 26, 2025
                                           X3
      1                                 Right
      2                       Just the middle
      3   One very long footnote full of text
      4   Two very long footnote full of text
      5 Three very long footnote full of text
      6  Four very long footnote full of text
      7  Five very long footnote full of text
      8       10:55 Wednesday, March 26, 2025

---

    Code
      as.data.frame(rvest::html_table(tab_html[[2]]))
    Output
                         X1                              X2
      1                Left                          Center
      2     Just the middle                 Just the middle
      3                  a1                              a1
      4                                                    
      5   Miles/(US) gallon             Number of cylinders
      6                21.0                               6
      7                21.0                               6
      8                22.8                               4
      9                21.4                               6
      10               18.7                               8
      11               18.1                               6
      12               14.3                               8
      13               24.4                               4
      14               22.8                               4
      15               19.2                               6
      16          Caption 1                       Caption 1
      17 Here's a footnote. 10:55 Wednesday, March 26, 2025
                                      X3                   X4                  X5
      1                            Right                 <NA>                <NA>
      2                  Just the middle                 <NA>                <NA>
      3                               a1                   a1                  a1
      4                                                       Span multiple pages
      5                 Gross horsepower Displacement(cu.in.)     Rear axle ratio
      6                              110                160.0                3.90
      7                              110                160.0                3.90
      8                               93                108.0                3.85
      9                              110                258.0                3.08
      10                             175                360.0                3.15
      11                             105                225.0                2.76
      12                             245                360.0                3.21
      13                              62                146.7                3.69
      14                              95                140.8                3.92
      15                             123                167.6                3.92
      16                       Caption 1            Caption 1           Caption 1
      17 10:55 Wednesday, March 26, 2025                 <NA>                <NA>
                          X6
      1                 <NA>
      2                 <NA>
      3                   a1
      4  Span multiple pages
      5    Weight (1000 lbs)
      6                2.620
      7                2.875
      8                2.320
      9                3.215
      10               3.440
      11               3.460
      12               3.570
      13               3.190
      14               3.150
      15               3.440
      16           Caption 1
      17                <NA>

---

    Code
      as.data.frame(rvest::html_table(tab_html[[3]]))
    Output
                         X1                              X2
      1                Left                          Center
      2     Just the middle                 Just the middle
      3                  a1                              a1
      4                                                    
      5   Miles/(US) gallon             Number of cylinders
      6                21.0                               6
      7                21.0                               6
      8                22.8                               4
      9                21.4                               6
      10               18.7                               8
      11               18.1                               6
      12               14.3                               8
      13               24.4                               4
      14               22.8                               4
      15               19.2                               6
      16          Caption 1                       Caption 1
      17 Here's a footnote. 10:55 Wednesday, March 26, 2025
                                      X3                  X4
      1                            Right                <NA>
      2                  Just the middle                <NA>
      3                               a1                  a1
      4                                  Span multiple pages
      5                 Gross horsepower       1/4 mile time
      6                              110               16.46
      7                              110               17.02
      8                               93               18.61
      9                              110               19.44
      10                             175               17.02
      11                             105               20.22
      12                             245               15.84
      13                              62               20.00
      14                              95               22.90
      15                             123               18.30
      16                       Caption 1           Caption 1
      17 10:55 Wednesday, March 26, 2025                <NA>
                                         X5                                      X6
      1                                <NA>                                    <NA>
      2                                <NA>                                    <NA>
      3                                  a1                                      a1
      4                 Span multiple pages                     Span multiple pages
      5  Engine(0 = V-shaped, 1 = straight) Transmission(0 = automatic, 1 = manual)
      6                                   0                                       1
      7                                   0                                       1
      8                                   1                                       1
      9                                   1                                       0
      10                                  0                                       0
      11                                  1                                       0
      12                                  0                                       0
      13                                  1                                       0
      14                                  1                                       0
      15                                  1                                       0
      16                          Caption 1                               Caption 1
      17                               <NA>                                    <NA>

