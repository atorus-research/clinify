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
                         X1                              X2   X3    X4   X5    X6
      1                Left                           Right <NA>  <NA> <NA>  <NA>
      2     Just the middle                 Just the middle <NA>  <NA> <NA>  <NA>
      3                  a1                              a1   a1    a1   a1    a1
      4                 mpg                             cyl   hp  disp drat    wt
      5                21.0                               6  110 160.0 3.90 2.620
      6                21.0                               6  110 160.0 3.90 2.875
      7                22.8                               4   93 108.0 3.85 2.320
      8                21.4                               6  110 258.0 3.08 3.215
      9                18.7                               8  175 360.0 3.15 3.440
      10               18.1                               6  105 225.0 2.76 3.460
      11               14.3                               8  245 360.0 3.21 3.570
      12               24.4                               4   62 146.7 3.69 3.190
      13               22.8                               4   95 140.8 3.92 3.150
      14               19.2                               6  123 167.6 3.92 3.440
      15 Here's a footnote. 11:58 Wednesday, March 05, 2025 <NA>  <NA> <NA>  <NA>

---

    Code
      as.data.frame(rvest::html_table(tab_html[[2]]))
    Output
                         X1                              X2   X3    X4   X5   X6
      1                Left                           Right <NA>  <NA> <NA> <NA>
      2     Just the middle                 Just the middle <NA>  <NA> <NA> <NA>
      3                  a1                              a1   a1    a1   a1   a1
      4                 mpg                             cyl   hp  qsec   vs   am
      5                21.0                               6  110 16.46    0    1
      6                21.0                               6  110 17.02    0    1
      7                22.8                               4   93 18.61    1    1
      8                21.4                               6  110 19.44    1    0
      9                18.7                               8  175 17.02    0    0
      10               18.1                               6  105 20.22    1    0
      11               14.3                               8  245 15.84    0    0
      12               24.4                               4   62 20.00    1    0
      13               22.8                               4   95 22.90    1    0
      14               19.2                               6  123 18.30    1    0
      15 Here's a footnote. 11:58 Wednesday, March 05, 2025 <NA>  <NA> <NA> <NA>

---

    Code
      as.data.frame(rvest::html_table(tab_html[[3]]))
    Output
                         X1                              X2   X3   X4   X5
      1                Left                           Right <NA> <NA> <NA>
      2     Just the middle                 Just the middle <NA> <NA> <NA>
      3                  a1                              a1   a1   a1   a1
      4                 mpg                             cyl   hp gear carb
      5                21.0                               6  110    4    4
      6                21.0                               6  110    4    4
      7                22.8                               4   93    4    1
      8                21.4                               6  110    3    1
      9                18.7                               8  175    3    2
      10               18.1                               6  105    3    1
      11               14.3                               8  245    3    4
      12               24.4                               4   62    4    2
      13               22.8                               4   95    4    2
      14               19.2                               6  123    4    4
      15 Here's a footnote. 11:58 Wednesday, March 05, 2025 <NA> <NA> <NA>

# Alternating pages print 3 pages with footnote page

    Code
      as.data.frame(rvest::html_table(tab_html[[1]]))
    Output
                                           X1                                    X2
      1                                  Left                                 Right
      2                       Just the middle                       Just the middle
      3   One very long footnote full of text   One very long footnote full of text
      4   Two very long footnote full of text   Two very long footnote full of text
      5 Three very long footnote full of text Three very long footnote full of text
      6  Four very long footnote full of text  Four very long footnote full of text
      7  Five very long footnote full of text  Five very long footnote full of text
      8                    Here's a footnote.       10:55 Wednesday, March 26, 2025

---

    Code
      as.data.frame(rvest::html_table(tab_html[[2]]))
    Output
                         X1                              X2               X3
      1                Left                           Right             <NA>
      2     Just the middle                 Just the middle             <NA>
      3                  a1                              a1               a1
      4                                                                     
      5   Miles/(US) gallon             Number of cylinders Gross horsepower
      6                21.0                               6              110
      7                21.0                               6              110
      8                22.8                               4               93
      9                21.4                               6              110
      10               18.7                               8              175
      11               18.1                               6              105
      12               14.3                               8              245
      13               24.4                               4               62
      14               22.8                               4               95
      15               19.2                               6              123
      16          Caption 1                       Caption 1        Caption 1
      17 Here's a footnote. 10:55 Wednesday, March 26, 2025             <NA>
                           X4                  X5                  X6
      1                  <NA>                <NA>                <NA>
      2                  <NA>                <NA>                <NA>
      3                    a1                  a1                  a1
      4                       Span multiple pages Span multiple pages
      5  Displacement(cu.in.)     Rear axle ratio   Weight (1000 lbs)
      6                 160.0                3.90               2.620
      7                 160.0                3.90               2.875
      8                 108.0                3.85               2.320
      9                 258.0                3.08               3.215
      10                360.0                3.15               3.440
      11                225.0                2.76               3.460
      12                360.0                3.21               3.570
      13                146.7                3.69               3.190
      14                140.8                3.92               3.150
      15                167.6                3.92               3.440
      16            Caption 1           Caption 1           Caption 1
      17                 <NA>                <NA>                <NA>

---

    Code
      as.data.frame(rvest::html_table(tab_html[[3]]))
    Output
                         X1                              X2               X3
      1                Left                           Right             <NA>
      2     Just the middle                 Just the middle             <NA>
      3                  a1                              a1               a1
      4                                                                     
      5   Miles/(US) gallon             Number of cylinders Gross horsepower
      6                21.0                               6              110
      7                21.0                               6              110
      8                22.8                               4               93
      9                21.4                               6              110
      10               18.7                               8              175
      11               18.1                               6              105
      12               14.3                               8              245
      13               24.4                               4               62
      14               22.8                               4               95
      15               19.2                               6              123
      16          Caption 1                       Caption 1        Caption 1
      17 Here's a footnote. 10:55 Wednesday, March 26, 2025             <NA>
                          X4                                 X5
      1                 <NA>                               <NA>
      2                 <NA>                               <NA>
      3                   a1                                 a1
      4  Span multiple pages                Span multiple pages
      5        1/4 mile time Engine(0 = V-shaped, 1 = straight)
      6                16.46                                  0
      7                17.02                                  0
      8                18.61                                  1
      9                19.44                                  1
      10               17.02                                  0
      11               20.22                                  1
      12               15.84                                  0
      13               20.00                                  1
      14               22.90                                  1
      15               18.30                                  1
      16           Caption 1                          Caption 1
      17                <NA>                               <NA>
                                              X6
      1                                     <NA>
      2                                     <NA>
      3                                       a1
      4                      Span multiple pages
      5  Transmission(0 = automatic, 1 = manual)
      6                                        1
      7                                        1
      8                                        1
      9                                        0
      10                                       0
      11                                       0
      12                                       0
      13                                       0
      14                                       0
      15                                       0
      16                               Caption 1
      17                                    <NA>

