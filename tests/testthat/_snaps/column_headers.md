# Headers apply as expected

    Code
      ct2$header$dataset
    Output
        Sepal.Length Sepal.Width Petal.Length Petal.Width Species
      1      Flowers     Flowers                                 
      2        Sepal       Sepal        Petal       Petal        
      3       Length       Width       Length       Width        

---

    Code
      ct2$header$spans
    Output
      $rows
           [,1] [,2] [,3] [,4] [,5]
      [1,]    2    0    2    0    1
      [2,]    2    0    2    0    1
      [3,]    1    1    1    1    1
      
      $columns
           [,1] [,2] [,3] [,4] [,5]
      [1,]    1    1    1    1    1
      [2,]    1    1    1    1    1
      [3,]    1    1    1    1    1
      

---

    Code
      ct3$header$dataset
    Output
        Sepal.Length Sepal.Width Petal.Length Petal.Width Species
      1       Flower      Flower       Flower      Flower        
      2        Sepal       Sepal        Petal       Petal        
      3       Length       Width       Length       Width        

---

    Code
      ct3$header$spans
    Output
      $rows
           [,1] [,2] [,3] [,4] [,5]
      [1,]    4    0    0    0    1
      [2,]    2    0    2    0    1
      [3,]    1    1    1    1    1
      
      $columns
           [,1] [,2] [,3] [,4] [,5]
      [1,]    1    1    1    1    1
      [2,]    1    1    1    1    1
      [3,]    1    1    1    1    1
      

# Overflowing page headers update appropriately

    Code
      p1$header$spans$rows
    Output
           [,1] [,2] [,3] [,4] [,5] [,6]
      [1,]    4    0    0    0    2    0
      [2,]    1    1    1    1    1    1

---

    Code
      p1$header$dataset
    Output
                      mpg                 cyl               hp                   disp
      1                                                                              
      2 Miles/(US) gallon Number of cylinders Gross horsepower Displacement\n(cu.in.)
                       drat                  wt
      1 Span multiple pages Span multiple pages
      2     Rear axle ratio   Weight (1000 lbs)

---

    Code
      p2$header$spans$rows
    Output
           [,1] [,2] [,3] [,4] [,5] [,6]
      [1,]    3    0    0    3    0    0
      [2,]    1    1    1    1    1    1

---

    Code
      p2$header$dataset
    Output
                      mpg                 cyl               hp                qsec
      1                                                        Span multiple pages
      2 Miles/(US) gallon Number of cylinders Gross horsepower       1/4 mile time
                                          vs
      1                  Span multiple pages
      2 Engine\n(0 = V-shaped, 1 = straight)
                                               am
      1                       Span multiple pages
      2 Transmission\n(0 = automatic, 1 = manual)

---

    Code
      p3$header$spans$rows
    Output
           [,1] [,2] [,3] [,4] [,5]
      [1,]    3    0    0    2    0
      [2,]    1    1    1    1    1

---

    Code
      p3$header$dataset
    Output
                      mpg                 cyl               hp
      1                                                       
      2 Miles/(US) gallon Number of cylinders Gross horsepower
                           gear                  carb
      1            Some Spanner          Some Spanner
      2 Number of forward gears Number of carburetors

