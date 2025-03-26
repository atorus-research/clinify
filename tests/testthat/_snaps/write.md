# Single table write

    Code
      x1
    Output
      rdocx document with 3 element(s)
      
      * styles:
                      Normal              heading 1              heading 2 
                 "paragraph"            "paragraph"            "paragraph" 
                   heading 3 Default Paragraph Font           Normal Table 
                 "paragraph"            "character"                "table" 
                     No List                 strong               centered 
                 "numbering"            "character"            "paragraph" 
              table_template    Light List Accent 2            Titre 1 Car 
                     "table"                "table"            "character" 
                 Titre 2 Car            Titre 3 Car          Image Caption 
                 "character"            "character"            "paragraph" 
               Table Caption     Table Professional                  toc 1 
                 "paragraph"                "table"            "paragraph" 
                       toc 2           Balloon Text    Texte de bulles Car 
                 "paragraph"            "paragraph"            "character" 
                reference_id          graphic title            table title 
                 "character"            "paragraph"            "paragraph" 
      
      * Content at cursor location:
        level num_id text style_name content_type
      1    NA     NA              NA    paragraph

---

    Code
      x2
    Output
      rdocx document with 3 element(s)
      
      * styles:
                      Normal              heading 1              heading 2 
                 "paragraph"            "paragraph"            "paragraph" 
                   heading 3 Default Paragraph Font           Normal Table 
                 "paragraph"            "character"                "table" 
                     No List                 strong               centered 
                 "numbering"            "character"            "paragraph" 
              table_template    Light List Accent 2            Titre 1 Car 
                     "table"                "table"            "character" 
                 Titre 2 Car            Titre 3 Car          Image Caption 
                 "character"            "character"            "paragraph" 
               Table Caption     Table Professional                  toc 1 
                 "paragraph"                "table"            "paragraph" 
                       toc 2           Balloon Text    Texte de bulles Car 
                 "paragraph"            "paragraph"            "character" 
                reference_id          graphic title            table title 
                 "character"            "paragraph"            "paragraph" 
      
      * Content at cursor location:
        level num_id text style_name content_type
      1    NA     NA              NA    paragraph

# Alt pages write

    Code
      x
    Output
      rdocx document with 62 element(s)
      
      * styles:
                      Normal              heading 1              heading 2 
                 "paragraph"            "paragraph"            "paragraph" 
                   heading 3 Default Paragraph Font           Normal Table 
                 "paragraph"            "character"                "table" 
                     No List                 strong               centered 
                 "numbering"            "character"            "paragraph" 
              table_template    Light List Accent 2            Titre 1 Car 
                     "table"                "table"            "character" 
                 Titre 2 Car            Titre 3 Car          Image Caption 
                 "character"            "character"            "paragraph" 
               Table Caption     Table Professional                  toc 1 
                 "paragraph"                "table"            "paragraph" 
                       toc 2           Balloon Text    Texte de bulles Car 
                 "paragraph"            "paragraph"            "character" 
                reference_id          graphic title            table title 
                 "character"            "paragraph"            "paragraph" 
      
      * Content at cursor location:
           row_id is_header cell_id                    text col_span row_span
      1.1       1      TRUE       1                      b2        5        1
      1.11      2      TRUE       1                                3        1
      1.12      3      TRUE       1       Miles/(US) gallon        1        1
      1.13      4     FALSE       1                    15.0        1        1
      1.14      5     FALSE       1                    21.4        1        1
      1.15      6     FALSE       1               Caption 4        5        1
      2.2       1      TRUE       2                    <NA>        0        1
      2.31      2      TRUE       2                    <NA>        0        1
      2.22      3      TRUE       2     Number of cylinders        1        1
      2.23      4     FALSE       2                       8        1        1
      2.24      5     FALSE       2                       4        1        1
      2.25      6     FALSE       2                    <NA>        0        1
      3.3       1      TRUE       3                    <NA>        0        1
      3.41      2      TRUE       3                    <NA>        0        1
      3.32      3      TRUE       3        Gross horsepower        1        1
      3.33      4     FALSE       3                     335        1        1
      3.34      5     FALSE       3                     109        1        1
      3.35      6     FALSE       3                    <NA>        0        1
      4.4       1      TRUE       4                    <NA>        0        1
      4.21      2      TRUE       4            Some Spanner        2        1
      4.42      3      TRUE       4 Number of forward gears        1        1
      4.43      4     FALSE       4                       5        1        1
      4.44      5     FALSE       4                       4        1        1
      4.45      6     FALSE       4                    <NA>        0        1
      5.5       1      TRUE       5                    <NA>        0        1
      5.51      2      TRUE       5                    <NA>        0        1
      5.52      3      TRUE       5   Number of carburetors        1        1
      5.53      4     FALSE       5                       8        1        1
      5.54      5     FALSE       5                       2        1        1
      5.55      6     FALSE       5                    <NA>        0        1
           content_type
      1.1    table cell
      1.11   table cell
      1.12   table cell
      1.13   table cell
      1.14   table cell
      1.15   table cell
      2.2    table cell
      2.31   table cell
      2.22   table cell
      2.23   table cell
      2.24   table cell
      2.25   table cell
      3.3    table cell
      3.41   table cell
      3.32   table cell
      3.33   table cell
      3.34   table cell
      3.35   table cell
      4.4    table cell
      4.21   table cell
      4.42   table cell
      4.43   table cell
      4.44   table cell
      4.45   table cell
      5.5    table cell
      5.51   table cell
      5.52   table cell
      5.53   table cell
      5.54   table cell
      5.55   table cell

