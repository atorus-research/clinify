# clin_replace_pagenums processes fields properly

    Code
      title$body$content$data[1, "Right"]
    Output
      $Right
          txt font.size italic bold underlined color shading.color font.family
      1 Page         NA     NA   NA         NA  <NA>          <NA>        <NA>
      2  <NA>        NA     NA   NA         NA  <NA>          <NA>        <NA>
      3   of         NA     NA   NA         NA  <NA>          <NA>        <NA>
      4  <NA>        NA     NA   NA         NA  <NA>          <NA>        <NA>
        hansi.family eastasia.family cs.family vertical.align width height  url
      1         <NA>            <NA>      <NA>           <NA>    NA     NA <NA>
      2         <NA>            <NA>      <NA>           <NA>   0.1   0.15 <NA>
      3         <NA>            <NA>      <NA>           <NA>    NA     NA <NA>
      4         <NA>            <NA>      <NA>           <NA>   0.1   0.15 <NA>
        eq_data word_field_data img_data .chunk_index
      1    <NA>            <NA>     NULL            1
      2    <NA>            PAGE     NULL            2
      3    <NA>            <NA>     NULL            3
      4    <NA>        NUMPAGES     NULL            4
      

---

    Code
      footnote$body$content$data[1, "Left"]
    Output
      $Left
          txt font.size italic bold underlined color shading.color font.family
      1 Page         NA     NA   NA         NA  <NA>          <NA>        <NA>
      2  <NA>        NA     NA   NA         NA  <NA>          <NA>        <NA>
        hansi.family eastasia.family cs.family vertical.align width height  url
      1         <NA>            <NA>      <NA>           <NA>    NA     NA <NA>
      2         <NA>            <NA>      <NA>           <NA>   0.1   0.15 <NA>
        eq_data word_field_data img_data .chunk_index
      1    <NA>            <NA>     NULL            1
      2    <NA>            PAGE     NULL            2
      

---

    Code
      footnote$body$content$data[1, "Right"]
    Output
      $Right
                  txt font.size italic bold underlined color shading.color
      1 Total Pages:         NA     NA   NA         NA  <NA>          <NA>
      2          <NA>        NA     NA   NA         NA  <NA>          <NA>
        font.family hansi.family eastasia.family cs.family vertical.align width
      1        <NA>         <NA>            <NA>      <NA>           <NA>    NA
      2        <NA>         <NA>            <NA>      <NA>           <NA>   0.1
        height  url eq_data word_field_data img_data .chunk_index
      1     NA <NA>    <NA>            <NA>     NULL            1
      2   0.15 <NA>    <NA>        NUMPAGES     NULL            2
      

