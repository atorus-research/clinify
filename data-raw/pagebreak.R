## code to prepare `pagebreak` dataset goes here
pagebreak <- officer:::runs_to_p_wml(officer:::run_pagebreak(), add_ns = TRUE)
usethis::use_data(pagebreak, overwrite = TRUE)
