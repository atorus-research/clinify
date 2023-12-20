# note: adsl.Rdata was copied over from vignettes/adsl.Rdata
# this is a copy of the PHUSE Test Data Factory data, trimmed down for size

load("data-raw/adsl.Rdata")
test_adsl <- adsl
usethis::use_data(test_adsl, overwrite = TRUE)
