# note: adlb.Rdata was copied over from vignettes/adsl.Rdata
# this is a copy of the PHUSE Test Data Factory data, trimmed down for size

load("data-raw/adlb.Rdata")
test_adlb <- adlb
usethis::use_data(test_adlb, overwrite = TRUE)
