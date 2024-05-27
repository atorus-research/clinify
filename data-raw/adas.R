# note: adlb.Rdata was copied over from vignettes/adsl.Rdata
# this is a copy of the PHUSE Test Data Factory data, trimmed down for size

load("data-raw/adas.Rdata")
test_adas <- adas
usethis::use_data(test_adas, overwrite = TRUE)
