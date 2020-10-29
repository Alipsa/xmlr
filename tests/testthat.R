library(testthat)
library(xmlr)

isRenjin <- function() {
  grepl("Renjin", R.Version()$version.string, fixed = TRUE)
}

test_check("xmlr")
