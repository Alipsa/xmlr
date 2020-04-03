context("Simple creation")
library(rcdom)

test_that("Element can be created", {
  e1 <- Element$new(name="Bla")
  printp("e1 is", e1$toString())
  printp("e1 with paste is", e1)
  print(paste("e1, name is", e1$getName()))
  expect_equal(e1$getName(), "Bla")
})
