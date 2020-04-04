context("Attribute unit tests")

test_that("Attributes can be created", {
  a1 <- Attribute$new()
  expect_identical(a1$getName(), character(0))
  expect_identical(a1$getValue(), character(0))

  a2 <- Attribute$new("style", "border=1")
  expect_match(a2$getName(), "style")
  expect_match(a2$getValue(), "border=1")

  a3 <- Attribute$new("href", "http://www.alipsa.se", Namespace$new("a", "http://www.github.com"))
  expect_match(a3$getName(), "href")
  expect_match(a3$getValue(), "http://www.alipsa.se")
  # TODO: do some test with Namespace...
})