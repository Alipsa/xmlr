context("utils unit tests")

test_that("hasWhiteSpace works", {
  expect_equal(hasWhiteSpace("Ett två"), TRUE)
  expect_equal(hasWhiteSpace("Etttvå"), FALSE)
  expect_equal(hasWhiteSpace(" "), TRUE)
  expect_equal(hasWhiteSpace("\t"), TRUE)
  expect_equal(hasWhiteSpace(""), FALSE)
  expect_equal(hasWhiteSpace("p"), FALSE)
  expect_equal(hasWhiteSpace(NULL), FALSE)
  expect_equal(hasWhiteSpace("bla\r\n"), TRUE)
})

test_that("isWhiteSpaceChar works", {
  expect_equal(isWhiteSpaceChar(" "), TRUE)
  expect_equal(isWhiteSpaceChar(""), FALSE)
  expect_equal(isWhiteSpaceChar("\t"), TRUE)
  expect_equal(isWhiteSpaceChar("\r"), TRUE)
  expect_equal(isWhiteSpaceChar("\n"), TRUE)
  expect_equal(isWhiteSpaceChar(""), FALSE)
  expect_equal(isWhiteSpaceChar("p"), FALSE)
  expect_equal(isWhiteSpaceChar(NULL), FALSE)
  expect_equal(isWhiteSpaceChar("bla\r\n"), TRUE)
})