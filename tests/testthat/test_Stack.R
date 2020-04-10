context("Stack unit tests")

test_that("Stack is working with numbers", {
  st <- Stack$new()
  st$push(1)
  st$push(2)
  st$push(3)
  expect_equal(info="element should be 3: ", st$pop(), 3)
  st$push(5)
  expect_equal(info="element should be 5: ", st$pop(), 5)
  expect_equal(info="element should be 2: ", st$pop(), 2)
  expect_equal(st$peek(), 1)
})

test_that("Stack is working with Objects", {
  st <- Stack$new()
  st$push(Element$new("Foo"))
  expect_equal(st$size(), 1)
  st$push(Element$new("Bar"))
  expect_equal(st$size(), 2)
  st$push(Element$new("Baz"))
  expect_equal(st$size(), 3)
  expect_equal(info="element should be Baz: ", st$pop()$getName(), "Baz")
  expect_equal(info="Top of the stack should be Bar", st$peek()$getName(), "Bar")
  expect_equal(info="Size of the stack", st$size(), 2)

  expect_equal(info="element should be Bar: ", st$pop()$getName(), "Bar")
  expect_equal(info="Size of the stack", st$size(), 1)

  expect_equal(info="element should be Foo: ", st$pop()$getName(), "Foo")
  expect_equal(info="Size of the stack", st$size(), 0)
  top <- st$peek()
  expect_equal(info="Top should be NULL when there are no more elements", is.null(top), TRUE )
})