context("Simple creation")

test_that("Element can be created", {
  e1 <- Element$new(name="Bla")
  expect_equal(e1$toString(), "<Bla></Bla>")
  expect_equal(paste("e1 with paste is", e1), "e1 with paste is <Bla></Bla>")
  expect_equal(e1$getName(), "Bla")
})

test_that("Document can be created", {
  d1 <- Document$new()
  root <- d1$getRootElement()
  expect_equal(info="Document with no root should be NULL", is.null(root), TRUE)
  d2 <- Document$new(Element$new("root"))
  expect_equal(info="getting root element from document", d2$getRootElement()$getName(), "root")
})

test_that("Element can have Namespace and attribute", {
  e2 <- Element$new(name="env:Foo")
  e2$setAttribute("xmlns:env", "http://alipsa.se/xmlr")
  expect_equal(info="Element name", e2$getName(), "env:Foo" )
  expect_equal(info="namespace prefix", e2$getAttribute("xmlns:env"), "http://alipsa.se/xmlr")
  #expect_equal(info="NamespacePrefix directly from element", e2$getNamespacePrefix(), "env")
  e2$setAttribute(name="style", value="color:white")
  expect_equal(info="get attribute value", e2$getAttribute("style"), "color:white")
  e2$setAttribute(name="href", value="http://www.nu.se")
  output <- capture.output(print(e2))
  expect_equal(output, "<env:Foo xmlns:env='http://alipsa.se/xmlr' style='color:white' href='http://www.nu.se'></env:Foo>")
})

test_that("Abstract classes cannot be instantiated", {
  expect_error(AbstractClass$new(), "AbstractClass is an abstract class that can't be initialized.")
})