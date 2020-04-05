context("Document unit tests")

test_that("Documents can be created and printed", {
  xmlString <- paste0(
    "<table xmlns='http://www.w3.org/TR/html4/'>",
      "<tr>",
        "<td>Apples</td>",
        "<td>Bananas</td>",
      "</tr>",
    "</table>"
  )
  doc <- Document$new()
  root <- Element$new("table")
  root$setNamespace(Namespace$new("xmlns", "http://www.w3.org/TR/html4/"))

  root$addContent(
    Element$new("tr")
      $addContent(Element$new("td")$setText("Apples"))
      $addContent(Element$new("td")$setText("Bananas"))
  )
  doc$setRootElement(root)
  #printp("Document is", doc)
  table <- doc$getRootElement()
  expect_equal(table$getName(), "table")
  children <- table$getChild("tr")$getChildren()
  expect_equal(class(children), "list")
  expect_equal(length(children), 2)
  expect_equal(children[[1]]$getText(), "Apples")
  expect_equal(children[[2]]$getText(), "Bananas")
  out <- capture.output({
    print(doc$getRootElement())
  })
  printp("out is", out)
  expect_match(out, xmlString)
})