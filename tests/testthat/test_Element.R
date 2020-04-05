context("Element unit tests")

test_that("Elements can be created and printed", {

  xmlString <- paste0(
    "<foo>",
      "<Bar>Some text</Bar>",
      "<Baz>More stuff</Baz>",
    "</foo>")
  foo <- Element$new("foo")
  foo$addContent(Element$new("Bar")$setText("Some text"))
  foo$addContent(Element$new("Baz")$setText("More stuff"))
  expect_equal(foo$getChild("Bar")$getText(), "Some text")

  expect_equal(capture.output(print(foo)), xmlString)
  expect_equal(paste(foo), xmlString)
})

test_that("Deep trees works", {
  xmlString <- paste0(
    '<purchase>',
      '<fresh>',
        '<fruit>',
          "<item quantity='3'>Apples</item>",
          "<item quantity='7'>Bananas</item>",
        '</fruit>',
      '</fresh>',
      '<frozen>',
        '<sweets>',
          '<icecream>',
            "<item quantity='5'>Vanilla stick</item>",
          '</icecream>',
        '</sweets>',
      '</frozen>',
    '</purchase>'
  )
  doc <- Document$new()
  root <- Element$new("purchase")
  doc$setRootElement(root)

  fresh <- Element$new("fresh")
  root$addContent(fresh)

  fresh$addContent(Element$new("fruit"))
  fruit <- fresh$getChild("fruit")
  fruit$addContent(Element$new("item")$setAttribute("quantity", "3")$setText("Apples"))
  firstItem <- fruit$getChild("item")
  firstItem$getParent()$addContent(Element$new("item")$setAttribute("quantity", "7")$setText("Bananas"))

  root$addContent(Element$new("frozen")
    $addContent(Element$new("sweets")
      $addContent(Element$new("icecream")
        $addContent(Element$new("item")$setAttribute("quantity", "5")$setText("Vanilla stick"))
      )
    )
  )
  #print(root)
  expect_equal(paste0(root), xmlString)

})