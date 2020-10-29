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

  e <- Element$new("")
  e$setName("now")
  time <- Sys.time()
  e$setAttribute("time", time)
  cdate <- date()
  e$setText(cdate)
  expect_equal(e$getAttribute("time"), as.character(time))
  expect_equal(e$getText(), cdate)
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

test_that("Elements can be removed", {
  xmlString <- paste0(
  "<foo>",
  "<Bar>Some text</Bar>",
  "<Baz>More stuff</Baz>",
  "</foo>")
  foo <- Element$new("foo")
  foo$addContent(Element$new("Bar")$setText("Some text"))
  foo$addContent(Element$new("Baz")$setText("More stuff"))
  expect_equal(foo$getChild("Bar")$getText(), "Some text")
  expect_equal(paste(foo), xmlString)

  baz <- foo$getChild("Baz")
  foo$removeContent(baz)
  expect_equal(paste(foo), paste0(
    "<foo>",
    "<Bar>Some text</Bar>",
    "</foo>")
  )
  # As we have just removed baz, removing it again should result in an error
  if (isRenjin()) {
    expect_error(foo$removeContent(baz), "Exception calling rlang_eval : null")
  } else {
    expect_error(foo$removeContent(baz), "There is no such content belonging to this Element")
  }
})

test_that("Conversions works and wrong input does not work", {
  e <- Element$new("test")
  attributelist <- c("foo", "bar")
  if (isRenjin()) {
    expect_error(e$setAttributes(attributelist),  "Exception calling rlang_eval : null")
  } else {
    expect_error(e$setAttributes(attributelist), "Argument to setAttributes must be a list")
  }
  attributelist <- as.list(attributelist)
  if (isRenjin()) {
    expect_error(e$setAttributes(attributelist),  "Exception calling rlang_eval : null")
  } else {
    expect_error(e$setAttributes(attributelist), "All attribute values in the list must be named")
  }
  # now we name it an all should work
  names(attributelist) <- c("first", "second")
  e$setAttributes(attributelist)
  expect_equal(e$getAttribute("second"), "bar")
  # add a number to the list
  attributelist$numbers <- 3L
  e$setAttributes(attributelist)
  expect_equal(e$getAttribute("numbers"), "3")
  bla <- c("bla")
  e$setAttribute(bla, c(8))
  expect_equal(e$getAttribute("bla"), "8")
  expect_equal(e$getAttributes()[["bla"]] , "8")
})

test_that("Text nodes can be created and also mixed", {
  e <- Element$new("cars")$setText("Volvo")
  expect_equal(e$getText(), "Volvo")
  xml <- "<cars>Volvo<value sek='200000'></value></cars>"
  e <- Element$new("cars")
  e$addContent(Text$new("Volvo"))
  e$addContent(Element$new("value")$setAttribute("sek", "200000"))
  expect_equal(e$toString(), xml)
  xml <- "<cars>Volvo<value sek='200000'></value>: for sale</cars>"
  e$addContent(Text$new(": for sale"))
  expect_equal(e$toString(), xml)
})