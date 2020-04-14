context("xmlConverter unit tests")

test_that("XML Strings can be imported", {
  doc <- parse.xmlstring("<foo><bar><baz val='the baz attribute'/></bar></foo>")
  expect_equal(doc$toString(), "<foo><bar><baz val='the baz attribute'></baz></bar></foo>")
})

test_that("XML files can be imported", {
  doc <- parse.xmlfile("pom.xml")
  root <- doc$getRootElement()
  expect_equal(root$getName(), "project")

  findByArtifactId <- function(dependencies, artifactId) {
    for (dependency in dependencies$getChildren()) {
      if (dependency$getChild("artifactId")$getText() == artifactId) {
        return (dependency)
      }
    }
  }

  dep <- findByArtifactId(root$getChild("dependencies"), "testthat")
  groupId <- dep$getChild("groupId")$getText()
  expect_equal("org.renjin.cran", groupId)
})