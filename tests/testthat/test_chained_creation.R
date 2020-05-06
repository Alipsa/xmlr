context("Chained creation and traversion")

createDom <- function() {
  doc <- Document$new()
  doc$setRootElement(Element$new(name="class"))
  root <- doc$getRootElement()
  e1 <- Element$new(name="student")
  e1$setAttribute(name="rollno", value="393")
  #print(e1$getAttribute("rollno"))
  e1c1 <- Element$new(name="firstname")
  e1c1$setText("Dinkar")
  e1$addContent(e1c1)
  e1c2 <- Element$new(name="lastname")$setText("Kad")
  e1$addContent(e1c2)
  e1$addContent(Element$new(name="nickname")$setText("dinkar"))
  e1$addContent(Element$new(name="marks")$setText("85"))
  root$addContent(e1)

  root$addContent(Element$new(name="student")$setAttribute(name="rollno",value="493")
    $addContent(Element$new(name="firstname")$setText("Vaneet"))
    $addContent(Element$new(name="lastname")$setText("Gupta"))
    $addContent(Element$new(name="nickname")$setText("vinni"))
    $addContent(Element$new(name="marks")$setText("95"))
  )
  return(doc)
}

test_that("Chained creation and traversion works", {
  document <- createDom()
  rootName <- document$getRootElement()$getName()
  expect_equal(info="Root element from document", rootName, "class")
  expect_equal(capture_output(print(paste("Root element :", rootName))), "[1] \"Root element : class\"");
  classElement <- document$getRootElement();
  studentList <- classElement$getChildren();
  #str(studentList)
  expect_equal(info="Number of elements", length(classElement$getContent()), 2)
  expect_equal(info="Number of children are", length(studentList), 2)

  printp <- function(...) {
    print(paste(...))
  }

  output <- capture.output({
    for (student in studentList) {
      print("-------------------------")
      printp("Current Element :", student$getName())
      printp("Student roll no : ", student$getAttribute("rollno") )
      printp("First Name : ", student$getChild("firstname")$getText())
      printp("Last Name : ", student$getChild("lastname")$getText())
      printp("Nick Name : ", student$getChild("nickname")$getText())
      printp("Marks : ", student$getChild("marks")$getText())
    }
  })

  expect <- c(
    '[1] "-------------------------"',
    '[1] "Current Element : student"',
    '[1] "Student roll no :  393"',
    '[1] "First Name :  Dinkar"',
    '[1] "Last Name :  Kad"',
    '[1] "Nick Name :  dinkar"',
    '[1] "Marks :  85"',
    '[1] "-------------------------"',
    '[1] "Current Element : student"',
    '[1] "Student roll no :  493"',
    '[1] "First Name :  Vaneet"',
    '[1] "Last Name :  Gupta"',
    '[1] "Nick Name :  vinni"',
    '[1] "Marks :  95"'
  )
  expect_equal(output, expect)
})