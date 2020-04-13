context("Tutorials point examples for the XML package")

# Examples taken from https://www.tutorialspoint.com/r/r_xml_files.htm
xmlString <- "<RECORDS>
   <EMPLOYEE>
      <ID>1</ID>
      <NAME>Rick</NAME>
      <SALARY>623.3</SALARY>
      <STARTDATE>1/1/2012</STARTDATE>
      <DEPT>IT</DEPT>
   </EMPLOYEE>

   <EMPLOYEE>
      <ID>2</ID>
      <NAME>Dan</NAME>
      <SALARY>515.2</SALARY>
      <STARTDATE>9/23/2013</STARTDATE>
      <DEPT>Operations</DEPT>
   </EMPLOYEE>

   <EMPLOYEE>
      <ID>3</ID>
      <NAME>Michelle</NAME>
      <SALARY>611</SALARY>
      <STARTDATE>11/15/2014</STARTDATE>
      <DEPT>IT</DEPT>
   </EMPLOYEE>

   <EMPLOYEE>
      <ID>4</ID>
      <NAME>Ryan</NAME>
      <SALARY>729</SALARY>
      <STARTDATE>5/11/2014</STARTDATE>
      <DEPT>HR</DEPT>
   </EMPLOYEE>

   <EMPLOYEE>
      <ID>5</ID>
      <NAME>Gary</NAME>
      <SALARY>843.25</SALARY>
      <STARTDATE>3/27/2015</STARTDATE>
      <DEPT>Finance</DEPT>
   </EMPLOYEE>

   <EMPLOYEE>
      <ID>6</ID>
      <NAME>Nina</NAME>
      <SALARY>578</SALARY>
      <STARTDATE>5/21/2013</STARTDATE>
      <DEPT>IT</DEPT>
   </EMPLOYEE>

   <EMPLOYEE>
      <ID>7</ID>
      <NAME>Simon</NAME>
      <SALARY>632.8</SALARY>
      <STARTDATE>7/30/2013</STARTDATE>
      <DEPT>Operations</DEPT>
   </EMPLOYEE>

   <EMPLOYEE>
      <ID>8</ID>
      <NAME>Guru</NAME>
      <SALARY>722.5</SALARY>
      <STARTDATE>6/17/2014</STARTDATE>
      <DEPT>Finance</DEPT>
   </EMPLOYEE>

</RECORDS>"

test_that("simple examples works", {
  #result <- xmlParse(file = "input.xml")
  doc <- parse.xmlstring(xmlString)

  # Exract the root node form the xml file.
  #rootnode <- xmlRoot(result)
  rootnode <- doc$getRootElement()

  # Find number of nodes in the root.
  #rootsize <- xmlSize(rootnode)
  rootsize <- length(rootnode$getChildren())

  expect_equal(info="Number of children under root", rootsize, 8)

  # Get the first element of the first node.
  #print(rootnode[[1]][[1]])
  expect_equal(info="first element of the first node.", rootnode$getChildren()[[1]]$getChildren()[[1]]$getText(), "1")

  # Get the fifth element of the first node.
  #print(rootnode[[1]][[5]])
  expect_equal(info="fifth element of the first node.", rootnode$getChildren()[[1]]$getChildren()[[5]]$getText(), "IT")

  # Get the second element of the third node.
  #print(rootnode[[3]][[2]])
  expect_equal(info="fifth element of the first node.", rootnode$getChildren()[[3]]$getChildren()[[2]]$getText(), "Michelle")
})

test_that("XML to Data Frame works", {
  # Convert the input xml file to a data frame.
  #xmldataframe <- xmlToDataFrame("input.xml")
  doc <- parse.xmlstring(xmlString)
  rootnode <- doc$getRootElement()
  element <- rootnode
  xmldataframe <- xmlrToDataFrame(rootnode)
  expect_equal(nrow(xmldataframe), 8)
  expect_equal(ncol(xmldataframe), 5)
  expect_equal(xmldataframe[5, 3], "843.25")
  expect_equal(which(xmldataframe$NAME == "Guru"), 8)

  # Not part of the tuorials example but useful as it mixed attributes with elements
  xml <- "
  <groceries>
    <item type='fruit' number='4'>Apples</item>
    <item type='fruit' number='2'>Bananas</item>
    <item type='vegetables' number='6'>Tomatoes</item>
  </groceries>
  "
  doc <- parse.xmlstring(xml)
  xmldataframe <- xmlrToDataFrame(doc$getRootElement())
  expect_equal(xmldataframe[3, 1], "vegetables")
})