
test_that("Namespace serialization works", {
  # <parent xmlns="http://some.default"> <child /> </parent>
  parent <- Element$new("parent")
  parent$setAttribute("xmlns", "http://some.default")
  parent$addContent(Element$new("child"))
  expect_equal(parent$toString(), "<parent xmlns='http://some.default'><child></child></parent>")

  # <parent xmlns:env="http://some.default"> <env:child /> </parent>
  parent <- Element$new("parent")$setAttribute("xmlns:env", "http://some.default")
  parent$addContent(Element$new("env:child"))
  expect_equal(parent$toString(), "<parent xmlns:env='http://some.default'><env:child></env:child></parent>")
  # <parent> <child /> </parent>
  # <root xmlns:env="http://some.default"> <env:parent> <env:child /> </parent> </root>

  # <env:parent xmlns:env="http://some.default"> <child xmlns:foo="http://another"/> </parent>

})


nsXml <-
'"<root>

<h:table xmlns:h="http://www.w3.org/TR/html4/">
  <h:tr>
    <h:td>Apples</h:td>
    <h:td>Bananas</h:td>
  </h:tr>
</h:table>

<f:table xmlns:f="https://www.w3schools.com/furniture">
  <f:name>African Coffee Table</f:name>
  <f:width>80</f:width>
  <f:length>120</f:length>
</f:table>

</root>'

rootDeclaredNsXml <-
'<root xmlns:h="http://www.w3.org/TR/html4/"
xmlns:f="https://www.w3schools.com/furniture">

<h:table>
  <h:tr>
    <h:td>Apples</h:td>
    <h:td>Bananas</h:td>
  </h:tr>
</h:table>

<f:table>
  <f:name>African Coffee Table</f:name>
  <f:width>80</f:width>
  <f:length>120</f:length>
</f:table>

</root>'

defaultNsXml <- '<table xmlns="http://www.w3.org/TR/html4/">
  <tr>
    <td>Apples</td>
    <td>Bananas</td>
  </tr>
</table>'

