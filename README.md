# xmlr
XML dom package for R implemented using Reference Classes

The jdom project (www.jdom.org) provided a lot of initial inspiration for the api but there are several differences, mainly 
- Attributes: Attribute is not a specific class but just a name value element of the named list property of an Element
- Namespaces:
    Whereas in jdom namespaces is a special class that exists as a specific object attribute, xmlr takes a "simpler"
    approach in the sense that namespace declarations are just another element attribute and name space prefixes are part of the 
    element name. This might change in the future but for now this is how it is done.

You can create and xmlr Document programmatically or by parsing text or a file. 

# Creating the DOM programmatically
To create the following xml
```
<table xmlns='http://www.w3.org/TR/html4/'>
    <tr>
        <td>Apples</td>
        <td>Bananas</td>
    </tr>
</table>

```
You could do something like this:
```
 doc <- Document$new()
  root <- Element$new("table")
  root$setAttribute("xmlns", "http://www.w3.org/TR/html4/")

  root$addContent(
    Element$new("tr")
      $addContent(Element$new("td")$setText("Apples"))
      $addContent(Element$new("td")$setText("Bananas"))
  )
  doc$setRootElement(root)
```
Or you could do like this:
```
doc2 <- parse.xmlstring("
<table xmlns='http://www.w3.org/TR/html4/'>
    <tr>
        <td>Apples</td>
        <td>Bananas</td>
    </tr>
</table>")
```
Note that there is no pretty print available (yet) so if you print it `print(doc2)`
it will look like this:
```
> print(doc2)
<table xmlns='http://www.w3.org/TR/html4/'><tr><td>Apples</td><td>Bananas</td></tr></table>
> 
```

# Limitations
Processing instructions, custom entity references, and comments are not yet supported.

Any proper xml including CDATA, comments, processing instructions etc. can be parsed though, it is just that
only elements, attributes and text will be retained.

CDATA can be read from string or file but handled as ordinary text after that. I.e. the output might not be valid XML.

There are probably issues (memory, performance) with very large XML trees.

# Why xmlr?
I had problems on one of my machines to install the XML package (some gcc issue) so needed an alternative.
As I have been thinking about doing something more comprehensive with Reference Classes I decided to create
a pure base-R DOM model with some simple ways to do input and output to strings and files. As it turned out to
be useful to me, I thought it might be for others as well, so I decided to open source and publish it.




