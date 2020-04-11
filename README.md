# xmlr
XML dom package for R similar to jdom (www.jdom.org) implemented using Reference Classes

Although a lot of the api is similar to that of jdom in one important regard it is very different: the handling of namespaces.
Whereas in jdom namespaces are a special class that exists as a specific object attribute, xmlr takes a "simpler"
approach in the sense that namespace declarations are just another element attribute and name space prefixes are part of the 
element name. This might change in the future but for now this is how it is done.

You can create and xmlr Document programatically or by parsing text. 

# Creating the DOM programatically
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
Processing instructions, custom entity references, comments are not supported yet.
CDATA can be read from string or file but handled as ordinary text after that.

There are probably issue with very large XML trees.




