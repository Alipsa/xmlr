# rcdom
XML dom package for R similar to jdom (www.jdom.org) implemented using Reference Classes

This is an experimental project to see if it is feasible to implement a typical OO domain using Reference Classes

Although a lo of the api is similar to that of jdom in one regard it is very different: the handling of namespaces.
Where as in jdom namespaces are a special class that exists as a specific object attribute, rcdom takes a "simpler"
approach in the sense that namespace declarations are just another element attribute and name space prefixes are part of the 
element name. This might change in the future but for now this is how it is done.
