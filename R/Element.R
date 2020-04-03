#' Reference class representing an XML tag
#' @details
#' An XML element. Methods allow the user to get and manipulate its child
#' elements and content, directly access the element's textual content,
#' manipulate its attributes, and manage namespaces.

#' @export
Element <- setRefClass(
  Class ="Element",
  contains = "Content",
  fields = list(
    #' @field name The local name of the element
    name = "character",
    #' @field namespace The namespace of the element
    namespace = "Namespace",
    #' @field parent The document if this is the root element, otherwise the parent element
    parent = "NULL",
    #' @field attributeList a list of all the attributes belonging to this element
    attributeList = "list",
    #' @field contentList all the children of this element
    contentList = "list"
  ),
  methods = list(
    #' @param name The name of the tag (optional)
    #' @param namespace a namespace object setting the context for the the element (optional)
    #' @export
    initialize = function(...) {
      args <- list(...)
      argsNames <- names(args)   
      if ("name" %in% argsNames) {
        name <<- args$name
      }
      if ("namespace" %in% argsNames) {
        ns <- args$namespace
        # TODO should we alow NULL or NA here?
        if ("Namespace" %in% class(ns)) {
         namespace <<- ns 
        } else {
         stop(paste("Element constructor, namespace is not an instance of the Namespace class:", class(ns))) 
        }      
      }
      #print(paste("Element created, name is", private$name))
    },

    show = function() {
      cat(toString())
    },

    toString = function() {
      attrString <- ""
      if (length(attributeList) > 0) {
        for (i in 1:length(attributeList)) {
          attrString <- paste(attrString, attributeList[[i]]$toString())
        }
      } 
      startElement = "<"
      nsPrefix <- ""
      if (isRc(namespace) && namespace$getPrefix() != "") {
        nsPrefix <- paste0(namespace$getPrefix(), ":")
      }
      paste0(startElement, nsPrefix, name, attrString, ">", contentList,
      "</", nsPrefix, name, ">")
    },

    getName = function() {
      return(name)
    },

    getNamespace = function() {
      return(namespace) 
    },

    # Returns the namespace prefix of the element or an empty string if none exist
    getNamespacePrefix = function() {
      return(namespace$getPrefix())
    },
    
    getAttribute = function(attname, ns = Namespace$new()) {
      #print(paste("Getting attribute for", attname))
      return(attributeList[[attname]])
    },
    
    setAttribute = function(...) {
      args <- list(...)
      argsNames <- names(args)
      if (length(args) == 1) {
        if ("attribute" %in% argsNames) {
          attr <- args$attribute
        } else {
          stop("Element$setAttribute with 1 parameter but it was not 'attribute'")
        }
      } else if (length(args) == 2) {              
        if ("name" %in% argsNames) {
          attname <- args$name
        } else {
          stop("Element$setAttribute with 2 parameters but none of them was 'name'")
        }
        if ("value" %in% argsNames) {
          attval <- args$value
        } else {
          stop("Element$setAttribute with 2 parameters but none of them was 'value'")
        }       
        attr <- Attribute$new(name=attname, value=attval)        
      } else {
         stop("Element$setAttribute unknown number of parameters")
      }
      #print(paste("Adding attribute", attr, "to element", name))
      attributeList[[attr$getName()]] <<- attr
      #print("Attributes are now")
      #for (at in attributeList) {
      #  print(paste("name =", at$getName(), ", value =", at$getValue()))
      #}
      return(.self)
    },
    
    addContent = function(content) {
      idx <- length(contentList) + 1
      contentList[[idx]] <<- content
      return(.self)
    },
    
    getContent = function() {
      return(contentList)
    },
    
    setText = function(text) {
      if ("Text" == class(text)) {
        textObj <- text
      } else {
        textObj <- Text$new(as.character(text))
      }
      contentList <<- list()
      contentList[[text]] <<- textObj
      return(.self)
    },
    
    getText = function() {
      texts <- Filter(function(x) "Text" == class(x), contentList)
      if (length(texts) > 0) {
        return(texts[[1]]$toString())
      } else {
        return("")
      }
    },
    
    getChildren = function() {
      Filter(function(x) "Element" == class(x), contentList)
    },
    
    getChild = function(name) {
      for (content in contentList) {
        if ("Element" == class(content) & content$getName() == name) {
          #print(paste("Found child element", content))
          return (content)
        }
      }
    }
  )
  
)

#' as.vector for Element classes
#' @param x the object to convert
setMethod('as.vector', "Element",
  function(x) {
    x$toString()
  }
)

#' as.character for Element classes
#' @param x the object to convert
setMethod('as.character', "Element",
  function(x) {
    x$toString()
  }
)