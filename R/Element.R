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
    m_name = "character",
    #' @field namespace The namespace of the element
    m_namespace = "Namespace",
    #' @field attributeList a list of all the attributes belonging to this element
    attributeList = "list",
    #' @field contentList all the children of this element
    contentList = "list"
  ),
  methods = list(
    #' @param name The name of the tag (optional)
    #' @param namespace a namespace object setting the context for the the element (optional)
    initialize = function(name = NULL, namespace = NULL) {
      if(!is.null(name)) {
        m_name <<- name
      }
      if (!is.null(namespace)) {
        if (isRc(namespace, "Namespace")) {
          m_namespace <<- namespace
        } else {
         stop(paste("Element constructor, namespace is not an instance of the Namespace class:", class(namespace)))
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

      # 5 types
      # <parent xmlns="http://some.default"> <child /> </parent>
      # <parent xmlns="env:http://some.default"> <env:child /> </parent>
      # <parent> <child /> </parent>
      # <env:parent> <env:child /> </parent>
      # <env:parent> <child xmlns="foo:http://another"/> </parent>
      if (isRc(m_parent, "Element")) {
        if (m_namespace$equals(m_parent$getNamespace())) {
          if (m_namespace$getPrefix() != "") {
            # do we need to continue all the way to the top?
            nsPrefix <- m_namespace$getPrefix()
          }
        }
      } else {
        if (m_namespace$getPrefix() != "") {
          nsPrefix <- paste0(m_namespace$getPrefix(), ":")
        }
        attrString <- trim(paste(m_namespace, attrString))
      }
      ########
      #if (m_namespace$getPrefix() != "" & m_namespace$getUri != "") {
      #  prefix <- m_namespace$getPrefix()
      #  if (prefix != "") {
      #    prefix <- paste0(prefix, ":")
      #  }
      #  nsPrefix <- paste0("xmlns='", m_namespace$getPrefix(), ":")
      #} else if (m_namespace$getPrefix() != "") {
      #  nsPrefix <- paste0(m_namespace$getPrefix(), ":")
      #}
      #########

      if (length(attrString) > 0) {
        attrString <- paste0(" ", attrString)
      }

      start <- paste0(startElement, nsPrefix, m_name, attrString, ">")
      children <- ""
      for (child in contentList) {
        children <- paste0(children, child$toString())
      }
      end <- paste0("</", nsPrefix, m_name, ">")
      paste0(start, children, end)
    },

    getName = function() {
      return(m_name)
    },

    getNamespace = function() {
      return(m_namespace)
    },

    setNamespace = function(namespace) {
      if (isRc(namespace, "Namespace")) {
        m_namespace <<- namespace
      } else {
        stop(paste("Element$setNamespace(), namespace is not an instance of the Namespace class:", class(namespace)))
      }
      return(.self)
    },

    #' Returns the namespace prefix of the element or an empty string if none exist
    getNamespacePrefix = function() {
      return(m_namespace$getPrefix())
    },
    
    getAttribute = function(attname, ns = Namespace$new()) {
      #print(paste("Getting attribute for", attname))
      return(attributeList[[attname]])
    },
    
    setAttribute = function(name, value) {
      attr <- Attribute$new(name=name, value=value)
      setAttributeObj(attr)
    },

    # function overloading not supported with reference classes so change the method name
    setAttributeObj = function(attribute) {
      attributeList[[attribute$getName()]] <<- attribute
      return(.self)
    },
    
    addContent = function(content) {
      idx <- length(contentList) + 1
      content$setParent(.self)
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
      textObj$setParent(.self)
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
#' @describeIn Element as.vector(Element)
#' @param x the object to convert
setMethod('as.vector', "Element",
  function(x) {
    x$toString()
  }
)

#' as.character for Element classes
#' @describeIn Element as.character(Element)
#' @param x the object to convert
setMethod('as.character', "Element",
  function(x) {
    x$toString()
  }
)