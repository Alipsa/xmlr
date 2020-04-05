#' Reference class representing an XML tag
#' @details
#' An XML element. Methods allow the user to get and manipulate its child
#' elements and content, directly access the element's textual content, and
#' manipulate its attributes.

#' @export
Element <- setRefClass(
  Class ="Element",
  contains = "Content",
  fields = list(
    #' @field name The local name of the element
    m_name = "character",
    #' @field attributeList a list of all the attributes belonging to this element
    attributeList = "list",
    #' @field contentList all the children of this element
    contentList = "list"
  ),
  methods = list(
    # @param name The name of the tag (optional)
    initialize = function(name = NULL) {
      if(!is.null(name)) {
        m_name <<- name
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

      start <- paste0(startElement, m_name, attrString, ">")
      children <- ""
      for (child in contentList) {
        children <- paste0(children, child$toString())
      }
      end <- paste0("</", m_name, ">")
      paste0(start, children, end)
    },

    getName = function() {
      return(m_name)
    },
    
    getAttribute = function(attname) {
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