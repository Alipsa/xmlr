#' @title Element, A reference class representing an XML tag
#' @description
#' An XML element. Methods allow the user to get and manipulate its child
#' elements and content, directly access the element's textual content, and
#' manipulate its attributes.
#'
#' @export Element
#' @exportClass Element
Element <- setRefClass(
  Class ="Element",
  contains = "Content",
  fields = list(

    #' @field name The local name of the element
    m_name = "character",

    #' @field contentList all the children of this element
    contentList = "list",

    #' @field attributeList a list of all the attributes belonging to this element
    attributeList = "list"
  ),
  methods = list(
    initialize = function(name = NULL) {
      "Element constructor, @param name, The name of the tag (optional)"
      if(!is.null(name)) {
        m_name <<- name
      }
      #print(paste("Element created, name is", private$name))
    },

    addContent = function(content) {
      "Appends the child to the end of the content list. return the parent (the calling object)"
      idx <- length(contentList) + 1
      content$setParent(.self)
      contentList[[idx]] <<- content
      return(invisible(.self))
    },

    getContent = function() {
      "Returns the full content of the element as a List that may contain objects of type Text, Element, Comment, ProcessingInstruction, CDATA, and EntityRef"
      return(contentList)
    },

    getContentSize = function() {
      "Return the number of elements in the content list belonging to this this element"
      length(contentList)
    },

    findContentIndex = function(content) {
      "Find the position of the content in the contentList or -1 if not found"
      for (idx in seq_along(contentList)) {
        if(identical(content, contentList[[idx]])) {
          return(idx)
        }
      }
      -1
    },

    removeContent = function(content) {
      "Remove the specified content from this element"
      # faster than looping with findContentIndex,
      # sapply returns a vector with TRUE or FALSE of each object matching or not matching the content
      idx <- sapply(contentList, identical, content)
      if (all(!idx)) stop("There is no such content belonging to this Element")
      contentList <<- contentList[!idx]
    },

    removeContentAt = function(index) {
      "Remove the content at the given index and return the content that was removed"
      if (is.numeric(index)) {
        content <- contentList[[index]]
        contentList <<- contentList[-index]
        return(content)
      }
      return(NULL)
    },

    cloneContent = function() {
      printp("Element", "cloneContent()", "Not implemented, should return a list containing detached clones of this parent's content list")
    },

    indexOf = function(child) {
      printp("Element", "indexOf(child)", "Not implemented, should return the number of children in this parent's content list.")
    },

    getName = function() {
      "Return the name of this Element"
      return(m_name)
    },

    getAttribute = function(name) {
      "Get an Attribute object"
      #print(paste("Getting attribute for", name))
      return(attributeList[[name]])
    },

    setAttribute = function(name, value) {
      "Add or replace an attribute"
      attr <- Attribute$new(name=name, value=value)
      setAttributeObj(attr)
    },

    # function overloading not supported with reference classes so change the method name
    setAttributeObj = function(attribute) {
      "Add or replace an attribute"
      attributeList[[attribute$getName()]] <<- attribute
      return(invisible(.self))
    },
    
    setText = function(text) {
      "Replace all content with the text supplied"
      if ("Text" == class(text)) {
        textObj <- text
      } else {
        textObj <- Text$new(as.character(text))
      }
      textObj$setParent(.self)
      contentList <<- list()
      contentList[[text]] <<- textObj
      return(invisible(.self))
    },
    
    getText = function() {
      "Return the text content of this element if any"
      texts <- Filter(function(x) "Text" == class(x), contentList)
      if (length(texts) > 0) {
        return(texts[[1]]$toString())
      } else {
        return("")
      }
    },
    
    getChildren = function() {
      "Get all the child Elements belong to this Element"
      Filter(function(x) "Element" == class(x), contentList)
    },
    
    getChild = function(name) {
      "Retrun the first child element matching the name"
      for (content in contentList) {
        if ("Element" == class(content) & content$getName() == name) {
          #print(paste("Found child element", content))
          return (content)
        }
      }
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
      startElement <- "<"

      start <- paste0(startElement, m_name, attrString, ">")
      children <- ""
      for (child in contentList) {
        children <- paste0(children, child$toString())
      }
      end <- paste0("</", m_name, ">")
      paste0(start, children, end)
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