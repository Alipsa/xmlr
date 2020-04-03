#' Reference Class representing an XML document
#' @description
#' The base container for the DOM
#' @details
#' Methods allow access to the root element as well as the
#' DocType and other document-level information.
Document <- setRefClass(
  Class = "Document",
  contains = "Parent",
  fields = list(
    #' @field This document's content including comments, PIs, a possible DocType, and a root element.
    content = "list",
    #' @field see https://www.w3.org/TR/2003/WD-DOM-Level-3-Core-20030226/core.html#baseURIs-Considerations
    baseURI = "character"
  ),
  methods = list(
    initialize = function(...) {
      print("Document created")
    },
    setRootElement = function(element) {
      content$root <<- element
      return(.self)
    },
    getRootElement = function() {
      return(content$root)
    }
  )
)  
  