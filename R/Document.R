#' Reference Class representing an XML document
#' @description
#' The base container for the DOM
#' @details
#' Methods allow access to the root element as well as the
#' DocType and other document-level information.

#' @export
Document <- setRefClass(
  Class = "Document",
  contains = "Parent",
  fields = list(
    # @field content This document's content including comments, PIs, a possible DocType, and a root element.
    content = "list",
    # @field baseURI see https://www.w3.org/TR/2003/WD-DOM-Level-3-Core-20030226/core.html#baseURIs-Considerations
    baseURI = "character"
  ),
  methods = list(

    # @param element the root element (optional)
    initialize = function(element = NULL) {
      if (!is.null(element)) {
        content$root <<- element
      }
    },

    setRootElement = function(element) {
      content$root <<- element
      return(.self)
    },

    getRootElement = function() {
      return(content$root)
    },

    toString = function() {
      paste0(content$root$toString())
    }

  )
)

#' as.vector for Document classes
#' @describeIn Document as.vector(Document)
#' @param x the object to convert
setMethod('as.vector', "Document",
  function(x) {
    x$toString()
  }
)

#' as.character for Document classes
#' @describeIn Document as.character(Document)
#' @param x the object to convert
setMethod('as.character', "Document",
  function(x) {
    x$toString()
  }
)
  