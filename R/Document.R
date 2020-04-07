#' Reference Class representing an XML document
#' @description
#' The base container for the DOM
#' @details
#' Methods allow access to the root element as well as the
#' DocType and other document-level information.

#' @export
Document <- setRefClass(
  Class = "Document",
  fields = list(
    # @field content This document's content including comments, PIs, a possible DocType, and a root element.
    content = "list",
    # @field baseURI From where the document was loaded, see https://www.w3.org/TR/2003/WD-DOM-Level-3-Core-20030226/core.html#baseURIs-Considerations
    m_baseURI = "character"
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

    getBaseURI = function() {
      "return the URI from which this document was loaded"
      return(m_baseURI)
    },

    setBaseURI = function(uri) {
      "Sets the effective URI from which this document was loaded"
      m_baseURI <<- uri
      return(.self)
    },

    getDocType = function() {
      printp("Document$getDocType() not implemented")
    },

    setDocType = function(docType) {
      printp("Document$setDocType(docType) not implemented")
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
  