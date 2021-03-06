#' Reference Class representing an XML document
#' @description
#' The base container for the DOM
#' @details
#' Methods allow access to the root element as well as the
#' DocType and other document-level information.

#' @export Document
#' @exportClass Document
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
      return(invisible(.self))
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
      return(invisible(.self))
    },

    getDocType = function() {
      warning("Document$getDocType() is not implemented")
    },

    setDocType = function(docType) {
      warning("Document$setDocType(docType) not implemented")
    },

    toString = function() {
      if ("root" %in% names(content) & !is.null(content$root)) {
        return(paste0(content$root$toString()))
      }
      message("Attempted toString on a rootless document; There is no root element for this document")
      ""
    },

    show = function() {
      cat(toString())
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
  