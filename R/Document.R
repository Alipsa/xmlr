Document <- setRefClass(
  Class = "Document",
  contains = "Parent",
  fields = list(
    #* This document's content including comments, PIs, a possible DocType, and a root element.
    content = "list",
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
  