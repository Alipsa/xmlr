#' @title XMLReader, A reference class to parse XML data in to rcdom object model
#' @description
#' A parser utility to import XML data.
#' reader <- XMLReader$new()
#' reader$parseString("<foo></foo>")
#'
#' @export
XMLReader <- setRefClass(
  Class ="XMLReader",
  methods = list(

    parseString = function(xml) {
      "parse an xml string into a document"
      doc <- Document$new()
      for (char in strsplit(xml, "")[[1]]) {
        # do stuff
      }
      return(doc)
    }
  )
)
